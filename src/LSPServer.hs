module LSPServer
  ( ServerState (..),
    Server,
    handleMessage,
    handleError,
    handleOutbox,
    runServer,
    createServer,
    positionToUTF32,
    positionFromUTF32,
    ServerOptions (..),
  )
where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, get, put, runStateT)
import Data.Aeson (object, toJSON, (.=))
import Data.Aeson.Types (Value (Array, Null, Object, String), emptyArray)
import qualified Data.ByteString as BS
import Data.Maybe (catMaybes, fromMaybe, mapMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Vector as V
import Irk (findSymbolDefinition, searchPaths, symbolAtPosition)
import LSP
import Language (languageByPath)
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import Transport (Transport (..), setup)
import Types (IrkFile (..), IrkFilePos (..), file)
import Utils (ePutStrLn, extractLine, fileText, tryEncoding)

newtype ServerOptions = ServerOptions {sVerbose :: Bool}

data ServerState = ServerState
  { verbose :: Bool,
    workspaces :: [URI],
    positionEncoding :: PositionEncoding,
    initializeDone :: Bool,
    initializedDone :: Bool,
    shuttingDown :: Bool,
    outbox :: [Message],
    transport :: Transport
  }

type Server a = StateT ServerState IO a

vPutStrLn :: String -> Server ()
vPutStrLn text = do
  srv <- get
  when (verbose srv) $ lift (ePutStrLn text)

addOutbox :: Message -> Server ()
addOutbox msg = do
  srv <- get
  put $ srv {outbox = outbox srv ++ [msg]}

handleExit :: Server ()
handleExit = do
  srv <- get
  if shuttingDown srv
    then lift exitSuccess
    else lift $ exitWith (ExitFailure 1)

returnError :: MessageID -> ErrorCode -> Text -> Maybe Value -> Server ()
returnError rid code msg edata = do
  addOutbox $ responseErr rid code msg edata

returnResponse :: MessageID -> Value -> Server ()
returnResponse rid val = do
  addOutbox $ response rid val

handleShutdown :: MessageID -> Server ()
handleShutdown rid = do
  srv <- get
  put $ srv {shuttingDown = True}
  returnResponse rid Null

positionToUTF32 :: Position -> PositionEncoding -> Text -> Position
positionToUTF32 pos@(Position {pLine = line, pCharacter = col}) encoding source =
  case params encoding of
    Nothing -> pos
    Just (encode, decode, multiplier) ->
      let mline = extractLine source line 0
       in case mline of
            Just (_, contents) ->
              let bytes = BS.take (col * multiplier) $ encode contents
                  newcol = T.length (decode lenientDecode bytes)
               in Position {pLine = line, pCharacter = newcol}
            Nothing -> pos
  where
    params UTF8 = Just (TE.encodeUtf8, TE.decodeUtf8With, 1)
    params UTF16 = Just (TE.encodeUtf16LE, TE.decodeUtf16LEWith, 2)
    params UTF32 = Nothing

positionFromUTF32 :: Position -> PositionEncoding -> Text -> Position
positionFromUTF32 pos@(Position {pLine = line, pCharacter = col}) encoding source =
  case params encoding of
    Nothing -> pos
    Just (encode, multiplier) ->
      let mline = extractLine source line 0
       in case mline of
            Just (_, contents) ->
              let bytes = encode (T.take col contents)
                  newcol = BS.length bytes `div` multiplier
               in Position {pLine = line, pCharacter = newcol}
            Nothing -> pos
  where
    params UTF8 = Just (TE.encodeUtf8, 1)
    params UTF16 = Just (TE.encodeUtf16LE, 2)
    params UTF32 = Nothing

locationLinkFromIrkFilePos :: PositionEncoding -> IrkFilePos -> IO LocationLink
locationLinkFromIrkFilePos encoding (IrkFilePos f line col) = do
  uri <- uriFromPath (iPath f)
  let utf32Pos = Position {pLine = line, pCharacter = col}
  pos <- case encoding of
    UTF32 -> return utf32Pos
    _ -> do
      msource <- fileText f
      return $ positionFromUTF32 utf32Pos encoding $ fromMaybe "" msource
  let range = Range {rStart = pos, rEnd = pos}
  return Location {lTargetUri = uri, lTargetRange = range, lTargetSelectionRange = range}

handleTextDocDefinition :: MessageID -> Maybe Value -> Server ()
handleTextDocDefinition rid mparams = do
  case mparams of
    Just params@(Object _) -> do
      let textDoc = jsonGetOr params "textDocument" $ object []
      let muri = jsonGet textDoc "uri" :: Maybe URI
      muriPath <- case muri of
        Just uri -> lift $ tryEncoding $ pathFromURI uri
        Nothing -> pure Nothing
      let mpos = jsonGet params "position" :: Maybe Position
      let mlang = muriPath >>= languageByPath

      msource <- case muriPath of
        Just uriPath -> lift $ fileText $ file {iPath = uriPath}
        Nothing -> pure Nothing

      srv <- get

      msymbol <- case (msource, mlang, mpos) of
        (Just source, Just lang, Just pos) -> do
          -- The LSP position we received will be using a specific position
          -- encoding. We first need to convert this position from its encoding
          -- to UTF32, in order for it to work with the main irk functions.
          let converted = positionToUTF32 pos (positionEncoding srv) source
          let irkPos = IrkFilePos file (pLine converted) (pCharacter converted)
          pure $ symbolAtPosition lang source irkPos
        _ -> pure Nothing

      case (muriPath, mlang, msource, msymbol) of
        (Just uriPath, Just lang, Just _, Just symbol) -> do
          let workspaceURIs = workspaces srv
          mworkspaces <- mapM (lift . tryEncoding . pathFromURI) workspaceURIs

          let searches = searchPaths lang (Just uriPath) (catMaybes mworkspaces)
          positions <- lift $ findSymbolDefinition lang symbol searches

          -- Convert positions back to the client's preferred encoding, if necessary.
          mlocations <- mapM (lift . tryEncoding . locationLinkFromIrkFilePos (positionEncoding srv)) positions
          let locations = catMaybes mlocations

          returnResponse rid $ Array (V.fromList $ map toJSON locations)
        (Nothing, _, _, _) -> returnError rid InvalidRequest "missing document uri, or uri encoding failed" Nothing
        (_, Nothing, _, _) -> returnError rid InvalidRequest "unsupported language" Nothing
        (_, _, Nothing, _) -> returnError rid InvalidRequest "unable to read document, or invalid position" Nothing
        (_, _, _, Nothing) -> returnResponse rid emptyArray
    _ -> returnError rid InvalidRequest "invalid params for textDocument/definition" Nothing

handleInitialize :: MessageID -> Maybe Value -> Server ()
handleInitialize rid mparams = do
  case mparams of
    Just params@(Object _) -> do
      let rootURI = jsonGet params "rootUri" <|> jsonGet params "rootPath" :: Maybe URI
      let workspacesFolders =
            mapMaybe
              (\v -> jsonGet v "uri" :: Maybe URI)
              (jsonGetOr params "workspaceFolders" [] :: [Value])
      let workspacesList =
            if not (null workspacesFolders)
              then workspacesFolders
              else maybeToList rootURI

      let clientCapabilities = jsonGetOr params "capabilities" $ object []
      let generalCapabilities = jsonGetOr clientCapabilities "general" $ object []
      let posEncoding = case jsonGetOr generalCapabilities "positionEncodings" [UTF16] of
            [] -> UTF16
            (enc : _) -> enc

      if null workspacesList
        then
          returnError rid InvalidRequest "no workspace root received" Nothing
        else do
          srv <- get
          put $
            srv
              { workspaces = workspacesList,
                positionEncoding = posEncoding,
                initializeDone = True
              }
          returnResponse
            rid
            ( object
                [ "capabilities"
                    .= object
                      [ "definitionProvider" .= True,
                        "positionEncoding" .= posEncoding,
                        "textDocumentSync"
                          .= object
                            [ "openClose" .= False,
                              "change" .= (0 :: Int)
                            ]
                      ],
                  "serverInfo" .= object ["name" .= String "irk"]
                ]
            )
    _ -> returnError rid InvalidRequest "invalid params for initialize" Nothing

handleInitialized :: Server ()
handleInitialized = do
  srv <- get
  when (initializeDone srv) $ do
    put $ srv {initializedDone = True}

handleRequestFallback :: MessageID -> Server ()
handleRequestFallback rid = do
  srv <- get
  if initializedDone srv
    then do
      vPutStrLn "info: ignoring request"
    else returnError rid ServerNotInitialized "server not yet initialized" Nothing

handleNotificationFallback :: Server ()
handleNotificationFallback = do
  vPutStrLn "info: ignoring notification"

handleResponseFallback :: Server ()
handleResponseFallback = do
  vPutStrLn "info: ignoring response"

handleMessage :: Message -> Server ()
handleMessage msg = do
  srv <- get
  case (srv, msg) of
    (ServerState {initializedDone = True}, MRequest Request {rId = rid, rMethod = Shutdown}) ->
      handleShutdown rid
    (ServerState {initializedDone = True}, MRequest Request {rId = rid, rMethod = TextDocumentDefinition, rParams = mparams}) ->
      handleTextDocDefinition rid mparams
    (_, MRequest Request {rId = rid, rMethod = Initialize, rParams = mparams}) -> handleInitialize rid mparams
    (_, MRequest Request {rId = rid}) -> handleRequestFallback rid
    (_, MNotification Notification {nMethod = Exit}) -> handleExit
    (_, MNotification Notification {nMethod = Initialized}) -> handleInitialized
    (_, MNotification Notification {}) -> handleNotificationFallback
    (_, MResponse Response {}) -> handleResponseFallback

handleError :: LSPError -> Server ()
handleError err = case err of
  ProtocolError description -> do
    lift $ ePutStrLn $ "error: " ++ description

handleOutbox :: Server ()
handleOutbox = do
  srv <- get
  lift $ mapM_ (writeMessage $ transport srv) (outbox srv)
  put $ srv {outbox = []}

createServer :: Bool -> ServerState
createServer verb =
  ServerState
    { verbose = verb,
      workspaces = [],
      positionEncoding = UTF16,
      initializeDone = False,
      initializedDone = False,
      shuttingDown = False,
      outbox = [],
      transport = Stdio
    }

runServer :: ServerOptions -> IO ()
runServer options = do
  let srv = createServer (sVerbose options)
  setup (transport srv)
  loop srv
  where
    loop s = do
      msg <- readMessage (transport s)
      let action = either handleError handleMessage msg
      (_, s') <- runStateT (action >> handleOutbox) s
      loop s'
