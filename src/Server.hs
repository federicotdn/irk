module Server
  ( Server (..),
    handleMessage,
    handleError,
    handleOutbox,
    runServer,
    createServer,
    ServerOptions (..),
  )
where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Aeson (object, toJSON, (.=))
import Data.Aeson.Types (Value (Array, Null, Object, String), emptyArray)
import Data.Maybe (catMaybes, mapMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Vector as V
import Irk (findSymbolDefinition, searchPaths, symbolAtPosition)
import LSP
import Language (languageByPath)
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import Types (IrkFile (..), IrkFilePos (..), file)
import Utils (ePutStrLn, fileText, tryEncoding)

newtype ServerOptions = ServerOptions {sVerbose :: Bool}

data Server = Server
  { verbose :: Bool,
    workspaces :: [URI],
    positionEncoding :: PositionEncoding,
    initializeDone :: Bool,
    initializedDone :: Bool,
    shuttingDown :: Bool,
    outbox :: [Message]
  }
  deriving (Show)

vPutStrLn :: Server -> String -> IO ()
vPutStrLn srv text = when (verbose srv) $ ePutStrLn text

addOutbox :: Server -> Message -> Server
addOutbox srv msg = srv {outbox = outbox srv ++ [msg]}

handleExit :: Server -> IO Server
handleExit srv =
  if shuttingDown srv
    then exitSuccess
    else exitWith (ExitFailure 1)

returnError :: Server -> MessageID -> ErrorCode -> Text -> Maybe Value -> IO Server
returnError srv rid code msg edata = return $ addOutbox srv $ responseErr rid code msg edata

returnResponse :: Server -> MessageID -> Value -> IO Server
returnResponse srv rid val = return $ addOutbox srv $ response rid val

handleShutdown :: Server -> MessageID -> IO Server
handleShutdown srv rid = returnResponse srv {shuttingDown = True} rid Null

handleTextDocDefinition :: Server -> MessageID -> Maybe Value -> IO Server
handleTextDocDefinition srv rid mparams = do
  case mparams of
    Just params@(Object _) -> do
      let textDoc = jsonGetOr params "textDocument" $ object []
      let muri = jsonGet textDoc "uri" :: Maybe URI
      muriPath <- case muri of
        Just uri -> tryEncoding $ pathFromURI uri
        Nothing -> pure Nothing
      let mpos = (\(Position l c) -> IrkFilePos file l c) <$> jsonGet params "position"
      let mlang = muriPath >>= languageByPath

      msource <- case muriPath of
        Just uriPath -> fileText $ file {iPath = uriPath}
        Nothing -> pure Nothing

      msymbol <- case (msource, mlang, mpos) of
        (Just source, Just lang, Just pos) -> pure $ symbolAtPosition lang source pos
        _ -> pure Nothing

      case (muriPath, mlang, msource, msymbol) of
        (Just uriPath, Just lang, Just _, Just symbol) -> do
          let workspaceURIs = workspaces srv
          mworkspaces <- mapM (tryEncoding . pathFromURI) workspaceURIs

          let searches = searchPaths lang (Just uriPath) (catMaybes mworkspaces)
          positions <- findSymbolDefinition lang symbol searches

          mlocations <- mapM (tryEncoding . locationFromFilePos) positions
          let locations = catMaybes mlocations

          returnResponse srv rid $ Array (V.fromList $ map toJSON locations)
        (Nothing, _, _, _) -> returnError srv rid InvalidRequest "missing document uri, or uri encoding failed" Nothing
        (_, Nothing, _, _) -> returnError srv rid InvalidRequest "unsupported language" Nothing
        (_, _, Nothing, _) -> returnError srv rid InvalidRequest "unable to read document, or invalid position" Nothing
        (_, _, _, Nothing) -> returnResponse srv rid emptyArray
    _ -> returnError srv rid InvalidRequest "invalid params for textDocument/definition" Nothing

handleInitialize :: Server -> MessageID -> Maybe Value -> IO Server
handleInitialize srv rid mparams = do
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
      let positionEncodings = case jsonGetOr generalCapabilities "positionEncodings" [UTF16] of
            [] -> [UTF16]
            xs -> xs

      if null workspacesList
        then
          returnError srv rid InvalidRequest "no workspace root received" Nothing
        else
          returnResponse
            ( srv
                { workspaces = workspacesList,
                  positionEncoding = head positionEncodings,
                  initializeDone = True
                }
            )
            rid
            ( object
                [ "capabilities"
                    .= object
                      [ "definitionProvider" .= True,
                        "positionEncoding" .= head positionEncodings,
                        "textDocumentSync"
                          .= object
                            [ "openClose" .= False,
                              "change" .= (0 :: Int)
                            ]
                      ],
                  "serverInfo" .= object ["name" .= String "irk"]
                ]
            )
    _ -> returnError srv rid InvalidRequest "invalid params for initialize" Nothing

handleInitialized :: Server -> IO Server
handleInitialized srv = do
  if initializeDone srv
    then return $ srv {initializedDone = True}
    else return srv -- ignore

handleRequestFallback :: Server -> MessageID -> IO Server
handleRequestFallback srv rid =
  if initializedDone srv
    then do
      vPutStrLn srv "info: ignoring request"
      return srv
    else returnError srv rid ServerNotInitialized "server not yet initialized" Nothing

handleNotificationFallback :: Server -> IO Server
handleNotificationFallback srv = do
  vPutStrLn srv "info: ignoring notification"
  return srv

handleResponseFallback :: Server -> IO Server
handleResponseFallback srv = do
  vPutStrLn srv "info: ignoring response"
  return srv

handleMessage :: Server -> Message -> IO Server
handleMessage srv msg = do
  case (srv, msg) of
    (Server {initializedDone = True}, MRequest Request {rId = rid, rMethod = Shutdown}) ->
      handleShutdown srv rid
    (Server {initializedDone = True}, MRequest Request {rId = rid, rMethod = TextDocumentDefinition, rParams = mparams}) ->
      handleTextDocDefinition srv rid mparams
    (_, MRequest Request {rId = rid, rMethod = Initialize, rParams = mparams}) -> handleInitialize srv rid mparams
    (_, MRequest Request {rId = rid}) -> handleRequestFallback srv rid
    (_, MNotification Notification {nMethod = Exit}) -> handleExit srv
    (_, MNotification Notification {nMethod = Initialized}) -> handleInitialized srv
    (_, MNotification Notification {}) -> handleNotificationFallback srv
    (_, MResponse Response {}) -> handleResponseFallback srv

handleError :: Server -> LSPError -> IO Server
handleError srv err = case err of
  ProtocolError description -> do
    ePutStrLn $ "error: " ++ description
    return srv

handleOutbox :: Server -> IO Server
handleOutbox srv = do
  mapM_ writeMessage $ outbox srv
  return srv {outbox = []}

createServer :: Bool -> Server
createServer verb =
  Server
    { verbose = verb,
      workspaces = [],
      positionEncoding = UTF16,
      initializeDone = False,
      initializedDone = False,
      shuttingDown = False,
      outbox = []
    }

runServer :: ServerOptions -> IO ()
runServer options = do
  let srv = createServer (sVerbose options)
  vPutStrLn srv "info: starting lsp server"
  loop srv
  where
    loop s =
      readMessage
        >>= either (handleError s) (handleMessage s)
        >>= handleOutbox
        >>= loop
