module LSP
  ( Position (..),
    Location (..),
    Range (..),
    MessageID (..),
    Method (..),
    Request (..),
    Response (..),
    ResponseError (..),
    Notification (..),
    Message (..),
    LSPError (..),
    ErrorCode (..),
    URI (..),
    PositionEncoding (..),
    pathFromURI,
    uriFromPath,
    readMessage,
    writeMessage,
    response,
    responseErr,
    jsonGet,
    jsonGetOr,
  )
where

import Control.Applicative ((<|>))
import Control.Monad.Catch (MonadThrow)
import Data.Aeson
  ( FromJSON (parseJSON),
    Options (..),
    Result (..),
    ToJSON (toEncoding, toJSON),
    Value (..),
    decode,
    defaultOptions,
    encode,
    fromJSON,
    genericParseJSON,
    genericToEncoding,
    genericToJSON,
  )
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BSL
import Data.Char (chr, digitToInt, isHexDigit, isSpace, toLower)
import Data.List (dropWhileEnd, find, stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics
import System.OsPath (OsPath, normalise, pathSeparator)
import qualified System.OsString as OS
import Text.Read (readMaybe)
import Transport (Transport, readLine, readNBytes, writeBytes, writeString)
import Utils (hasWindowsDrive, os, osc)

data Header = Header String String deriving (Show)

headerContentLength :: String
headerContentLength = "Content-Length"

headerContentType :: String
headerContentType = "Content-Type"

validContentTypes :: [String]
validContentTypes = ["utf-8", "utf8"]

urlEncode :: String -> String
urlEncode "" = ""
urlEncode (':' : rest) = "%3A" ++ urlEncode rest
urlEncode (' ' : rest) = "%20" ++ urlEncode rest
urlEncode (c : rest) = c : urlEncode rest

urlDecode :: String -> String
urlDecode "" = ""
urlDecode ('%' : h1 : h2 : rest)
  | isHexDigit h1 && isHexDigit h2 =
      chr (digitToInt h1 * 16 + digitToInt h2) : urlDecode rest
urlDecode (c : rest) = c : urlDecode rest

newtype URI = FileURI String deriving (Show, Eq)

instance FromJSON URI where
  parseJSON v = do
    s <- parseJSON v
    case stripPrefix "file://" s of
      Just stripped -> return $ FileURI (urlDecode stripped)
      Nothing -> fail "malformed URI"

instance ToJSON URI where
  toJSON (FileURI s) = toJSON $ "file://" ++ urlEncode s
  toEncoding = toEncoding . toJSON

isWindowsAbsURI :: (MonadThrow m) => URI -> m Bool
isWindowsAbsURI (FileURI s) = do
  path <- OS.encodeUtf s
  return $ not (OS.null path) && OS.head path == osc '/' && hasWindowsDrive (OS.tail path)

pathFromURI :: (MonadThrow m) => URI -> m OsPath
pathFromURI uri@(FileURI s) = do
  windowsAbsURI <- isWindowsAbsURI uri
  let path = if windowsAbsURI then fromMaybe s (stripPrefix "/" s) else s
  encoded <- OS.encodeUtf path
  return $ normalise encoded

uriFromPath :: (MonadThrow m) => OsPath -> m URI
uriFromPath path
  | hasWindowsDrive path = FileURI <$> OS.decodeUtf (os "/" <> uriPath)
  | otherwise = FileURI <$> OS.decodeUtf uriPath
  where
    uriPath = OS.map (\c -> if c == pathSeparator then osc '/' else c) path

data Position = Position {pLine :: Int, pCharacter :: Int} deriving (Show, Eq, Generic)

instance FromJSON Position where
  parseJSON = genericParseJSON customOptions

instance ToJSON Position where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

data Range = Range {rStart :: Position, rEnd :: Position} deriving (Show, Generic)

instance FromJSON Range where
  parseJSON = genericParseJSON customOptions

instance ToJSON Range where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

data Location = Location {lUri :: URI, lRange :: Range} deriving (Show, Generic)

instance FromJSON Location where
  parseJSON = genericParseJSON customOptions

instance ToJSON Location where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

newtype LSPError = ProtocolError String

data PositionEncoding
  = UTF8
  | UTF16
  | UTF32
  deriving (Show, Eq)

instance FromJSON PositionEncoding where
  parseJSON v = do
    s <- parseJSON v
    return $ case s :: String of
      "utf-8" -> UTF8
      "utf-16" -> UTF16
      "utf-32" -> UTF32
      -- Tolerate errors; LSP defaults to UTF-16
      _ -> UTF16

instance ToJSON PositionEncoding where
  toJSON UTF8 = toJSON ("utf-8" :: String)
  toJSON UTF16 = toJSON ("utf-16" :: String)
  toJSON UTF32 = toJSON ("utf-32" :: String)
  toEncoding = toEncoding . toJSON

data Method
  = Initialize
  | Initialized
  | Shutdown
  | Exit
  | TextDocumentDefinition
  | UnknownMethod String
  deriving (Show, Eq)

instance FromJSON Method where
  parseJSON v = do
    s <- parseJSON v
    return $ case s :: String of
      "initialize" -> Initialize
      "initialized" -> Initialized
      "shutdown" -> Shutdown
      "exit" -> Exit
      "textDocument/definition" -> TextDocumentDefinition
      _ -> UnknownMethod s

instance ToJSON Method where
  toJSON m = toJSON $ methodStr m
  toEncoding = toEncoding . toJSON

methodStr :: Method -> String
methodStr m = case m of
  Initialize -> "initialize"
  Initialized -> "initialized"
  Shutdown -> "shutdown"
  Exit -> "exit"
  TextDocumentDefinition -> "textDocument/definition"
  (UnknownMethod s) -> s

data ErrorCode = ServerNotInitialized | InvalidRequest | ParseError | ServerError Int deriving (Show, Eq)

instance FromJSON ErrorCode where
  parseJSON v = do
    n <- parseJSON v
    return $ case n :: Int of
      -32002 -> ServerNotInitialized
      -32600 -> InvalidRequest
      -32700 -> ParseError
      _ -> ServerError n

instance ToJSON ErrorCode where
  toJSON ServerNotInitialized = toJSON (-32002 :: Int)
  toJSON InvalidRequest = toJSON (-32600 :: Int)
  toJSON ParseError = toJSON (-32700 :: Int)
  toJSON (ServerError n) = toJSON n
  toEncoding = toEncoding . toJSON

data MessageID = IDString Text | IDInt Int deriving (Show)

instance FromJSON MessageID where
  parseJSON v =
    (IDInt <$> parseJSON v)
      <|> (IDString <$> parseJSON v)

instance ToJSON MessageID where
  toJSON (IDInt i) = toJSON i
  toJSON (IDString s) = toJSON s
  toEncoding (IDInt i) = toEncoding i
  toEncoding (IDString s) = toEncoding s

data Request = Request
  { rJsonrpc :: Text,
    rId :: MessageID,
    rMethod :: Method,
    rParams :: Maybe Value
  }
  deriving (Show, Generic)

instance FromJSON Request where
  parseJSON = genericParseJSON customOptions

instance ToJSON Request where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

data Response = Response
  { pJsonrpc :: Text,
    pId :: Maybe MessageID,
    pResult :: Maybe Value,
    pError :: Maybe ResponseError
  }
  deriving (Show, Generic)

instance FromJSON Response where
  parseJSON = genericParseJSON customOptions

instance ToJSON Response where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

data ResponseError = ResponseError
  { eCode :: ErrorCode,
    eMessage :: Text,
    eData :: Maybe Value
  }
  deriving (Show, Generic)

instance FromJSON ResponseError where
  parseJSON = genericParseJSON customOptions

instance ToJSON ResponseError where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

data Notification = Notification
  { nJsonrpc :: Text,
    nMethod :: Method,
    nParams :: Maybe Value
  }
  deriving (Show, Generic)

instance FromJSON Notification where
  parseJSON = genericParseJSON customOptions

instance ToJSON Notification where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

data Message
  = MRequest Request
  | MResponse Response
  | MNotification Notification
  deriving (Show)

instance FromJSON Message where
  parseJSON v =
    (MRequest <$> parseJSON v)
      <|> (MNotification <$> parseJSON v)
      <|> (MResponse <$> parseJSON v)

instance ToJSON Message where
  toJSON (MRequest r) = toJSON r
  toJSON (MResponse r) = toJSON r
  toJSON (MNotification n) = toJSON n
  toEncoding (MRequest r) = toEncoding r
  toEncoding (MResponse r) = toEncoding r
  toEncoding (MNotification n) = toEncoding n

dropPrefixAndLower :: String -> String
dropPrefixAndLower (_ : rest) = map toLower rest
dropPrefixAndLower [] = ""

customOptions :: Options
customOptions =
  defaultOptions
    { fieldLabelModifier = dropPrefixAndLower,
      omitNothingFields = True,
      rejectUnknownFields = True
    }

responseErr :: MessageID -> ErrorCode -> Text -> Maybe Value -> Message
responseErr rid code message edata =
  MResponse
    Response
      { pJsonrpc = "2.0",
        pId = Just rid,
        pResult = Nothing,
        pError =
          Just
            ResponseError
              { eCode = code,
                eMessage = message,
                eData = edata
              }
      }

response :: MessageID -> Value -> Message
response rid val =
  MResponse
    Response
      { pJsonrpc = "2.0",
        pId = Just rid,
        pResult = Just val,
        pError = Nothing
      }

cut :: String -> Char -> (String, String)
cut s c = case parts of
  (_, "") -> parts
  (left, _ : right) -> (left, right)
  where
    parts = break (== c) s

trim :: String -> String
trim xs = dropWhile isSpace (dropWhileEnd isSpace xs)

parseHeaders :: Transport -> IO [Header]
parseHeaders tr = loop []
  where
    loop :: [Header] -> IO [Header]
    loop headers = do
      line <- readLine tr
      let cleanLine = filter (/= '\r') line
      if null cleanLine
        then return headers
        else
          let parts = cut cleanLine ':'
           in loop $ headers ++ [Header (trim (fst parts)) (trim (snd parts))]

getHeader :: [Header] -> String -> Maybe Header
getHeader hs headerName = find matches hs
  where
    matches (Header name _) = map toLower name == map toLower headerName

getContentLength :: [Header] -> Maybe Int
getContentLength hs = case getHeader hs headerContentLength of
  Nothing -> Nothing
  Just (Header _ value) -> readMaybe value

getContentType :: [Header] -> String
getContentType hs = case getHeader hs headerContentType of
  Nothing -> head validContentTypes
  Just (Header _ value) -> value

validateMessage :: Message -> Either LSPError Message
validateMessage msg = case msg of
  MRequest Request {rJsonrpc = "2.0"} -> Right msg
  MResponse Response {pJsonrpc = "2.0"} -> Right msg
  MNotification Notification {nJsonrpc = "2.0"} -> Right msg
  _ -> Left (ProtocolError "invalid jsonrpc field value")

readMessage :: Transport -> IO (Either LSPError Message)
readMessage tr = do
  headers <- parseHeaders tr
  let contentType = getContentType headers
  if contentType `notElem` validContentTypes
    then
      return $ Left (ProtocolError $ "invalid content type: " ++ contentType)
    else case getContentLength headers of
      Nothing -> return $ Left (ProtocolError "no valid content length header")
      Just l -> do
        body <- readNBytes tr l
        if BSL.length body == fromIntegral l
          then case decode body of
            Just msg -> return $ validateMessage msg
            Nothing -> return $ Left (ProtocolError "failed to decode json")
          else return $ Left (ProtocolError "invalid body length")

writeMessage :: Transport -> Message -> IO ()
writeMessage tr msg = do
  let encoded = encode msg
  let len = BSL.length encoded
  writeString tr $ headerContentLength ++ ": " ++ show len ++ "\r\n\r\n"
  writeBytes tr encoded

jsonGet :: (FromJSON a) => Value -> String -> Maybe a
jsonGet val key = case val of
  Object obj -> case KM.lookup (K.fromString key) obj of
    Nothing -> Nothing
    Just ival -> case fromJSON ival of
      Success s -> Just s
      Error _ -> Nothing
  _ -> Nothing

jsonGetOr :: (FromJSON a) => Value -> String -> a -> a
jsonGetOr val key dflt = fromMaybe dflt $ jsonGet val key
