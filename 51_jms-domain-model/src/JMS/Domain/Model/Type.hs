{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module JMS.Domain.Model.Type where

import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import Control.Monad.Logger
import qualified Data.Text as T
import qualified Text.Read as R
import Control.Concurrent.STM.TQueue
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Default
import System.Exit

import JMS.Domain.Model.TH.Utility

--------------------------------------------------------------------------------
-- |
--
instance FromJSON LogLevel where
  parseJSON (String v) = case R.readEither ("Level" ++ T.unpack v) of
    Right l -> pure l
    Left er -> error $ "invalid loglevel. <" ++ T.unpack v ++ "> " ++ er
  parseJSON o = error $ "json parse error. Priority:" ++ show o

instance ToJSON LogLevel where
  toJSON  LevelDebug    = String $ T.pack "Debug"
  toJSON  LevelInfo     = String $ T.pack "Info"
  toJSON  LevelWarn     = String $ T.pack "Warn"
  toJSON  LevelError    = String $ T.pack "Error"
  toJSON (LevelOther m) = String m

--------------------------------------------------------------------------------
-- |
--
newtype RawJsonString = RawJsonString { unRawJsonString :: String }
  deriving (Show, Read, Eq)

instance FromJSON RawJsonString where
  parseJSON = pure . RawJsonString . BL.unpack . encode

instance ToJSON RawJsonString where
  toJSON (RawJsonString str) =
    case eitherDecode (BL.pack str) of
      Right v -> v
      Left  e -> error $ show e

instance Default RawJsonString where
  def = RawJsonString ""

-- |
--
newtype RawJsonByteString = RawJsonByteString { unRawJsonByteString :: BL.ByteString }
  deriving (Show, Read, Eq)

instance FromJSON RawJsonByteString where
  parseJSON = pure . RawJsonByteString . encode

instance ToJSON RawJsonByteString where
  toJSON (RawJsonByteString bs) =
    case eitherDecode bs of
      Right v -> v
      Left  e -> error $ show e

instance Default RawJsonByteString where
  def = RawJsonByteString ""

--------------------------------------------------------------------------------
-- |
--
data JsonRpcRequest =
  JsonRpcRequest {
    _jsonrpcJsonRpcRequest  :: String
  , _idJsonRpcRequest       :: Maybe Int
  , _methodJsonRpcRequest   :: String
  , _paramsJsonRpcRequest   :: Maybe RawJsonByteString
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "JsonRpcRequest", omitNothingFields = True} ''JsonRpcRequest)
makeLenses ''JsonRpcRequest

instance Default JsonRpcRequest where
  def = JsonRpcRequest {
        _jsonrpcJsonRpcRequest  = def
      , _idJsonRpcRequest  = def
      , _methodJsonRpcRequest = def
      , _paramsJsonRpcRequest = def
      }

-- |
--
data JsonRpcResponse =
  JsonRpcResponse {
    _jsonrpcJsonRpcResponse :: String
  , _idJsonRpcResponse      :: Int
  , _resultJsonRpcResponse  :: RawJsonByteString
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "JsonRpcResponse", omitNothingFields = True} ''JsonRpcResponse)
makeLenses ''JsonRpcResponse

instance Default JsonRpcResponse where
  def = JsonRpcResponse {
        _jsonrpcJsonRpcResponse = ""
      , _idJsonRpcResponse = 0
      , _resultJsonRpcResponse = RawJsonByteString ""
      }

defaultJsonRpcResponse :: JsonRpcRequest -> JsonRpcResponse
defaultJsonRpcResponse req = def {
    _jsonrpcJsonRpcResponse = req^.jsonrpcJsonRpcRequest
  , _idJsonRpcResponse = maybe 0 id (req^.idJsonRpcRequest)
  , _resultJsonRpcResponse = def
  }

--------------------------------------------------------------------------------

-- |
--
data McpInitializeRequestParams =
  McpInitializeRequestParams {
    _protocolVersionMcpInitializeRequestParams :: String
  , _capabilitiesMcpInitializeRequestParams    :: RawJsonByteString
  , _clientInfoMcpInitializeRequestParams      :: RawJsonByteString
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpInitializeRequestParams", omitNothingFields = True} ''McpInitializeRequestParams)
makeLenses ''McpInitializeRequestParams

instance Default McpInitializeRequestParams where
  def = McpInitializeRequestParams {
        _protocolVersionMcpInitializeRequestParams = def
      , _capabilitiesMcpInitializeRequestParams = def
      , _clientInfoMcpInitializeRequestParams = def
      }

-- |
--
data McpInitializeRequestData =
  McpInitializeRequestData {
    _jsonrpcMcpInitializeRequestData :: JsonRpcRequest
  , _paramsMcpInitializeRequestData  :: McpInitializeRequestParams
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpInitializeRequestData", omitNothingFields = True} ''McpInitializeRequestData)
makeLenses ''McpInitializeRequestData

instance Default McpInitializeRequestData where
  def = McpInitializeRequestData {
        _jsonrpcMcpInitializeRequestData = def
      , _paramsMcpInitializeRequestData = def
      }


-- |
--
data McpInitializedNotificationData =
  McpInitializedNotificationData {
    _jsonrpcMcpInitializedNotificationData :: JsonRpcRequest
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpInitializedNotificationData", omitNothingFields = True} ''McpInitializedNotificationData)
makeLenses ''McpInitializedNotificationData

instance Default McpInitializedNotificationData where
  def = McpInitializedNotificationData {
        _jsonrpcMcpInitializedNotificationData = def
      }

-- |
--
data McpToolsListRequestData =
  McpToolsListRequestData {
    _jsonrpcMcpToolsListRequestData :: JsonRpcRequest
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpToolsListRequestData", omitNothingFields = True} ''McpToolsListRequestData)
makeLenses ''McpToolsListRequestData

instance Default McpToolsListRequestData where
  def = McpToolsListRequestData {
        _jsonrpcMcpToolsListRequestData = def
      }


-- |
--
data McpToolsCallRequestDataParamsSystemArgs =
  McpToolsCallRequestDataParamsSystemArgs {
    _argsMcpToolsCallRequestDataParamsSystemArgs :: String
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpToolsCallRequestDataParamsSystemArgs", omitNothingFields = True} ''McpToolsCallRequestDataParamsSystemArgs)
makeLenses ''McpToolsCallRequestDataParamsSystemArgs

instance Default McpToolsCallRequestDataParamsSystemArgs where
  def = McpToolsCallRequestDataParamsSystemArgs {
        _argsMcpToolsCallRequestDataParamsSystemArgs = def
      }

-- |
--
data McpToolsCallRequestDataParams =
  McpToolsCallRequestDataParams {
    _nameMcpToolsCallRequestDataParams :: String
  , _argumentsMcpToolsCallRequestDataParams  :: RawJsonByteString
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpToolsCallRequestDataParams", omitNothingFields = True} ''McpToolsCallRequestDataParams)
makeLenses ''McpToolsCallRequestDataParams

instance Default McpToolsCallRequestDataParams where
  def = McpToolsCallRequestDataParams {
        _nameMcpToolsCallRequestDataParams = def
      , _argumentsMcpToolsCallRequestDataParams  = def
      }


-- |
--
data McpToolsCallRequestData =
  McpToolsCallRequestData {
    _jsonrpcMcpToolsCallRequestData :: JsonRpcRequest
  , _paramsMcpToolsCallRequestData  :: McpToolsCallRequestDataParams
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpToolsCallRequestData", omitNothingFields = True} ''McpToolsCallRequestData)
makeLenses ''McpToolsCallRequestData

instance Default McpToolsCallRequestData where
  def = McpToolsCallRequestData {
        _jsonrpcMcpToolsCallRequestData = def
      , _paramsMcpToolsCallRequestData  = def
      }


-- |
--
data McpRequest =
    McpInitializeRequest McpInitializeRequestData
  | McpInitializedNotification McpInitializedNotificationData
  | McpToolsListRequest McpToolsListRequestData
  | McpToolsCallRequest McpToolsCallRequestData
  deriving (Show, Read, Eq)

-- $(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpRequest", omitNothingFields = True} ''McpRequest)
-- makeLenses ''McpRequest
-- $(deriveJSON defaultOptions{sumEncoding = UntaggedValue} ''McpRequest)

--------------------------------------------------------------------------------
-- |
--
data McpInitializeResponseResultCapabilitiesParams =
  McpInitializeResponseResultCapabilitiesParams {
    _listChangedMcpInitializeResponseResultCapabilitiesParams :: Bool
  } deriving (Show, Read, Eq)
$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpInitializeResponseResultCapabilitiesParams", omitNothingFields = True} ''McpInitializeResponseResultCapabilitiesParams)
makeLenses ''McpInitializeResponseResultCapabilitiesParams

instance Default McpInitializeResponseResultCapabilitiesParams where
  def = McpInitializeResponseResultCapabilitiesParams {
        _listChangedMcpInitializeResponseResultCapabilitiesParams = def
      }

data McpInitializeResponseResultCapabilitiesResources =
  McpInitializeResponseResultCapabilitiesResources {
    _subscribeMcpInitializeResponseResultCapabilitiesResources   :: Bool
  , _listChangedMcpInitializeResponseResultCapabilitiesResources :: Bool
  } deriving (Show, Read, Eq)
$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpInitializeResponseResultCapabilitiesResources", omitNothingFields = True} ''McpInitializeResponseResultCapabilitiesResources)
makeLenses ''McpInitializeResponseResultCapabilitiesResources

instance Default McpInitializeResponseResultCapabilitiesResources where
  def = McpInitializeResponseResultCapabilitiesResources {
        _subscribeMcpInitializeResponseResultCapabilitiesResources = def
      , _listChangedMcpInitializeResponseResultCapabilitiesResources = def
      }

-- |
--
data McpInitializeResponseResultCapabilitiesTools =
  McpInitializeResponseResultCapabilitiesTools {
    _listChangedMcpInitializeResponseResultCapabilitiesTools :: Bool
  } deriving (Show, Read, Eq)
$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpInitializeResponseResultCapabilitiesTools", omitNothingFields = True} ''McpInitializeResponseResultCapabilitiesTools)
makeLenses ''McpInitializeResponseResultCapabilitiesTools

instance Default McpInitializeResponseResultCapabilitiesTools where
  def = McpInitializeResponseResultCapabilitiesTools {
        _listChangedMcpInitializeResponseResultCapabilitiesTools = def
      }

-- |
--
data McpInitializeResponseResultCapabilities =
  McpInitializeResponseResultCapabilities {
    _promptsMcpInitializeResponseResultCapabilities   :: McpInitializeResponseResultCapabilitiesParams
  , _resourcesMcpInitializeResponseResultCapabilities :: McpInitializeResponseResultCapabilitiesResources
  , _toolsMcpInitializeResponseResultCapabilities     :: McpInitializeResponseResultCapabilitiesTools
  } deriving (Show, Read, Eq)
$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpInitializeResponseResultCapabilities", omitNothingFields = True} ''McpInitializeResponseResultCapabilities)
makeLenses ''McpInitializeResponseResultCapabilities

instance Default McpInitializeResponseResultCapabilities where
  def = McpInitializeResponseResultCapabilities {
        _promptsMcpInitializeResponseResultCapabilities = def
      , _resourcesMcpInitializeResponseResultCapabilities = def
      , _toolsMcpInitializeResponseResultCapabilities = def
      }

-- |
--
data McpInitializeResponseResultServerInfo =
  McpInitializeResponseResultServerInfo {
    _nameMcpInitializeResponseResultServerInfo    :: String
  , _versionMcpInitializeResponseResultServerInfo :: String
  } deriving (Show, Read, Eq)
$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpInitializeResponseResultServerInfo", omitNothingFields = True} ''McpInitializeResponseResultServerInfo)
makeLenses ''McpInitializeResponseResultServerInfo

instance Default McpInitializeResponseResultServerInfo where
  def = McpInitializeResponseResultServerInfo {
        _nameMcpInitializeResponseResultServerInfo = def
      , _versionMcpInitializeResponseResultServerInfo = def
      }

-- |
--
data McpInitializeResponseResult =
  McpInitializeResponseResult {
    _protocolVersionMcpInitializeResponseResult :: String
  , _capabilitiesMcpInitializeResponseResult    :: McpInitializeResponseResultCapabilities
  , _serverInfoMcpInitializeResponseResult      :: McpInitializeResponseResultServerInfo
  , _instructionsMcpInitializeResponseResult    :: String
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpInitializeResponseResult", omitNothingFields = True} ''McpInitializeResponseResult)
makeLenses ''McpInitializeResponseResult

instance Default McpInitializeResponseResult where
  def = McpInitializeResponseResult {
        _protocolVersionMcpInitializeResponseResult = def
      , _capabilitiesMcpInitializeResponseResult = def
      , _serverInfoMcpInitializeResponseResult = def
      , _instructionsMcpInitializeResponseResult = def
      }


-- |
--
data McpInitializeResponseData =
  McpInitializeResponseData {
    _jsonrpcMcpInitializeResponseData :: JsonRpcRequest
  , _resultMcpInitializeResponseData  :: McpInitializeResponseResult
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpInitializeResponseData", omitNothingFields = True} ''McpInitializeResponseData)
makeLenses ''McpInitializeResponseData

instance Default McpInitializeResponseData where
  def = McpInitializeResponseData {
        _jsonrpcMcpInitializeResponseData = def
      , _resultMcpInitializeResponseData = def
      }


-- |
--
data McpToolsListResponseResult =
  McpToolsListResponseResult {
    _toolsMcpToolsListResponseResult :: RawJsonByteString
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpToolsListResponseResult", omitNothingFields = True} ''McpToolsListResponseResult)
makeLenses ''McpToolsListResponseResult

instance Default McpToolsListResponseResult where
  def = McpToolsListResponseResult {
        _toolsMcpToolsListResponseResult = def
      }

-- |
--
data McpToolsListResponseData =
  McpToolsListResponseData {
    _jsonrpcMcpToolsListResponseData :: JsonRpcRequest
  , _resultMcpToolsListResponseData  :: McpToolsListResponseResult
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpToolsListResponseData", omitNothingFields = True} ''McpToolsListResponseData)
makeLenses ''McpToolsListResponseData

instance Default McpToolsListResponseData where
  def = McpToolsListResponseData {
        _jsonrpcMcpToolsListResponseData = def
      , _resultMcpToolsListResponseData = def
      }


-- |
--
data McpToolsCallResponseResultContent =
  McpToolsCallResponseResultContent {
    _typeMcpToolsCallResponseResultContent :: String
  , _textMcpToolsCallResponseResultContent :: String
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpToolsCallResponseResultContent", omitNothingFields = True} ''McpToolsCallResponseResultContent)
makeLenses ''McpToolsCallResponseResultContent

instance Default McpToolsCallResponseResultContent where
  def = McpToolsCallResponseResultContent {
        _typeMcpToolsCallResponseResultContent = def
      , _textMcpToolsCallResponseResultContent = def
      }


-- |
--
data McpToolsCallResponseResult =
  McpToolsCallResponseResult {
    _contentMcpToolsCallResponseResult :: [McpToolsCallResponseResultContent]
  , _isErrorMcpToolsCallResponseResult :: Bool
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpToolsCallResponseResult", omitNothingFields = True} ''McpToolsCallResponseResult)
makeLenses ''McpToolsCallResponseResult

instance Default McpToolsCallResponseResult where
  def = McpToolsCallResponseResult {
        _contentMcpToolsCallResponseResult = def
      , _isErrorMcpToolsCallResponseResult = def
      }


-- |
--
data McpToolsCallResponseData =
  McpToolsCallResponseData {
    _jsonrpcMcpToolsCallResponseData :: JsonRpcRequest
  , _resultMcpToolsCallResponseData  :: McpToolsCallResponseResult
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpToolsCallResponseData", omitNothingFields = True} ''McpToolsCallResponseData)
makeLenses ''McpToolsCallResponseData

instance Default McpToolsCallResponseData where
  def = McpToolsCallResponseData {
        _jsonrpcMcpToolsCallResponseData = def
      , _resultMcpToolsCallResponseData = def
      }

-- |
--
data McpResponse =
    McpInitializeResponse McpInitializeResponseData
  | McpToolsListResponse McpToolsListResponseData
  | McpToolsCallResponse McpToolsCallResponseData
  deriving (Show, Read, Eq)

--------------------------------------------------------------------------------
-- |
--
data InitializeCommandData = InitializeCommandData {
    _paramsInitializeCommandData   :: Int
  , _callbackInitializeCommandData :: (Int -> IO ())
  }

makeLenses ''InitializeCommandData

data LaunchCommandData = LaunchCommandData


-- |
--
data SystemCommandDataParams =
  SystemCommandDataParams {
    _arg1SystemCommandDataParams :: String
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "SystemCommandDataParams", omitNothingFields = True} ''SystemCommandDataParams)
makeLenses ''SystemCommandDataParams

instance Default SystemCommandDataParams where
  def = SystemCommandDataParams {
        _arg1SystemCommandDataParams = def
      }

-- |
--
type SystemCommandCallback a = ExitCode
                            -> String
                            -> String
                            -> IO a

-- |
--
data SystemCommandData =
  SystemCommandData {
    _nameSystemCommandData :: String
  , _argumentsSystemCommandData :: RawJsonByteString
  , _callbackSystemCommandData  :: SystemCommandCallback ()
  }

makeLenses ''SystemCommandData

-- |
--
data Command =
    InitializeCommand InitializeCommandData
  | SystemCommand     SystemCommandData
  | LaunchCommand     LaunchCommandData


--------------------------------------------------------------------------------
-- |
--
data DomainData = DomainData {
    _logDirDomainData        :: Maybe String
  , _logLevelDomainData      :: LogLevel
  , _scriptsDirDomainData    :: String
  , _requestQueueDomainData  :: TQueue McpRequest
  , _responseQueueDomainData :: TQueue McpResponse
  , _commandQueueDomainData  :: TQueue Command
  }

makeLenses ''DomainData

defaultDomainData :: IO DomainData
defaultDomainData = do
  reqQ <- newTQueueIO
  resQ <- newTQueueIO
  cmdQ <- newTQueueIO
  return DomainData {
           _logDirDomainData        = Nothing
         , _logLevelDomainData      = LevelDebug
         , _scriptsDirDomainData    = "./scripts"
         , _requestQueueDomainData  = reqQ
         , _responseQueueDomainData = resQ
         , _commandQueueDomainData  = cmdQ
         }

-- |
--
type ErrorData = String
type DomainContext a = DomainData -> IO a



