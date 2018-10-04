{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE StrictData, DeriveGeneric    #-}

module Types
  ( PaDeviceInfo(..), PaDeviceList
  , PaAudioError(..), paErrorByCode, paEitherLog
  ) where

import qualified Language.C.Inline       as C
import           GHC.Generics                 (Generic)
import           Foreign.C.Types              (CInt, CLong, CDouble, CChar)
import           Foreign.C.String             (CString)
import qualified Foreign.C.String        as C (peekCAString)
import           Foreign.Storable.Generic
import           Control.Monad                (void)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Logger         (ToLogStr(..), LogLevel(..), MonadLogger(..), LogSource)
import qualified Control.Monad.Logger    as L (logWithoutLoc)
import           Control.Monad.Trans.Resource (ResourceT, MonadResource)
import           Control.Exception            (Exception(..))
import qualified Control.Exception       as E (throwIO)
import           Data.String                  (IsString(..))
import           Data.Proxy                   (Proxy(Proxy))
import           Internal.Foreign             (malloc)

type PaHostApiIndex     = CInt
type PaTime             = CDouble
type PaDeviceList       = [PaDeviceInfo]


data PaDeviceInfo = PaDeviceInfo
  { _structVersion              :: CInt
  , padName                     :: CString
  , padAudioApi                 :: PaHostApiIndex
  , padMaxInputChannels         :: CInt
  , padMaxOutputChannels        :: CInt
  , padDefaultLowInputLatency   :: PaTime
  , padDefaultLowOutputLatency  :: PaTime
  , padDefaultHighInputLatency  :: PaTime
  , padDefaultHighOutputLatency :: PaTime
  , padDefaultSampleRate        :: CDouble
  } deriving (Show, Generic)

instance GStorable PaDeviceInfo


data PaAudioError = PaAudioError Int String
  deriving (Show)

instance Exception PaAudioError
instance ToLogStr PaAudioError where
  toLogStr (PaAudioError errCode errText) = fromString $
    "PaError ~ code " ++ show errCode ++ ", " ++ errText

paErrorByCode :: (MonadResource m) => CLong -> ResourceT m PaAudioError
paErrorByCode errCode = do
  (_, ptrErrText) <- malloc (Proxy :: Proxy CChar)
  liftIO $ do
    copyErrorText ptrErrText errCode
    errText <- C.peekCAString ptrErrText
    return $ PaAudioError (fromIntegral errCode) errText
  where copyErrorText ptr errCode = void $ [C.block| int {
          strcpy($(char* ptr), Pa_GetErrorText($(long errCode)));
          return 0;
        } |]

paEitherLog :: (MonadResource m, MonadLogger m) => LogSource -> ResourceT m CLong -> ResourceT m ()
paEitherLog logSource action = action >>= \result ->
  if ((toEnum . fromIntegral) result :: Bool)
    then let errCode = result
         in do
           paError <- paErrorByCode errCode
           L.logWithoutLoc logSource LevelError paError
           liftIO . void $ E.throwIO paError
           return ()
    else return ()
