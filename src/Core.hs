{-# LANGUAGE TemplateHaskell, QuasiQuotes                   #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards             #-}
{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}

module Core ( audioDevicesAvailable, main ) where

import qualified Language.C.Inline          as C
import           Foreign.Ptr                       (Ptr)
import qualified Foreign.Ptr                as Ptr (plusPtr)
import           Foreign.Storable                  (sizeOf)
import qualified Foreign.C.String           as C   (peekCAString)
import           Foreign.C.Types                   (CLong, CChar)
import           Control.Monad.Trans.Reader        (ReaderT(runReaderT))
import           Control.Monad.IO.Class            (liftIO)
import           Data.IORef                        (newIORef)
import           Internal.HeapPM                   (HeapPM, Ptr', pmMalloc, pmFreeAll)

C.context (C.baseCtx <> C.bsCtx)
C.include "<portaudio.h>"
C.include "<alsa/asoundlib.h>"
C.include "<stdlib.h>"
C.include "<string.h>"

data Device = Device String
  deriving (Show)
type DevicesList = [Device]

data PortAudioError = PortAudioError
  { errorCode :: Int
  , errorText :: String }
  deriving (Show)

audioDevicesAvailable :: HeapPM (Either PortAudioError DevicesList)
audioDevicesAvailable = do
  ptrErrorCode <- pmMalloc :: Ptr' CLong
  ptrDeviceNames <- pmMalloc :: Ptr' (Ptr CChar)
  cResult <- liftIO $ [C.block| int {
    void phantom_error_handler(const char* file, int line, const char* fun,
                               int err, const char* fmt,...) { ; };
    snd_lib_error_set_handler(&phantom_error_handler);
    Pa_Initialize();
    PaDeviceIndex numDevices = Pa_GetDeviceCount();
    if (numDevices < 0) {
      *$(long* ptrErrorCode) = numDevices;
      return 1;
    } else {
      PaDeviceInfo* deviceInfo = (PaDeviceInfo*) malloc(sizeof(PaDeviceInfo));
      $(char** ptrDeviceNames) = (char**) malloc(numDevices * sizeof(char*));
      char** deviceNames = $(char** ptrDeviceNames);
      for (size_t i=0; i<numDevices; i++) {
        deviceInfo = Pa_GetDeviceInfo(i);
        size_t nameLength = strlen(deviceInfo->name) + 1;
        *(deviceNames + i) = (char*) malloc(nameLength * sizeof(char));
        strcpy(*(deviceNames + i), deviceInfo->name);
      }
      return numDevices;
    }
  } |]
  liftIO $ print cResult
  result <- if (fromIntegral cResult == 1)
              then Left <$> obtainPAError ptrErrorCode
              else let numDevices = fromIntegral cResult
                   in Right <$> buildDevicesList ptrDeviceNames numDevices
  r <- pmFreeAll
  liftIO $ print r
  return result;
  where obtainPAError :: Ptr CLong -> HeapPM PortAudioError
        obtainPAError ptrErrorCode = do
          ptrErrorText <- pmMalloc :: Ptr' CChar
          _ <- liftIO $ [C.block| int {
            strcpy($(char* ptrErrorText), Pa_GetErrorText(*$(long* ptrErrorCode)));
            return 0;
          } |]
          errorText <- liftIO $ C.peekCAString ptrErrorText
          errorCode' <- liftIO $ [C.block| int { return *$(long* ptrErrorCode); } |]
          let errorCode = fromIntegral errorCode' :: Int
          return PortAudioError{..}

        buildDevicesList :: Ptr (Ptr CChar) -> Int -> HeapPM DevicesList
        buildDevicesList _ 0 = return []
        buildDevicesList ptrDeviceNames numDevices = do
          ptrDeviceName <- pmMalloc :: Ptr' CChar
          liftIO $ print "wow"
          liftIO $ [C.block| int {
            size_t nameLength = strlen(*$(char** ptrDeviceNames)) + 1;
            $(char* ptrDeviceName) = (char*) malloc(nameLength * sizeof(char));
            strcpy($(char* ptrDeviceName), *$(char** ptrDeviceNames));
            return 0;
          } |]
          liftIO $ C.peekCAString ptrDeviceName >>= print
          device <- liftIO (C.peekCAString ptrDeviceName) >>= return . Device
          let s = sizeOf ptrDeviceNames
          restDevices <- buildDevicesList (Ptr.plusPtr ptrDeviceNames s) (pred numDevices)
          return $ device:restDevices

main :: IO ()
main = do
  heapPointers <- newIORef []
  result <- runReaderT audioDevicesAvailable heapPointers
  print result
