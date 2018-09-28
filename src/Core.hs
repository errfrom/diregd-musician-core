{-# LANGUAGE TemplateHaskell, QuasiQuotes                   #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards             #-}
{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}

module Core ( audioDevicesAvailable, main ) where

import           Control.Monad.Trans.Reader (ReaderT(runReaderT), ask)
import           Control.Monad.IO.Class     (liftIO)
import           Data.IORef                 (IORef, modifyIORef', readIORef, newIORef)
import           Foreign.Ptr                (Ptr)
import qualified Foreign.Ptr       as Ptr   (plusPtr, nullPtr)
import           Foreign.Storable           (Storable, sizeOf)
import qualified Language.C.Inline as C
import qualified Foreign.C.String  as C     (peekCAString)
import           Foreign.C.Types            (CLong, CChar)
import           Foreign.Marshal.Alloc      (malloc, free)

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

data AnyPtr = forall a. Storable a => AnyPtr (Ptr a)

data DynamicMemoryManager = DynamicMemoryManager
  { heapPointers :: IORef [AnyPtr] }

type DMM a = ReaderT DynamicMemoryManager IO a

dmmMalloc :: forall a. Storable a => DMM (Ptr a)
dmmMalloc = do
  ptr <- liftIO malloc :: DMM (Ptr a)
  DynamicMemoryManager{..} <- ask
  liftIO $ modifyIORef' heapPointers (\hp -> AnyPtr ptr : hp)
  return ptr

freeMM :: DMM ()
freeMM = do
  DynamicMemoryManager{..} <- ask
  liftIO $ do
    heapPointers' <- readIORef heapPointers
    mapM_ worker heapPointers'
    modifyIORef' heapPointers (\_ -> [])
  where worker (AnyPtr ptr) = free ptr

type Ptr' a = DMM (Ptr a)

audioDevicesAvailable :: DMM (Either PortAudioError DevicesList)
audioDevicesAvailable = do
  ptrErrorCode <- dmmMalloc :: Ptr' CLong
  ptrDeviceNames <- dmmMalloc :: Ptr' (Ptr CChar)
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
  return result;
  where obtainPAError :: Ptr CLong -> DMM PortAudioError
        obtainPAError ptrErrorCode = do
          ptrErrorText <- dmmMalloc :: Ptr' CChar
          _ <- liftIO $ [C.block| int {
            strcpy($(char* ptrErrorText), Pa_GetErrorText(*$(long* ptrErrorCode)));
            return 0;
          } |]
          errorText <- liftIO $ C.peekCAString ptrErrorText
          errorCode' <- liftIO $ [C.block| int { return *$(long* ptrErrorCode); } |]
          let errorCode = fromIntegral errorCode' :: Int
          return PortAudioError{..}

        buildDevicesList :: Ptr (Ptr CChar) -> Int -> DMM DevicesList
        buildDevicesList _ 0 = return []
        buildDevicesList ptrDeviceNames numDevices = do
          ptrDeviceName <- dmmMalloc :: Ptr' CChar
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
  let dmm = DynamicMemoryManager{..}
  result <- runReaderT audioDevicesAvailable dmm
  print result
