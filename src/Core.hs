{-# LANGUAGE TemplateHaskell, QuasiQuotes                   #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards             #-}
{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}

module Core ( audioDevicesAvailable, main ) where

import           Control.Monad.Trans.Reader (ReaderT(runReaderT), ask)
import           Control.Monad.IO.Class     (liftIO)
import           Data.IORef                 (IORef, modifyIORef', readIORef, newIORef)
import           Foreign.Ptr                (Ptr)
import qualified Foreign.Ptr       as Ptr   (plusPtr)
import           Foreign.Storable           (Storable)
import qualified Language.C.Inline as C
import qualified Foreign.C.String  as C     (peekCAString)
import           Foreign.C.Types            (CLong, CChar)
import           Foreign.Marshal.Alloc      (malloc, free)

C.context (C.baseCtx <> C.bsCtx)
C.include "<portaudio.h>"
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
    PaDeviceIndex numDevices = Pa_GetDeviceCount();
    if (numDevices < 0) {
      $(long* ptrErrorCode) = (long*) &numDevices;
      return 1;
    } else {
      PaDeviceInfo* deviceInfo = (PaDeviceInfo*) malloc(sizeof(PaDeviceInfo));
      char** deviceNames = $(char** ptrDeviceNames);
      for (size_t i=0; i<numDevices; i++) {
        deviceInfo = Pa_GetDeviceInfo(i);
        *(deviceNames + i) = deviceInfo->name;
      }
      *(deviceNames + 1) = '\0';
      deviceNames -= numDevices + 1; /* Make it point to the first element. */
      return 0;
    }
  } |]
  result <- case (toEnum . fromIntegral $ cResult) of
    False -> Right <$> buildDevicesList ptrDeviceNames
    True -> Left <$> obtainPAError ptrErrorCode
  freeMM >> return result;
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

        buildDevicesList :: Ptr (Ptr CChar) -> DMM DevicesList
        buildDevicesList ptrDeviceNames = worker ptrDeviceNames
          where worker :: Ptr (Ptr CChar) -> DMM DevicesList
                worker ptrDeviceNames = do
                  ptrDeviceName <- dmmMalloc :: Ptr' CChar
                  cResult <- liftIO $ [C.block| int {
                    char* deviceName = *$(char** ptrDeviceNames);
                    if (*deviceName == '\0') {
                      return 1;
                    } else {
                      strcpy($(char* ptrDeviceName), deviceName);
                      return 0;
                    }
                  } |]
                  case (toEnum . fromIntegral $ cResult) of
                    False -> do
                      res <- liftIO (C.peekCAString ptrDeviceName) >>= return . Device
                      rest <- worker (Ptr.plusPtr ptrDeviceNames 1)
                      return $ res:rest
                    True -> return []

main :: IO ()
main = do
  heapPointers <- newIORef []
  let dmm = DynamicMemoryManager{..}
  result <- runReaderT audioDevicesAvailable dmm
  print result
