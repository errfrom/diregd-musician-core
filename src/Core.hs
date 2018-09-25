{-# LANGUAGE TemplateHaskell, QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification          #-}

module Core ( audioDevicesAvailable ) where

import           Control.Monad.Trans.Reader (ReaderT, ask)
import           Data.Text                  (Text)
import           Data.IORef                 (IORef, modifyIORef', readIORef)
import           Foreign.Ptr                (Ptr)
import           Foreign.Storable           (Storable)
import qualified Language.C.Inline as C
import           Foreign.C.Types            (CLong, CChar)
import           Foreign.Marshal.Alloc      (malloc, free)

C.context C.baseCtx
C.include "<portaudio.h>"

data Device = Device String
type DevicesList = [Device]

data PortAudioError = PortAudioError
  { errorCode :: Int
  , errorText :: Text }
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
    fmap worker heapPointers'
    modifyIORef' heapPointers (\_ -> [])
  where worker (AnyPtr ptr) = free ptr

audioDevicesAvailable :: DMM (Either PortAudioError DevicesList)
audioDevicesAvailable = do
  ptrErrorCode <- dmmMalloc :: DMM (Ptr CLong)
  ptrDeviceNames <- dmmMalloc :: DMM (Ptr (Ptr CChar))
  result <- [C.block| int {
    PaDeviceIndex numDevices = Pa_GetDeviceCount();
    if (numDevices < 0) {
      $(long ptrErrorCode) = numDevices;
      return 1;
    } else {
      const PaDeviceInfo* deviceInfo = (PaDeviceInfo*) malloc(sizeof(PaDeviceInfo));
      for (size_t i=0; i<numDevices; i++) {
        deviceInfo = Pa_GetDeviceInfo(i);
        *($(char** ptrDeviceNames) + i) = deviceInfo->name;
      }
      return 0;
    }
  } |]
  \result -> do { freeMM; return result; } =<< case (toEnum result) of
    False -> Left <$> obtainPAError ptrErrorCode
    True -> Right <$> buildDevicesList ptrDeviceNames
  where obtainPAError :: Ptr CLong -> DMM PortAudioError
        obtainPAError = undefined

        buildDevicesList :: Ptr (Ptr CChar) -> DMM DevicesList
        buildDevicesList = undefined
