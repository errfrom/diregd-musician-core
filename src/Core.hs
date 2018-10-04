{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings            #-}

module Core ( audioDevicesAvailable, main ) where

import qualified Language.C.Inline       as C
import           Foreign.Storable             (peek)
import           Foreign.Ptr                  (castPtr)
import           Control.Monad                (void)

import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource (ResIO, runResourceT)
import           Data.Proxy                   (Proxy(Proxy))
import           Internal.Foreign
import           Types

C.context (C.baseCtx <> C.bsCtx)
C.include "<portaudio.h>"
C.include "<alsa/asoundlib.h>"
C.include "<stdlib.h>"
C.include "<string.h>"


audioDevicesAvailable :: ResIO PaDeviceList
audioDevicesAvailable = do
  (_, ptrDeviceList') <- malloc (Proxy :: Proxy (Ptr a))
  paEitherLog "PA Available Devices" . liftIO $ [C.block| long {
    PaDeviceIndex numDevices = Pa_GetDeviceCount();
    if (numDevices < 0) {
      return numDevices; /* errCode == numDevices */
    } else {
      PaDeviceInfo** deviceList = $(void** ptrDeviceList');
      PaDeviceInfo** deviceList = (PaDeviceInfo**) malloc(numDevices * sizeof(PaDeviceInfo*));
      for (size_t i=0; i<numDevices; i++) {
        *(deviceList + i) = (PaDeviceInfo*) malloc(sizeof(PaDeviceInfo));
        *(deviceList + i) = Pa_GetDeviceInfo(i);
      }
    }
    return 0;
  } |]
  let ptrDeviceList = castPtr ptrDeviceList' :: Ptr (Ptr PaDeviceInfo)
  deviceList <- liftIO (peek ptrDeviceList >>= peek)
  return [deviceList]

main :: IO ()
main = print =<< (runResourceT $ do
  paEitherLog "PA Init" initPa
  r <- audioDevicesAvailable
  return r)
  where initPa = liftIO $ [C.block| long {
          void phantom_error_handler(const char* file, int line, const char* fun,
                                     int err, const char* fmt,...) { ; };
          snd_lib_error_set_handler(&phantom_error_handler);
          return 0;
          err = Pa_Initialize();
          if (err != paNoError) {
            return err;
          }
        } |]
