{-# LANGUAGE StrictData, DeriveGeneric #-}

module Types
  ( PaDeviceInfo(..), PaDeviceList
  , PaAudioError(..)
  ) where

import           GHC.Generics             (Generic)
import           Foreign.C.Types          (CInt, CDouble)
import           Foreign.C.String         (CString)
import           Foreign.Storable.Generic
import           Control.Monad.Logger     (ToLogStr(..))
import           Data.String              (IsString(..))

type PaHostApiIndex = CInt
type PaTime         = CDouble

type PaDeviceList = [PaDeviceInfo]

data PaDeviceInfo = PaDeviceInfo
  { padName                     :: CString
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

instance ToLogStr PaAudioError where
  toLogStr (PaAudioError errCode errText) = fromString $
    "PaError ~ code " ++ show errCode ++ ", " ++ errText
