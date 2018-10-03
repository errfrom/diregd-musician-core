{-# LANGUAGE ScopedTypeVariables #-}

module Internal.Foreign
  ( Ptr
  , alloc, malloc, calloc
  , mallocBytes, callocBytes
  ) where


import           Foreign.Storable             (Storable)
import           Foreign.Ptr                  (Ptr)
import qualified Foreign.Marshal.Alloc   as A (malloc, mallocBytes, calloc, callocBytes, free)
import           Control.Monad.Trans.Resource (ResourceT, ReleaseKey, MonadResource, allocate)


type RPtr m a = ResourceT m (ReleaseKey, Ptr a)

alloc :: forall a m. (Storable a, MonadResource m) => IO (Ptr a) -> RPtr m a
alloc funAlloc = allocate (funAlloc :: IO (Ptr a)) A.free
  where free ptr = A.free

malloc, calloc :: forall proxy a. forall m. (Storable a, MonadResource m) => proxy a -> RPtr m a
malloc _ = alloc A.malloc
calloc _ = alloc A.calloc

mallocBytes, callocBytes :: forall proxy a. forall m. (Storable a, MonadResource m) => proxy a
                                                                                    -> Int
                                                                                    -> RPtr m a
mallocBytes _ = alloc . A.mallocBytes
callocBytes _ = alloc . A.callocBytes
