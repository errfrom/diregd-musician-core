{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts, AllowAmbiguousTypes
                            #-}

module Internal.HeapPM
  ( HeapPM, Ptr'
  , pmAlloc
  , pmMalloc, pmMallocBytes
  , pmCalloc, pmCallocBytes
  , pmFree, pmFreeAll ) where

import           Foreign.Storable                (Storable)
import           Foreign.Ptr                     (Ptr, nullPtr)
import           Foreign.Marshal.Alloc           (malloc, mallocBytes, calloc, callocBytes, free)
import           Control.Monad.IO.Class          (liftIO)
import           Control.Monad.Trans.Reader      (ReaderT, ask)
import           Data.Typeable                   (Typeable, cast)
import qualified Data.List                  as L (partition)
import           Data.IORef                      (IORef, readIORef, modifyIORef')

type S a    = (Storable a, Eq a, Typeable a)
type Ptr' a = HeapPM (Ptr a)


-- Existential datatype that handles pointers of any type.
data AnyPtr = forall a. S a => AnyPtr (Ptr a)

instance Eq AnyPtr where
  AnyPtr a == AnyPtr b = Just a == cast b

-- Heap Pointers Manager.
-- Reader transformer that stores all the pointers initialized with malloc.
-- So one has the capability to manage memory freed that way.
type HeapPM a = ReaderT (IORef [AnyPtr]) IO a


-- Heap memory allocation

pmAlloc :: forall ptr. S ptr => IO (Ptr ptr) -> HeapPM (Ptr ptr)
pmAlloc funAlloc = do
  ptr <- liftIO funAlloc :: HeapPM (Ptr ptr)
  heapPointersIORef <- ask
  liftIO $ modifyIORef' heapPointersIORef (\hp -> AnyPtr ptr : hp)
  return ptr

pmMalloc, pmCalloc :: forall ptr. S ptr => HeapPM (Ptr ptr)
pmMalloc = pmAlloc malloc
pmCalloc = pmAlloc calloc

pmMallocBytes, pmCallocBytes :: forall ptr. S ptr => Int -> HeapPM (Ptr ptr)
pmMallocBytes = pmAlloc . mallocBytes
pmCallocBytes = pmAlloc . callocBytes


-- Heap memory freeing

pmFree :: forall ptr. S ptr => Ptr ptr -> HeapPM (Maybe ())
pmFree ptr
  | ptr == nullPtr = return Nothing
  | otherwise      = ask >>= \heapPointersIORef -> liftIO $ do
      heapPointers <- readIORef heapPointersIORef
      let (samePointers, restPointers) = L.partition (\x -> x == AnyPtr ptr) heapPointers
      -- It is redundant to free all the pointers because
      -- they merely point to the same piece of memory.
      -- So only the first one is to be freed.
      case samePointers of
        []      -> return Nothing
        (AnyPtr ptr:_) -> do
          free ptr
          modifyIORef' heapPointersIORef (\_ -> restPointers)
          return (Just ())

-- Frees all the memory allocated during the execution.
pmFreeAll :: HeapPM ()
pmFreeAll = ask >>= \heapPointersIORef -> do
  heapPointers <- liftIO (readIORef heapPointersIORef)
  mapM_ (\(AnyPtr ptr) -> pmFree ptr) heapPointers
