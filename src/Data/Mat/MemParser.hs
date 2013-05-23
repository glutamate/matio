module Data.Mat.MemParser where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Complex
import Foreign

import Data.Mat.Raw

-- | A 'MemParser a b' is a parser that for an array of 'a's to a Haskell structure
-- of 'b's. It maintains an initial pointer to an 'a' and an integer offset.
type MemParser a = ReaderT (Ptr a) (StateT Int IO)

-- | A storable type can be parsed directly.
storable :: Storable a => MemParser a a
storable = do
  offset <- get
  p <- ask
  x <- liftIO (peekElemOff p offset)
  put (offset + 1)
  return x

-- | Parses an array of the given dimension. Takes a parser for the elements.
array :: Int -> MemParser a b -> MemParser a [b]
array n pe = replicateM n pe

-- | Parses a complex value of the given element type.
complex :: MemParser a b -> MemParser (CMatComplexSplit a) (Complex b)
complex pe = do
  ptr <- ask
  liftIO $ do
    c   <- peek ptr
    xRe <- runMemParser pe (re c)
    xIm <- runMemParser pe (im c)
    return $ xRe :+ xIm

-- | Parses nothing.
nothing :: MemParser a ()
nothing = return ()

-- | Runs a 'MemParser' by taking a pointer to the initial element.
runMemParser :: MemParser a b -> Ptr a -> IO b
runMemParser mp p = evalStateT (runReaderT mp p) 0
