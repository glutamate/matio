import Control.Monad.Reader
import Control.Monad.State
import Foreign

-- Goal. Peek a multi-dimensional array.
-- State: the offset.
--
-- To peek a [4,1,5] array, we 4 times peek a [1,5] array, yielding a [[a]] each, and join them in a list.

peekDimArray :: (Int -> [Int] -> Ptr a -> IO b) -> 
                 Int -> [Int] -> Ptr a -> IO [b]
peekDimArray peekElemArray offset (d : dimens) p =
  let innerSize = product dimens
      offsets   = take d [offset, offset + innerSize ..]
  in  mapM (\ o -> peekElemArray o dimens p) offsets

peekSimple :: Storable a => Int -> [Int] -> Ptr a -> IO a
peekSimple offset _ p = peekElemOff p offset

type MemParser a = ReaderT (Ptr a) (StateT Int IO)

memSimple :: Storable a => MemParser a a
memSimple = do
  offset <- get
  p <- ask
  x <- liftIO (peekElemOff p offset)
  put (offset + 1)
  return x

memArray :: Int -> MemParser a b -> MemParser a [b]
memArray n pe = replicateM n pe

runMemParser :: MemParser a b -> Ptr a -> IO b
runMemParser mp p = evalStateT (runReaderT mp p) 0
