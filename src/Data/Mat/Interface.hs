module Data.Mat.Interface (
  module Data.Mat.Interface,
  MatAcc(..)
) where

import Control.Applicative
import Control.Exception
import Control.Monad
import qualified Data.Map as M
import Foreign
import Foreign.C

import Data.Mat.Data
import Data.Mat.Raw

newtype Mat = Mat { unMat :: Ptr CMat }

openMatFile :: FilePath -> MatAcc -> IO Mat
openMatFile file acc =
  withCString file $ \ cfile ->
    Mat <$>
      throwErrnoIf (== nullPtr) "openMatFile" (cMatOpen cfile acc)

matClose :: Mat -> IO ()
matClose (Mat mat) = Control.Monad.void $ cMatClose mat

withMatFile :: FilePath -> MatAcc -> (Mat -> IO a) -> IO a
withMatFile file acc =
  bracket (openMatFile file acc) matClose

varRead :: Mat -> String -> IO (Maybe MatData)
varRead (Mat mat) n =
  withCString n $ \ cn -> do
    ptr <- cMatVarRead mat cn
    if ptr == nullPtr
      then return Nothing
      else do
        x <- matDataPtr ptr
        cMatVarFree ptr
        return (Just x)

varReadAtType :: Mat -> String -> MatType d a b -> IO (Maybe b)
varReadAtType mat n t = do
  mx <- varRead mat n
  return (mx >>= flip matCast t)

varType :: Mat -> String -> IO (Maybe AnyMatType)
varType (Mat mat) n =
  withCString n $ \ cn -> do
    ptr <- cMatVarReadInfo mat cn
    if ptr == nullPtr
      then return Nothing
      else do
        x <- matTypePtr ptr
        cMatVarFree ptr
        return (Just x)

-- | Read a given MAT file and return all the variables that
-- are contained in it in a 'Map'.
--
-- This is the most high-level function for reading that is
-- provided. If you want to read an entire file, use this.
readMatFile :: FilePath -> IO (M.Map String MatData)
readMatFile file =
  M.fromList <$> withMatFile file rdonly (go . unMat)
  where
    go :: Ptr CMat -> IO [(String, MatData)]
    go mat = loop
      where
        loop :: IO [(String, MatData)]
        loop = do
          ptr <- cMatVarReadNext mat
          if ptr == nullPtr
            then return []
            else do
              v  <- peek ptr
              mn <- matVarName v
              case mn of
                Nothing -> continue ptr -- we silently drop empty names
                Just n  -> do
                  x  <- matData ptr v
                  xs <- continue ptr
                  return ((n, x) : xs)

        continue :: Ptr CMatVar -> IO [(String, MatData)]
        continue ptr = do
          cMatVarFree ptr
          loop

-- | Write support not yet implemented.
writeMatFile :: FilePath -> M.Map String MatData -> IO ()
writeMatFile = undefined
