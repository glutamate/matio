{-# LANGUAGE ScopedTypeVariables, GADTs #-}
module Main where

import Control.Monad
import Data.Complex
import Data.Int
import Data.List
import Data.Mat
import qualified Data.Map
import Data.Maybe
import System.Environment
import System.Exit
import System.IO

-- A universal datatype for MAT data.
data MatVal =
    VDouble  Double
  | VInt64   Int64
  | VBool    Bool
  | VChar    Char
  | VComplex (Complex MatVal)
  | VStruct  (Data.Map.Map String MatVal)
  | VArray   Int [MatVal]
  deriving (Eq, Show)

-- | Transform from the types into the universal representation.
toMatVal :: MatData -> MatVal
toMatVal (MatData MatDouble d)               = VDouble d
toMatVal (MatData MatInt64  i)               = VInt64  i
toMatVal (MatData MatBool   b)               = VBool   b
toMatVal (MatData MatChar   c)               = VChar   c
toMatVal (MatData MatCell   d)               = toMatVal d
toMatVal (MatData (MatStruct (Known fs)) ds) = VStruct (Data.Map.fromList (zip fs (map toMatVal ds)))
toMatVal (MatData (MatArray t (Known d)) xs) = VArray d (map (toMatVal . MatData t) xs)
toMatVal (MatData (MatComplex t) (x :+ y))   = matZipWith (\ r i -> VComplex (r :+ i)) (toMatVal (MatData t x)) (toMatVal (MatData t y))

-- | Push down complex values.
matZipWith :: (MatVal -> MatVal -> MatVal) -> MatVal -> MatVal -> MatVal
matZipWith op (VStruct xs) (VStruct ys)   = VStruct (Data.Map.unionWith (matZipWith op) xs ys)
matZipWith op (VArray d xs) (VArray _ ys) = VArray d (zipWith (matZipWith op) xs ys)
matZipWith op x y = op x y

main :: IO ()
main = do
  (fp:_) <- getArgs
  -- Read an entire MAT file
  xs :: [(String, MatData)] <- liftM Data.Map.toList (readMatFile fp)
  putStrLn "Variables contained in MAT file:"
  -- Print all the contents
  mapM_ (\ (n, d) -> putStr (n ++ " ->\n  " ++ show (toMatVal d) ++ "\n")) xs
  exitSuccess
