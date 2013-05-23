{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad
import Data.List
import Data.Mat
import qualified Data.Map
import Data.Maybe
import System.Environment
import System.Exit
import System.IO

main :: IO ()
main = do
  (fp:_) <- getArgs
  -- Read an entire MAT file
  xs :: [(String, MatData)] <- liftM Data.Map.toList (readMatFile fp)
  putStrLn "Variables contained in MAT file:"
  -- Print all the contents
  mapM_ (\ (n, MatData t d) -> case matShowDict t of
           ShowDict -> putStr (n ++ " :: " ++ showMatType t ++ " ->\n  " ++ show d ++ "\n")) xs
  putStrLn ""
  putStrLn "Extracting all two-dimensional arrays of doubles:"
  let ys :: [(String, [[Double]])]
      ys = [ (x, ds) | (x, v) <- xs, Just ds <- [matCast v (dim2 MatDouble)] ]
  mapM_ (\ (n, ds) -> putStr (n ++ " ->\n  " ++ show ds ++ "\n")) ys
  exitSuccess
