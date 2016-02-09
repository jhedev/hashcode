module Main where

import Debug.Trace
--import Data.Matrix hiding (trace)
import Numeric.LinearAlgebra.Data
import Numeric.LinearAlgebra.Devel
import Numeric.LinearAlgebra.HMatrix

import System.Environment

data PaintCommand = PaintSquare Int Int Int
                  | PaintLine Int Int Int Int
                  | EraseCell Int Int

instance Show PaintCommand where
  show (PaintSquare r c s) = unwords ["PAINT_SQUARE", show r, show c, show s]
  show (PaintLine r1 c1 r2 c2) = unwords ["PAINT_LINE", show r1, show c1, show r2, show c2]
  show (EraseCell r c) = unwords ["ERASE_CELL", show r, show c]

matrixFromString :: String -> Matrix Double
matrixFromString str = matrix (read noCols) elems
  where
    (firstLine:ls) = lines str
    [noRows, noCols] = words firstLine
    elems = map toBool $ concat ls
    toBool '.' = 0
    toBool '#' = 1

fromFile :: FilePath -> IO (Matrix Double)
fromFile fp = do
  contents <- readFile fp
  return $ matrixFromString contents

toString :: [PaintCommand] -> String
toString pCmds = unlines $ (show (length pCmds)) : map show pCmds

toFile :: FilePath -> [PaintCommand] -> IO ()
toFile fp pCmds = writeFile fp (toString pCmds)

withIndex :: (Int, Int) -> ((Int, Int), [PaintCommand]) -> Double -> ((Int, Int), [PaintCommand])
withIndex (noRows, noCols) ((row, col), cmds) i = case i of
    1 -> (newIndex, PaintSquare row col 0 :cmds)
    _ -> (newIndex, cmds)
 where
   numCells = row*noCols+col+1
   newIndex = (numCells `div` noCols , numCells `mod` noCols)

findCells :: Matrix Double -> ([PaintCommand], Matrix Double)
findCells mat =(cmds, mat)
  where
    (_, cmds) = foldl (withIndex (noRows, noCols)) ((0,0), []) $ concat $ toLists mat
    noRows = rows mat
    noCols = cols mat

findLines :: Matrix Double -> ([PaintCommand], Matrix Double)
findLines mat = ([], mat)

findSquares :: Matrix Double -> ([PaintCommand], Matrix Double)
findSquares mat = (cmds, mat - painted)
  where
    (noRows, noCols) = size mat
    filterMat = (3><3) [1..]
    result = conv2 mat filterMat
    indexes = find (==9) result
    cmds = map (\(r,c) -> PaintSquare r c 3) indexes
    painted = sum (+) $ map toMat indexes


calc :: Matrix Double -> [PaintCommand]
calc mat = cmds1 ++ cmds2 ++ cmds3
  where
    (cmds1, mat1) = findSquares mat
    (cmds2, mat2) = findLines mat1
    (cmds3, mat3) = findCells mat2

main :: IO ()
main = do
  (title:_) <- getArgs
  mat <- fromFile $ title ++ ".in"
  let result = calc mat
  toFile (title ++ ".out") result
