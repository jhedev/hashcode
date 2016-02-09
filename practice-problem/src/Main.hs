module Main where

import Debug.Trace

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
matrixFromString = fromLists . map (map toBool) . tail . lines
  where
    toBool '.' = 0
    toBool '#' = 1
    toBool _   = error "Unexpected cell character!"

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
findSquares mat =
  let (_, mat', cmds) = removeSqs (firstSize, mat, [])
  in (cmds, mat')
  where
    firstSize = floor $ (fromIntegral $ min (rows mat) (cols mat)) / 4

removeSqs :: (Int, Matrix Double, [PaintCommand]) -> (Int, Matrix Double, [PaintCommand])
removeSqs (filtSize, mat, cmds) =
    case foundSquares of
      ((row',col'):_) ->
        let (row, col) = (row'+filtSize, col'+filtSize)
            mat' = subtractSquare filtSize mat (row,col)
        in removeSqs (filtSize, mat', PaintSquare row col filtSize:cmds)
      _             ->
        if filtSize > 0
        then removeSqs (filtSize-1, mat, cmds)
        else (0, mat, cmds)
  where
    (noRows, noCols) = size mat
    filtLength = filtSize*2+1
    filterMat = konst 1 (filtLength, filtLength)
    result = corr2 filterMat mat
    foundSquares = find (== (fromIntegral $ filtLength^2)) result

subtractSquare :: Int -> Matrix Double -> (Int, Int) -> Matrix Double
subtractSquare filtSize mat (row, col) =
  mat - bigFilter
  where
    (noRows, noCols) = size mat
    filtLength = filtSize*2+1
    filterMat = konst 1 (filtLength, filtLength)
    topPad = max 0 $ row - filtSize
    bottomPad = max 0 $ (noRows-1) - (row+filtSize)
    leftPad = max 0 $ col - filtSize
    rightPad = max 0 $ (noCols-1) - (col+filtSize)
    leftBlock = konst (0 :: Double) (topPad, leftPad)
    rightBlock = konst (0 :: Double) (bottomPad, rightPad)
    bigFilter = diagBlock [leftBlock, filterMat, rightBlock]


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
