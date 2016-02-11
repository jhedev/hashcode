module Main where


import System.Environment

data Input = Input deriving Show

data Output = Output deriving Show

calc :: Input -> IO Output
calc input = undefined

parseInput :: String -> Input
parseInput str = Input
  where
    (l:ls) = lines str
    (x:xs) = words l
    rest = map id ls

outputToString :: Output -> String
outputToString out = show out

fromFile :: FilePath -> IO Input
fromFile fp = do
  content <- readFile fp
  return $ parseInput content

toFile :: FilePath -> Output -> IO ()
toFile fp = writeFile fp . outputToString

main :: IO ()
main = do
  (title:_) <- getArgs
  input <- fromFile (title ++ ".in")
  output <- calc input
  toFile (title ++ ".out") output
