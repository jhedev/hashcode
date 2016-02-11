module Main where

import Debug.Trace

import Data.Map (Map)
import qualified Data.Map as Map

import System.Environment

type DroneId = Int
type WareHouseId = Int
type ProdId = Int
type OrderId = Int

type Weight = Int

type Products = Map ProdId Weight

data Simulation = Simulation
  { rows :: Int
  , cols :: Int
  , noDrones :: Int
  , deadline :: Int
  , sizeDrone :: Int
  , warehouses :: Map WareHouseId WareHouse
  , products :: Products
  , orders :: Map OrderId Order
  } deriving Show

data WareHouse = WareHouse
  { wLocation :: (Int, Int)
  , wProducts :: Map Int Int
  } deriving Show

data Order = Order
  { oLocation :: (Int, Int)
  , oProducts :: Map Int Int
  } deriving Show



data DroneCommand = Load DroneId WareHouseId ProdId Int
                   | Unload DroneId WareHouseId ProdId Int
                   | Deliver DroneId OrderId ProdId Int
                   | Wait DroneId Int

instance Show DroneCommand where
  show (Load d w p i) = unwords [show d, "L", show w, show p, show i]
  show (Unload d w p i) = unwords [show d, "U", show w, show p, show i]
  show (Deliver d o p i) = unwords [show d, "D", show o, show p, show i]
  show (Wait d i) = unwords [show d, "W", show i]

calc :: Simulation -> IO [DroneCommand]
calc input = return [Load 1 2 3 4]

parseWarehouses :: [String] -> [WareHouse]
parseWarehouses [] = []
parseWarehouses (l1:l2:ls) = parseWarehouse l1 l2 : parseWarehouses ls

parseWarehouse :: String -> String -> WareHouse
parseWarehouse loc prods = WareHouse (read x, read y) prodmap
  where
    [x,y] = words loc
    prodmap = Map.fromList $ zip [0..] $ map read $ words prods

parseOrders :: [String] -> [Order]
parseOrders [] = []
parseOrders (l1:l2:l3:ls) = parseOrder l1 l2 l3 : parseOrders ls

parseOrder :: String -> String -> String -> Order
parseOrder loc noProds prodTypes = Order (read x, read y) prodmap
  where
    [x,y] = words loc
    prodmap = foldl updateMap Map.empty $ map read (words prodTypes)

updateMap :: Map Int Int -> Int -> Map Int Int
updateMap m k = case Map.lookup k m of
  Nothing -> Map.insert k 1 m
  Just n -> Map.adjust (+1) k m

parseSimulation :: String -> Simulation
parseSimulation str = Simulation (read rows) (read cols) (read drones)
                                 (read deadline) (read sizeOfDrones) warehouses
                                 prods orders
  where
    (l:noProds:prodTypes:noWarehouses:ls) = lines str
    [rows, cols, drones, deadline, sizeOfDrones] = words l
    prods = Map.fromList $ zip [0..((read noProds)-1)] $ map read (words prodTypes)
    warehouses = Map.fromList $ zip [0..] $ parseWarehouses $ take (2 * (read noWarehouses)) ls
    orders = Map.fromList $ zip [0..] $ parseOrders $ drop (2*(read noWarehouses) +1 ) ls

outputToString :: [DroneCommand] -> String
outputToString cmds = unlines $ show (length cmds) : map show cmds

fromFile :: FilePath -> IO Simulation
fromFile fp = do
  content <- readFile fp
  return $ parseSimulation content

toFile :: FilePath -> [DroneCommand] -> IO ()
toFile fp = writeFile fp . outputToString

main :: IO ()
main = do
  (title:_) <- getArgs
  input <- fromFile (title ++ ".in")
  print input
  output <- calc input
  toFile (title ++ ".out") output
