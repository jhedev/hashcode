module Parse (parseSimulation) where

import Data.Maybe (fromJust)
import Data.Map (Map)
import qualified Data.Map as Map

import Types

parseWarehouses :: [String] -> [WareHouse]
parseWarehouses [] = []
parseWarehouses (l1:l2:ls) = parseWarehouse l1 l2 : parseWarehouses ls

parseWarehouse :: String -> String -> WareHouse
parseWarehouse loc prods = WareHouse (read x, read y) prodmap
  where
    [x,y] = words loc
    prodmap = Map.fromList $ filter ((/=) 0 . snd) $ zip [0..] $ map read $ words prods

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

parseSimulation :: String -> (SimConfig, SimState)
parseSimulation str =
  (SimConfig (read rows) (read cols) (read drones)
            (read deadline) (read sizeOfDrones) prods
  , SimState 0 warehouses orders ds)
  where
    (l:noProds:prodTypes:noWarehouses:ls) = lines str
    [rows, cols, drones, deadline, sizeOfDrones] = words l
    prods = Map.fromList $ zip [0..((read noProds)-1)] $ map read (words prodTypes)
    warehouses = Map.fromList $ zip [0..] $ parseWarehouses $ take (2 * (read noWarehouses)) ls
    orders = Map.fromList $ zip [0..] $ parseOrders $ drop (2*(read noWarehouses) +1 ) ls
    firstWh = wLocation $ fromJust $ Map.lookup 0 warehouses
    newDrone = Drone firstWh (0, Wait 0 0) Map.empty
    ds = Map.fromList $ zip [0..(read drones)-1] (repeat newDrone)
