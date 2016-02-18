{-# LANGUAGE TupleSections #-}
module Parse (parseSimulation) where

import Data.Maybe (fromJust)
import Data.Map (Map)
import qualified Data.Map as Map

import Types

parseWarehouses :: [String] -> [Warehouse]
parseWarehouses [] = []
parseWarehouses (l1:l2:ls) = parseWarehouse l1 l2 : parseWarehouses ls

parseWarehouse :: String -> String -> Warehouse
parseWarehouse loc prods = Warehouse (read x, read y) prodmap
  where
    [x,y] = words loc
    prodmap = Map.fromList $ filter ((/=) 0 . snd) $ zip (map ProdId [0..]) $ map read $ words prods

parseOrders :: [String] -> [Order]
parseOrders [] = []
parseOrders (l1:l2:l3:ls) = parseOrder l1 l2 l3 : parseOrders ls

parseOrder :: String -> String -> String -> Order
parseOrder loc noProds prodTypes = Order (read x, read y) (read noProds) prodmap
  where
    [x,y] = words loc
    prodmap = foldl updateMap Map.empty $ map (ProdId . read) (words prodTypes)

updateMap :: Map ProdId Int -> ProdId -> Map ProdId Int
updateMap m k = case Map.lookup k m of
  Nothing -> Map.insert k 1 m
  Just n -> Map.adjust (+1) k m

parseSimulation :: String -> (SimConfig, SimState)
parseSimulation str =
  (SimConfig (read rows) (read cols) (read drones)
            (read deadline) (read sizeOfDrones) prods
  ,SimState warehouses orders ds)
  where
    (l:noProds:prodTypes:noWarehouses:ls) = lines str
    [rows, cols, drones, deadline, sizeOfDrones] = words l
    prods = Map.fromList $ zip (map ProdId [0..]) $ map read (words prodTypes)
    warehouses = Map.fromList $ zip (map WarehouseId [0..]) $ parseWarehouses $ take (2 * (read noWarehouses)) ls
    orders = Map.fromList $ zip (map OrderId [0..]) $ parseOrders $ drop (2*(read noWarehouses) +1 ) ls
    firstWh = wLocation $ fromJust $ Map.lookup (WarehouseId 0) warehouses
    newDrone i = (DroneId i, Drone firstWh 0 (DroneCommand (DroneId i) $ Wait 0) Map.empty)
    ds = Map.fromList $ map newDrone [0..read drones-1]
