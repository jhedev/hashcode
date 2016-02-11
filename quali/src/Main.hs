module Main where

import Control.Monad.State

import Debug.Trace

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

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
  , commands :: [DroneCommand]
  , drones :: Map DroneId (Map ProdId Int)
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

--locDist :: (Int, Int) -> (Int, Int) -> Double
--locDist (a,b) (c,d) = sqrt $ (a - c)^2 + (b - d)^2

isFull :: Int -> Map Int Int -> Int -> Bool
isFull size m k = case Map.lookup k m of
  Nothing -> True
  Just v  -> size <= v



prodsToWeight :: Map ProdId Int -> State Simulation Weight
prodsToWeight m = do
    ps <- gets products
    return $ sum $ map (f ps) $ Map.toList m
  where
    f :: Products -> (ProdId, Int) -> Int
    f ps (pid, no) = (fromJust (Map.lookup pid ps)) * no


getFirstFreeDrone :: Int -> State Simulation (Maybe Int)
getFirstFreeDrone weight = do
  maxSize <- gets sizeDrone
  ds <- gets drones
  droneWeights <- mapM prodsToWeight ds
  let l =   map fst $ filter (\(did,y) -> y+weight <= maxSize) $ Map.toList droneWeights
  return $ listToMaybe l

getFirstFreeDrones :: Weight -> Int -> State Simulation (Maybe [Int])
getFirstFreeDrones weight number = do
  maxSize <- gets sizeDrone
  ds <- gets drones
  droneWeights <- mapM prodsToWeight ds
  return $ map (\(did, w) -> (did, (maxSize - w) `div` weight)) $ Map.toList droneWeights


getWareHouse :: ProdId -> State Simulation (WareHouseId)
getWareHouse prodId = do
    whs <- gets warehouses
    let ls = catMaybes $ map (lookupProd prodId) $ Map.toList whs
    return $ head ls
  where
    lookupProd prodid (whid, wh) = Map.lookup prodid (wProducts wh) >> return whid

updateWareHouse :: WareHouseId -> ProdId -> State Simulation ()
updateWareHouse whid prodid = do
    whs <- gets warehouses
    let wh = fromJust $ Map.lookup whid whs
        ps = wProducts wh
        wh2 = wh { wProducts = Map.update f prodid ps}
        whs2 = Map.insert whid wh2 whs
    modify (\sim -> sim { warehouses = whs2 })
  where
    f 1 = Nothing
    f i = Just $ i - 1

updateDrone :: DroneId -> ProdId -> Int -> State Simulation ()
updateDrone did pid no = do
    ds <- gets drones
    let ps = fromJust $ Map.lookup did ds
        ps2 = Map.alter f pid ps
        ds2 = Map.insert did ps2 ds
    modify (\sim -> sim {drones = ds2})
  where
    f Nothing  = Just no
    f (Just i) = Just $ i + no

addLoadCmd :: DroneId -> WareHouseId -> ProdId -> Int -> State Simulation ()
addLoadCmd did whid pid no = do
  cmds <- gets commands
  let newCmd = Load did whid pid no
  modify (\sim -> sim { commands = cmds ++ [newCmd]})

deliver :: OrderId -> (DroneId, Map ProdId Int) -> State Simulation ()
deliver oid (did, m) = do
  cmds <- gets commands
  let newCmds = map (\(pid, no) -> Deliver did oid pid no) $ Map.toList m
      cmds' = cmds ++ newCmds
  modify (\sim -> sim {commands = cmds'})

calc :: State Simulation ()
calc = gets orders >>= mapM_ handleOrder . Map.toList

handleOrder :: (OrderId, Order) -> State Simulation ()
handleOrder (oid, o) = do
  mapM_ load $ Map.toList $ oProducts o
  sim' <- get
  mapM_ (deliver oid) $ Map.toList $ drones sim'
  clearDrones

clearDrones :: State Simulation ()
clearDrones = do
  ds <- gets drones
  let ds' = fmap (const Map.empty) ds
  modify (\sim -> sim { drones = ds'})

load :: (Int, Int) -> State Simulation ()
load (prodId, 0) = return ()
load (prodId, n) = do
   prodWeight <- gets (fromJust . Map.lookup prodId . products)
   dws <- getFirstFreeDrones prodWeight n
   case dws of
     Nothing -> return ()
     Just dws -> do
       let (_, dnos) = foldl (\(acc, ls) (did, no) -> if acc <= 0 then (acc, ls) else (acc - (min no acc), ls ++ [(did, min no acc)])) (n, []) dws
       let f = \(d,no) -> do
          updateDrone d prodId no
          whid <- getWareHouse prodId -- location
          updateWareHouse whid prodId
          addLoadCmd d whid prodId no
       mapM_ f dnos










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

parseSimulation :: String -> Simulation
parseSimulation str = Simulation (read rows) (read cols) (read drones)
                                 (read deadline) (read sizeOfDrones) warehouses
                                 prods orders [] ds
  where
    (l:noProds:prodTypes:noWarehouses:ls) = lines str
    [rows, cols, drones, deadline, sizeOfDrones] = words l
    prods = Map.fromList $ zip [0..((read noProds)-1)] $ map read (words prodTypes)
    warehouses = Map.fromList $ zip [0..] $ parseWarehouses $ take (2 * (read noWarehouses)) ls
    orders = Map.fromList $ zip [0..] $ parseOrders $ drop (2*(read noWarehouses) +1 ) ls
    ds = Map.fromList $ zip [0..(read drones)-1] (repeat Map.empty)

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
  let output = execState calc input
  toFile (title ++ ".out") (commands output)
