{-# LANGUAGE TupleSections #-}
module Main where

import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import System.Environment

import Types
import Debug.Trace





prodsToWeight :: Map ProdId Int -> State Simulation Weight
prodsToWeight m = do
    ps <- gets products
    return $ sum $ map (f ps) $ Map.toList m
  where
    f :: Products -> (ProdId, Int) -> Int
    f ps (pid, no) = fromJust (Map.lookup pid ps) * no


getFirstFreeDrone :: Int -> State Simulation (Maybe Int)
getFirstFreeDrone weight = do
  maxSize <- gets sizeDrone
  ds <- gets drones
  droneWeights <- mapM prodsToWeight ds
  return $ listToMaybe $ fits maxSize droneWeights
  where
    checkFit maxSize (_, y) = y + weight <= maxSize
    fits maxSize = map fst . filter (checkFit maxSize) . Map.toList

getDroneSpace :: State Simulation [(DroneId, Int)]
getDroneSpace = do
  maxSize <- gets sizeDrone
  droneWeights <- gets drones >>= mapM prodsToWeight
  return $ fmap (getFit maxSize) $ Map.toList droneWeights
  where
    getFit maxSize (dId, w) = (dId, maxSize - w)


getWareHouse :: Location -> ProdId -> State Simulation WareHouseId
getWareHouse loc prodId = do
    whs <- gets warehouses
    let validWarehouses = mapMaybe (lookupProd prodId) $ Map.toList whs
    case validWarehouses of
      [] -> do
        sim <- get
        error (show sim ++ "\nItem missing!")
      _  -> return . fst $ minimumDist validWarehouses
  where
    lookupProd prodid (whId, wh) =
      Map.lookup prodid (wProducts wh)
      >> (return . (whId, ) . dist loc . wLocation) wh
    minimumDist [] = maxBound
    minimumDist ((whId, wDist):ws) =
      let (whId', wDist') = minimumDist ws
      in if wDist' < wDist then (whId', wDist') else (whId, wDist)

updateWareHouse :: WareHouseId -> ProdId -> State Simulation ()
updateWareHouse whid prodid = do
    whs <- gets warehouses
    let wh = trace "here" $ traceShow whid $ fromJust $ Map.lookup whid whs
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
    let d = fromJust $ Map.lookup did ds
        ps2 = Map.alter f pid (dProducts d)
        ds2 = Map.insert did (d {dProducts = ps2}) ds
    modify (\sim -> sim {drones = ds2})
  where
    f Nothing  = Just no
    f (Just i) = Just $ i + no

addLoadCmd :: DroneId -> WareHouseId -> ProdId -> Int -> State Simulation DroneCommand
addLoadCmd did whid pid no = do
  let newCmd = Load did whid pid no
  modify (\sim -> sim { commands = newCmd: commands sim})
  return newCmd

deliver :: OrderId -> (DroneId, Drone) -> State Simulation ()
deliver oid (did, m) = do
  cmds <- gets commands
  let newCmds = map (\(pid, no) -> Deliver did oid pid no) $ Map.toList m
      cmds' = cmds ++ newCmds
  modify (\sim -> sim {commands = cmds'})

releaseTheDrones :: State Simulation ()
releaseTheDrones = gets orders >>= mapM_ handleOrder . Map.toList

handleOrder :: (OrderId, Order) -> State Simulation ()
handleOrder (oid, o) = do
  trace ("Order: " ++ show oid) $ return ()
  let oLoc = oLocation o
  lCmds <- mapM (load oLoc) $ Map.toList $ oProducts o
  mapM_ orderDrone lCmds
  let f cmd@(Load dId _ _ _) = do
        drones <- gets drones
        let dLoc = dLocation $ fromJust $ Map.lookup dId drones
        cmdTime dLoc cmd
      f cmd@(Deliver dId _ _ _) = do
        drones <- gets drones
        let dLoc = dLocation $ fromJust $ Map.lookup dId drones
        cmdTime dLoc cmd
  lTimes <- mapM (sum . map f) lCmds
  dCmds <- get >>= mapM (deliver oid) . Map.toList . drones
  dTimes <- mapM (sum . map f) dCmds
  mapM_ orderDrone dCmds
  let maxTime = maximum $ zipWith (+) lTimes dTimes
  return maxTime
  --here I wanted to just get the maximum amount taken

--better would be just to "execute" the command on the state
--each drone has a list of commands maybe?
orderDrone :: DroneCommand -> State Simulation ()
orderDrone = do
  ds <- gets drones
  let ds' = Map.map (const $ Drone loc Map.empty) ds
  modify (\sim -> sim { drones = ds'})

load :: Location -> (Int, Int) -> State Simulation [DroneCommand]
load _   (_, 0) = return 0
load loc (prodId, n) = do
   prodWeight <- gets (fromJust . Map.lookup prodId . products)
   let totalWeight = prodWeight*n
   droneSpaces <- getDroneSpace
   case droneSpaces of
     [] -> return 0
     _  ->
       let dnos = foldl (getLoadDistrib prodWeight) (totalWeight, []) droneSpaces in
       mapM loadDrone $ filter ((> 0) . snd) . snd $ dnos
  where
    loadDrone (d, numProds) = do
      updateDrone d prodId numProds
      whid <- getWareHouse loc prodId
      updateWareHouse whid prodId
      addLoadCmd d whid prodId numProds
    getLoadDistrib prodWeight (acc, ls) (dId, freeSpace) =
      if acc <= 0
      then (acc, ls)
      else
        let numProds = min acc freeSpace `div` prodWeight
        in (acc - prodWeight*numProds, (dId, numProds):ls)
    -- here one might consider not greedily loading every done full
    -- for example if we want to use Unload
    -- also we could find the warehouse and drone at the same time


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
  let output = execState releaseTheDrones input
  toFile (title ++ ".out") (commands output)
