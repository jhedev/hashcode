{-# LANGUAGE TupleSections #-}
module Main where

import Data.List (sortOn)
import Debug.Trace
import Control.Monad.RWS

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import System.Environment

import Types
import Parse


prodsToWeight :: Drone -> Simulation Weight
prodsToWeight d = do
    let m = dProducts d
    ps <- asks products
    return $ sum $ map (f ps) $ Map.toList m
  where
    f :: Map ProdId Weight -> (ProdId, Int) -> Int
    f ps (pid, no) = fromJust (Map.lookup pid ps) * no


getDroneSpace :: Simulation [(DroneId, Int)]
getDroneSpace = do
  maxSize <- asks sizeDrone
  drones <- gets drones
  droneWeights <- mapM prodsToWeight drones
  let spaces = fmap (getFit maxSize) $ Map.toList droneWeights
  return $ sortOn (\(dId, space) -> fst . lastCommand . fromJust $ Map.lookup dId drones) spaces
  where
    getFit maxSize (dId, w) = (dId, maxSize - w)

-- Get "best" warehouse from location and product id
getWareHouses :: Location -> ProdId -> Simulation [(WareHouseId, Int, Int)]
getWareHouses loc prodId = do
    whs <- gets warehouses
    let validWarehouses = mapMaybe (lookupProd prodId) $ Map.toList whs
    case validWarehouses of
      [] -> get >>= error . ("Item missing from warehouse!" ++). show
      _  -> return validWarehouses
      --_  -> return . fst $ minimumDist validWarehouses
  where
    lookupProd prodid (whId, wh) =
      Map.lookup prodid (wProducts wh)
      >>= \numLeft -> (return . (whId,numLeft,) . dist loc . wLocation) wh
    minimumDist [] = maxBound
    minimumDist ((whId, wDist):ws) =
      let (whId', wDist') = minimumDist ws
      in if wDist' < wDist then (whId', wDist') else (whId, wDist)

-- Destock or stock a warehouse
updateWareHouse :: WareHouseId -> ProdId -> Int -> Simulation ()
updateWareHouse whid prodid no = do
    whs <- gets warehouses
    let wh = fromJust $ Map.lookup whid whs
        ps = wProducts wh
        wh2 = wh { wProducts = Map.update f prodid ps}
        whs2 = Map.insert whid wh2 whs
    modify (\sim -> sim { warehouses = whs2 })
  where
    f i = if i - no > 0
          then Just $ i - no
          else if i - no < 0 then error "Missing item" else Nothing

-- Load or unload a drone
updateDrone :: DroneId -> ProdId -> Int -> Simulation ()
updateDrone did pid no = do
    ds <- gets drones
    let d = fromJust $ Map.lookup did ds
        ps2 = Map.alter f pid (dProducts d)
        ds2 = Map.insert did (d {dProducts = ps2}) ds
    modify (\sim -> sim {drones = ds2})
  where
    f Nothing  = Just no
    f (Just i) = Just $ i + no

clearDrones :: Simulation ()
clearDrones = do
  ds <- gets drones
  let ds' = Map.map (\d -> d {dProducts = Map.empty}) ds
  modify (\sim -> sim { drones = ds'})

loadToLoc :: Location -> (ProdId, Int) -> Simulation [DroneCommand]
loadToLoc _   (_, 0) = return []
loadToLoc loc (prodId, n) = do
   prodWeight <- asks (fromJust . Map.lookup prodId . products)
   droneFreeSpaces <- getDroneSpace
   let totalWeight = prodWeight*n
   case droneFreeSpaces of
     [] -> return []
     _  ->
       let (_, dnos) = foldl (getLoadDistrib prodWeight) (totalWeight, []) droneFreeSpaces in
       fmap concat $ mapM loadDrone $ filter ((> 0) . snd) $ dnos
  where
    loadDrone :: (DroneId, Int) -> Simulation [DroneCommand]
    loadDrone (dId, numProds) = do
      updateDrone dId prodId numProds
      whIds <- getWareHouses loc prodId
      let (_, whs) = foldl getWHDistrib (numProds, []) whIds
      mapM (loadDroneFromWh dId) whs
    loadDroneFromWh dId (whId, numProds) = do
       updateWareHouse whId prodId numProds
       let newCmd = Load dId whId prodId numProds
       success <- runCmd newCmd
       return newCmd
    getLoadDistrib prodWeight (acc, ls) (dId, freeSpace) =
      if acc <= 0
      then (acc, ls)
      else
        let numProds = min acc freeSpace `div` prodWeight
        in (acc - prodWeight*numProds, (dId, numProds):ls)
    getWHDistrib (acc, ls) (whId, numLeft, _) =
      if acc <= 0
      then (acc, ls)
      else (acc - min acc numLeft, (whId, min acc numLeft):ls)
   -- here one might consider not greedily loading every done full
   -- for example if we want to use Unload
   -- also we could find the warehouse and drone at the same time

deliver :: OrderId -> (DroneId, Drone) -> Simulation ()
deliver oid (dId, d) = do
  let newCmds = map (\(pid, no) -> Deliver dId oid pid no) $ Map.toList $ dProducts d
  mapM_ runCmd newCmds
  clearDrones

handleOrder :: (OrderId, Order) -> Simulation ()
handleOrder (oid, o) = do
  let oLoc = oLocation o
  lCmds <- mapM (loadToLoc oLoc) $ Map.toList $ oProducts o
  dCmds <- get >>= mapM (deliver oid) . Map.toList . drones
  return ()

releaseTheDrones :: Simulation ()
releaseTheDrones = gets orders >>= mapM_ handleOrder . Map.toList


--MAIN
outputToString :: [DroneCommand] -> String
outputToString cmds = unlines $ show (length cmds) : map show cmds

fromFile :: FilePath -> IO (SimConfig, SimState)
fromFile fp = do
  content <- readFile fp
  return $ parseSimulation content

toFile :: FilePath -> [DroneCommand] -> IO ()
toFile fp = writeFile fp . outputToString

main :: IO ()
main = do
  files <- getArgs
  forM_ files $ \title -> do
    (conf, initState) <- fromFile (title ++ ".in")
    print conf
    print initState
    let (_, commands) = evalRWS releaseTheDrones conf initState
    toFile (title ++ ".out") commands
