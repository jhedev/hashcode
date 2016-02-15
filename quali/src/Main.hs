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

getCmdCost :: OrderId -> DroneId -> WareHouseId -> Simulation Int
getCmdCost oId dId whId = do
  oLoc <- gets (oLocation . fromJust . Map.lookup oId . orders)
  wLoc <- gets (wLocation . fromJust . Map.lookup whId . warehouses)
  drone <- gets (fromJust . Map.lookup dId . drones)
  return $ ((oLoc `dist` wLoc) + (wLoc `dist` dLocation drone)) + dTurnFree drone

getBestCombos :: OrderId
              -> ProdId
              -> Simulation [(Int, (DroneId, WareHouseId))]
getBestCombos oId prodId = do
  possibleWhs <- fmap (\(_, _, z) -> z) <$> getWareHouses (0, 0) (0, 0) prodId
  droneIds <- Map.keys <$> gets drones
  let combos = concatMap (\d -> map (d,) possibleWhs) droneIds
  let g t@(dId, whId) = fmap (,t) $ getCmdCost oId dId whId
  ratedCombos <- mapM g combos
  return $ sortOn fst ratedCombos


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
  let spaces = getFit maxSize <$> Map.toList droneWeights
  return $ sortOn (\(dId, space) -> dTurnFree . fromJust $ Map.lookup dId drones) spaces
  where
    getFit maxSize (dId, w) = (dId, maxSize - w)

-- Get "best" warehouse from drone location and order location and product id
getWareHouses :: Location -> Location -> ProdId -> Simulation [(WareHouseId, Int, Int)]--  [(WareHouseId, Int, Int)]
getWareHouses loc1 loc2 prodId = do
    whs <- gets warehouses
    let validWarehouses = mapMaybe (getProdInfo prodId) $ Map.toList whs
    case validWarehouses of
      [] -> get >>= error . ("Item missing from warehouse!" ++). show
      _  -> return $ sortOn (\(whId, numLeft, whDist) -> whDist) validWarehouses
       --_  -> return validWarehouses
  where
    getProdInfo prodId (whId, wh) = do -- Maybe monad
      numLeft <- Map.lookup prodId (wProducts wh)
      let distFromLoc = dist loc1 (wLocation wh) + dist loc2 (wLocation wh)
      return (whId, numLeft, distFromLoc)

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
    f i | i - no > 0 = Just $ i - no
        | i - no < 0 = error "Item not found in warehouse!"
        | otherwise  = Nothing

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

loadForLoc' :: OrderId -> (ProdId, Int) -> Simulation [DroneCommand]
loadForLoc' _   (_, 0) = return []
loadForLoc' oId (prodId, n) = do
   prodWeight <- asks (fromJust . Map.lookup prodId . products)
   combos <- fmap (map snd) $ getBestCombos oId prodId
   let totalWeight = prodWeight*n
   case combos of
     [] -> return []
     _  -> do
       (_, loads) <- foldM (getDroneDistrib prodWeight) (totalWeight, []) combos
       return loads
  where
    getDroneDistrib prodWeight (acc, ls) (dId, whId) = do
      maxSize <- asks sizeDrone
      occupiedSpace <- gets drones >>= prodsToWeight . fromJust . Map.lookup dId
      let freeSpace = maxSize - occupiedSpace
      let f wh = return $ case Map.lookup prodId (wProducts wh) of
            Nothing -> 0
            Just i  -> i
      numInWh <- gets warehouses >>= f . fromJust . Map.lookup whId
      let numPossible = min (freeSpace `div` prodWeight) numInWh
          numProds = min (acc `div` prodWeight) numPossible
      if numProds <= 0
        then return (0, ls)
        else do
        updateDrone dId prodId numProds
        updateWareHouse whId prodId numProds
        let newCmd = DroneCommand dId $ Load whId prodId numProds
        success <- runCmd newCmd
        return (acc - prodWeight*numProds, newCmd:ls)

loadForLoc :: OrderId -> (ProdId, Int) -> Simulation [DroneCommand]
loadForLoc _   (_, 0) = return []
loadForLoc oId (prodId, n) = do
   loc <- (oLocation . fromJust . Map.lookup oId) <$> gets orders
   prodWeight <- asks (fromJust . Map.lookup prodId . products)
   droneFreeSpaces <- getDroneSpace
   let totalWeight = prodWeight*n
   case droneFreeSpaces of
     [] -> return []
     _  ->
       let (_, dnos) = foldl (getDroneDistrib prodWeight) (totalWeight, []) droneFreeSpaces in
       fmap concat $ mapM loadDrone $ filter ((> 0) . snd) dnos
  where
    getDroneDistrib prodWeight (acc, ls) (dId, freeSpace) =
      if acc <= 0
      then (acc, ls)
      else
        let numProds = min acc freeSpace `div` prodWeight
        in (acc - prodWeight*numProds, (dId, numProds):ls)
    getWHDistrib (acc, ls) (whId, numLeft, _) =
      if acc <= 0
      then (acc, ls)
      else (acc - min acc numLeft, (whId, min acc numLeft):ls)
    loadDrone (dId, numProds) = do
      dLoc <- dLocation . fromJust . Map.lookup dId <$> gets drones
      updateDrone dId prodId numProds
      loc <- (oLocation . fromJust . Map.lookup oId) <$> gets orders
      whIds <- getWareHouses loc dLoc prodId
      let (_, whs) = foldl getWHDistrib (numProds, []) whIds
      mapM (loadDroneFromWh dId) whs
    loadDroneFromWh dId (whId, numProds) = do
       updateWareHouse whId prodId numProds
       let newCmd = DroneCommand dId $ Load whId prodId numProds
       success <- runCmd newCmd
       return newCmd
   -- here one might consider not greedily loading every done full
   -- for example if we want to use Unload
   -- also we could find the warehouse and drone at the same time

deliver :: OrderId -> (DroneId, Drone) -> Simulation ()
deliver oid (dId, d) = do
  let newCmds = map (\(pid, no) -> DroneCommand dId $ Deliver oid pid no) $ Map.toList $ dProducts d
  mapM_ runCmd newCmds
  clearDrones

handleOrder :: (OrderId, Order) -> Simulation ()
handleOrder (oId, o) = do
  let oLoc = oLocation o
  lCmds <- mapM (loadForLoc oId) $ Map.toList $ oProducts o
  dCmds <- get >>= mapM (deliver oId) . Map.toList . drones
  return ()

releaseTheDrones :: Simulation ()
releaseTheDrones = gets orders >>= mapM_ handleOrder . sortOn (oTotalNum . snd) . Map.toList


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
