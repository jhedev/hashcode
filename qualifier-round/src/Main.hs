{-# LANGUAGE TupleSections #-}
module Main where

import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Control.Monad.RWS
import System.Environment

import Types
import Parse

getCmdCost :: OrderId -> DroneId -> WarehouseId -> Simulation Int
getCmdCost oId dId whId = do
  oLoc <- oLocation <$> getOrder oId
  wLoc <- wLocation <$> getWh whId
  drone <- getDrone dId
  return $ ((oLoc `dist` wLoc) + (wLoc `dist` dLocation drone)) + dTurnFree drone

getBestCombos :: OrderId
              -> ProdId
              -> Simulation [(DroneId, WarehouseId)]
getBestCombos oId prodId = do
  -- here getWareHouses takes 2 locations but we ignore the corresponding info
  possibleWhs <- fmap (\(a, _, _) -> a) <$> getWareHouses (0, 0) (0, 0) prodId
  droneIds <- Map.keys <$> gets drones
  let combos = concatMap (\d -> map (d,) possibleWhs) droneIds
  let g t@(dId, whId) = (,t) <$> getCmdCost oId dId whId
  ratedCombos <- mapM g combos
  return $ map snd $ sortOn fst ratedCombos


prodsToWeight :: Drone -> Simulation Weight
prodsToWeight d = do
    let m = dProducts d
    ps <- asks products
    return $ sum $ map (f ps) $ Map.toList m
  where
    f :: Map ProdId Weight -> (ProdId, Int) -> Int
    f ps (pid, no) = fromJust (Map.lookup pid ps) * no

getDroneSpace :: Location -> Simulation [(DroneId, Int)]
getDroneSpace oLoc = do
  maxSize <- asks sizeDrone
  drones <- gets drones
  droneWeights <- mapM prodsToWeight drones
  let spaces = getFit maxSize <$> Map.toList droneWeights
  let sorter (dId, _) =
        let freeTurn = dTurnFree . fromJust $ Map.lookup dId drones
            dLoc     = dLocation . fromJust $ Map.lookup dId drones
        in dLoc `dist` oLoc + freeTurn
  return $ sortOn sorter spaces
  where
    getFit maxSize (dId, w) = (dId, maxSize - w)

-- Get "best" warehouse from drone location and order location and product id
-- Only takes `Location`s because of legacy `loadOld`
getWareHouses :: Location
              -> Location
              -> ProdId
              -> Simulation [(WarehouseId, Int, Int)]
getWareHouses loc1 loc2 prodId = do
    whs <- gets warehouses
    let validWarehouses = mapMaybe (getProdInfo prodId) $ Map.toList whs
    case validWarehouses of
      [] -> get >>= error . ("Item missing from warehouse!" ++). show
      _  -> return $ sortOn (\(_, _, whDist) -> whDist) validWarehouses
  where
    getProdInfo prodId (whId, wh) = do -- Maybe monad
      numLeft <- Map.lookup prodId (wProducts wh)
      let distFromLoc = (loc1 `dist` wLocation wh) + (loc2 `dist` wLocation wh)
      return (whId, numLeft, distFromLoc)

-- Destock or stock a warehouse
updateWareHouse :: WarehouseId -> ProdId -> Int -> Simulation ()
updateWareHouse whId prodId no = do
    whs <- gets warehouses
    let wh = fromJust $ Map.lookup whId whs
        ps = wProducts wh
        wh2 = wh { wProducts = Map.update f prodId ps}
        whs2 = Map.insert whId wh2 whs
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

load :: OrderId -> (ProdId, Int) -> Simulation [DroneCommand]
load _   (_, 0) = return []
load oId (prodId, n) = do
   prodWeight <- asks (fromJust . Map.lookup prodId . products)
   combos <- getBestCombos oId prodId
   (numLoaded, loadCmd) <- takeFirstValid prodWeight combos
   loads <- load oId (prodId, n-numLoaded)
   return $ loadCmd:loads
  where
    takeFirstValid _          [] = error "There are never any drones free, wtf?"
    takeFirstValid prodWeight (c:cs) = do
      (numLoaded, load) <- getDroneDistrib prodWeight c
      if numLoaded <= 0
         then takeFirstValid prodWeight cs
         else return (numLoaded, load)
    getDroneDistrib prodWeight (dId, whId) = do
      maxSize <- asks sizeDrone
      occupiedSpace <- getDrone dId >>= prodsToWeight
      let freeSpace = maxSize - occupiedSpace
      let f wh = return $ case Map.lookup prodId (wProducts wh) of
            Nothing -> 0
            Just i  -> i
      numInWh <- getWh whId >>= f
      let numPossible = min (freeSpace `div` prodWeight) numInWh
          numProds = min n numPossible
      if numProds <= 0
        then return (0, undefined)
        else do
        updateDrone dId prodId numProds
        updateWareHouse whId prodId numProds
        let newCmd = DroneCommand dId $ Load whId prodId numProds
        success <- runCmd newCmd
        return (numProds, newCmd)

--Inferior method
loadOld :: Location -> (ProdId, Int) -> Simulation ()
loadOld _   (_, 0) = return ()
loadOld loc (prodId, n) = do
   prodWeight <- getProdWeight prodId
   droneFreeSpaces <- getDroneSpace loc
   let totalWeight = prodWeight*n
   case droneFreeSpaces of
     [] -> return ()
     _  ->
       let init = (totalWeight, [])
           (_, dnos) = foldl (getDroneDistrib prodWeight) init droneFreeSpaces
       in mapM_ loadDrone $ filter ((> 0) . snd) dnos
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
      else let actualLoaded = min acc numLeft
           in (acc - actualLoaded, (whId, actualLoaded):ls)
    loadDrone (dId, numProds) = do
      dLoc <- dLocation <$> getDrone dId
      updateDrone dId prodId numProds
      whIds <- getWareHouses loc dLoc prodId
      let (_, whs) = foldl getWHDistrib (numProds, []) whIds
      mapM (loadDroneFromWh dId) whs
    loadDroneFromWh dId (whId, numProds) = do
      updateWareHouse whId prodId numProds
      let newCmd = DroneCommand dId $ Load whId prodId numProds
      void $ runCmd newCmd

deliver :: OrderId -> (DroneId, Drone) -> Simulation ()
deliver oid (dId, d) = do
  let mkCmd (pid, no) = DroneCommand dId (Deliver oid pid no)
  let newCmds = map mkCmd $ Map.toList $ dProducts d
  mapM_ runCmd newCmds
  clearDrones

handleOrder :: (OrderId, Order) -> Simulation ()
handleOrder (oId, o) = do
  mapM_ (load oId) $ Map.toList $ oProducts o
  get >>= mapM_ (deliver oId) . Map.toList . drones

releaseTheDrones :: Simulation ()
releaseTheDrones = do
  orders <- Map.toList <$> gets orders
  mapM_ handleOrder $ sortOn sorter orders
  where
  sorter (_, o) = let total = oTotalNum o
                      numTypes = length $ Map.keys $ oProducts o
                  in fromIntegral total * (1.5 ^^ fromIntegral numTypes)


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
