 {-# LANGUAGE TupleSections #-}
module Types where

import Debug.Trace
import Control.Monad.State
import Control.Monad.RWS
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

type DroneId = Int
type WareHouseId = Int
type ProdId = Int
type OrderId = Int

type Weight = Int

type Location = (Int, Int)

dist :: Location -> Location -> Int
dist (a,b) (c,d) = ceiling $ sqrt $ fromIntegral $ (a - c)^2 + (b - d)^2

--represents where the drone is "at the moment" i.e. end of its last cmd
data Drone = Drone
  { dLocation :: Location
  , lastCommand :: (Int, DroneCommand)
  , dProducts :: Map ProdId Int
  } deriving Show

data WareHouse = WareHouse
  { wLocation :: Location
  , wProducts :: Map Int Int
  } deriving Show

data Order = Order
  { oLocation :: Location
  , oProducts :: Map Int Int
  } deriving Show

data DroneCommand = Load DroneId WareHouseId ProdId Int
                  | Unload DroneId WareHouseId ProdId Int
                  | Deliver DroneId OrderId ProdId Int
                  | Wait DroneId Int

data SimConfig = SimConfig
   { rows :: Int
   , cols :: Int
   , noDrones :: Int
   , deadline :: Int
   , sizeDrone :: Int
   , products :: Map ProdId Weight
   } deriving Show

data SimState = SimState
  { currTurn :: Int
  , warehouses :: Map WareHouseId WareHouse
  , orders :: Map OrderId Order
  , drones :: Map DroneId Drone
  } deriving Show

type Simulation = RWS SimConfig [DroneCommand] SimState

cmdTime :: DroneCommand -> Simulation (DroneId, Location, Int)
cmdTime (Load dId wId _ _) = do
  drone <- gets drones >>= return . fromJust . Map.lookup dId
  wLoc <- gets warehouses >>= return . wLocation . fromJust . Map.lookup wId
  return . (dId,wLoc,) $ dist wLoc (dLocation drone) + 1
cmdTime (Unload dId wId _ _) = do
  drone <- gets drones >>= return . fromJust . Map.lookup dId
  wLoc <- gets warehouses >>= return . wLocation . fromJust . Map.lookup wId
  return . (dId,wLoc,)$ dist wLoc (dLocation drone) + 1
cmdTime (Deliver dId oId _ _) = do
  drone <- gets drones >>= return . fromJust . Map.lookup dId
  oLoc <- gets orders >>= return . oLocation . fromJust . Map.lookup oId
  return . (dId,oLoc,)$ dist oLoc (dLocation drone) + 1
cmdTime (Wait dId i) = do
  drone <- gets drones >>= return . fromJust . Map.lookup dId
  return (dId, dLocation drone, i)
 
--Command agnostic functions
applyCmd :: DroneCommand -> Simulation Int
applyCmd newCmd = do
  (dId, newLoc, finishTime) <- cmdTime newCmd
  drone <- gets drones >>= return . fromJust . Map.lookup dId
  let currentTime = fst . lastCommand $ drone
  let newDrone = Drone newLoc (finishTime+currentTime, newCmd) (dProducts drone)
  ds' <- gets drones >>= return . Map.insert dId newDrone
  modify (\sim -> sim {drones = ds'})
  return $ finishTime+currentTime

runCmd :: DroneCommand -> Simulation Bool
runCmd newCmd = do
  newTurn <- applyCmd newCmd
  latestNow <- gets currTurn
  deadln <- asks deadline
  modify (\sim -> sim {currTurn = max latestNow newTurn})
  if newTurn <= deadln
    then tell [newCmd] >> return True
    else return False
 ---

instance Show DroneCommand where
  show (Load d w p i) = unwords [show d, "L", show w, show p, show i]
  show (Unload d w p i) = unwords [show d, "U", show w, show p, show i]
  show (Deliver d o p i) = unwords [show d, "D", show o, show p, show i]
  show (Wait d i) = unwords [show d, "W", show i]
