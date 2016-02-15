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
  , dTurnFree :: Int
  , dLastCmd  :: DroneCommand
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

data DroneCommand' = Load WareHouseId ProdId Int
                  | Unload WareHouseId ProdId Int
                  | Deliver OrderId ProdId Int
                  | Wait Int

instance Show DroneCommand' where
  show (Load w p i) = unwords ["L", show w, show p, show i]
  show (Unload w p i) = unwords ["U", show w, show p, show i]
  show (Deliver o p i) = unwords ["D", show o, show p, show i]
  show (Wait i) = unwords ["W", show i]

data DroneCommand = DroneCommand DroneId DroneCommand'

instance Show DroneCommand where
  show (DroneCommand dId cmd) = unwords [show dId, show cmd]

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
cmdTime (DroneCommand dId cmd) = do
  drone <- fromJust . Map.lookup dId <$> gets drones
  case cmd of
    (Load wId _ _) -> do
      wLoc <- wLocation . fromJust . Map.lookup wId <$> gets warehouses
      return . (dId,wLoc,) $ dist wLoc (dLocation drone) + 1
    (Unload wId _ _) -> do
      wLoc <- wLocation . fromJust . Map.lookup wId <$> gets warehouses
      return . (dId,wLoc,)$ dist wLoc (dLocation drone) + 1
    (Deliver oId _ _) -> do
      oLoc <- oLocation . fromJust . Map.lookup oId <$> gets orders
      return . (dId,oLoc,)$ dist oLoc (dLocation drone) + 1
    (Wait i) -> return (dId, dLocation drone, i)

--Command agnostic functions
applyCmd :: DroneCommand -> Simulation Int
applyCmd newCmd = do
  (dId, newLoc, finishTime) <- cmdTime newCmd
  drone <- fromJust . Map.lookup dId <$> gets drones
  let currentTime = dTurnFree drone
  let newDrone = Drone newLoc (finishTime+currentTime) newCmd (dProducts drone)
  ds' <- Map.insert dId newDrone <$> gets drones
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

