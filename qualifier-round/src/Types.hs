 {-# LANGUAGE TupleSections #-}
module Types where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Control.Monad.RWS

newtype DroneId = DroneId Int deriving (Eq, Ord)
instance Show DroneId where
  show (DroneId i) = show i
newtype WarehouseId = WarehouseId Int deriving (Eq, Ord)
instance Show WarehouseId where
  show (WarehouseId i) = show i
newtype ProdId = ProdId Int deriving (Eq, Ord)
instance Show ProdId where
  show (ProdId i) = show i
newtype OrderId = OrderId Int deriving (Eq, Ord)
instance Show OrderId where
  show (OrderId i) = show i

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

data Warehouse = Warehouse
  { wLocation :: Location
  , wProducts :: Map ProdId Int
  } deriving Show

data Order = Order
  { oLocation :: Location
  , oTotalNum :: Int
  , oProducts :: Map ProdId Int
  } deriving Show

data DroneCommand' = Load WarehouseId ProdId Int
                  | Unload WarehouseId ProdId Int
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
  { warehouses :: Map WarehouseId Warehouse
  , orders :: Map OrderId Order
  , drones :: Map DroneId Drone
  } deriving Show

getDrone :: DroneId -> Simulation Drone
getDrone i = fromJust . Map.lookup i <$> gets drones
getWh :: WarehouseId -> Simulation Warehouse
getWh i = fromJust . Map.lookup i <$> gets warehouses
getOrder :: OrderId -> Simulation Order
getOrder i = fromJust . Map.lookup i <$> gets orders
getProdWeight :: ProdId -> Simulation Int
getProdWeight i = fromJust . Map.lookup i <$> asks products
type Simulation = RWS SimConfig [DroneCommand] SimState

cmdTime :: DroneCommand -> Simulation (DroneId, Location, Int)
cmdTime (DroneCommand dId cmd) = do
  drone <- fromJust . Map.lookup dId <$> gets drones
  let currentTime = dTurnFree drone
  case cmd of
    (Load wId _ _) -> do
      wLoc <- wLocation <$> getWh wId
      return . (dId,wLoc,) $ dist wLoc (dLocation drone) + 1 + currentTime
    (Unload wId _ _) -> do
      wLoc <- wLocation <$> getWh wId
      return . (dId,wLoc,) $ dist wLoc (dLocation drone) + 1 + currentTime
    (Deliver oId _ _) -> do
      oLoc <- oLocation <$> getOrder oId
      return . (dId,oLoc,) $ dist oLoc (dLocation drone) + 1 + currentTime
    (Wait i) -> return (dId, dLocation drone, i + currentTime)

--Command agnostic functions
applyCmd :: DroneCommand -> Simulation Int
applyCmd newCmd = do
  (dId, newLoc, finishTime) <- cmdTime newCmd
  drone <- getDrone dId
  let newDrone = Drone newLoc finishTime newCmd (dProducts drone)
      addDrone = Map.insert dId newDrone
  modify (\sim -> sim {drones = addDrone (drones sim)})
  return finishTime

runCmd :: DroneCommand -> Simulation Bool
runCmd newCmd = do
  newTurn <- applyCmd newCmd
  deadln <- asks deadline
  if newTurn <= deadln
    then tell [newCmd] >> return True
    else return False
