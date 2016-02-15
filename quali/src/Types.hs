module Types where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

type DroneId = Int
type WareHouseId = Int
type ProdId = Int
type OrderId = Int

type Weight = Int

type Products = Map ProdId Weight

type Location = (Int, Int)

dist :: Location -> Location -> Int
dist (a,b) (c,d) = round $ sqrt $ fromIntegral $ (a - c)^2 + (b - d)^2

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
  , drones :: Map DroneId Drone
  } deriving Show

data Drone = Drone
  { dLocation :: Location
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

--use dId to get Location
cmdTime :: Location -> DroneCommand -> State Simulation Int
cmdTime loc (Load dId wId _ _) = do
  sim <- get
  let whs = warehouses sim
      wLoc = wLocation . fromJust . Map.lookup wId $ whs
  return $ dist wLoc loc
cmdTime loc (Unload dId wId _ _) = do
   sim <- get
   let whs = warehouses sim
       wLoc = wLocation . fromJust . Map.lookup wId $ whs
   return $ dist wLoc loc
cmdTime loc (Deliver dId oId _ _) = do
   sim <- get
   let ors = orders sim
       oLoc = oLocation . fromJust . Map.lookup oId $ ors
   return $ dist oLoc loc
cmdTime _ (Wait _ i) = return i

instance Show DroneCommand where
  show (Load d w p i) = unwords [show d, "L", show w, show p, show i]
  show (Unload d w p i) = unwords [show d, "U", show w, show p, show i]
  show (Deliver d o p i) = unwords [show d, "D", show o, show p, show i]
  show (Wait d i) = unwords [show d, "W", show i]
