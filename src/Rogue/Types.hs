{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Rogue.Types where

import           Control.Lens hiding (Level)
import           Control.Lens.TH (declareLenses,declarePrisms)
import           Data.Array (Array,bounds)
import qualified Data.Map.Strict as Map
import           System.Random (StdGen,split)


type Position = (Int,Int)

data Level = Level (Array Position Cell)
             deriving (Show)

data Seed  = Seed Int Int StdGen
             deriving (Show)

type Depth = Int

type Levels = Array Depth (Either Seed Level)

data Cell = Floor
          | Wall
          | Door
          | Void
            deriving (Eq,Show,Ord)


type Health = Int

class HasHealth a where
  health :: Lens' a Health

declareLenses [d|

  data Weapon = Weapon deriving (Show)

  data Scroll = Scroll deriving (Show)

  |]

declarePrisms [d|

  data ItemInfo = IWeapon Weapon
                | IScroll Scroll
                  deriving (Show)

  |]

declareLenses [d|
  data Item = Item { iName :: String
                   , iInfo :: ItemInfo
                   } deriving (Show)
  |]

type Inventory = [Item]

class HasInventory a where
  inventory :: Lens' a Inventory


posX, posY :: HasPosition a => Lens' a Int
posX = position . _1
posY = position . _2

class HasPosition a where
  position :: Lens' a Position

instance HasPosition Position where
  position = lens id (\_ p -> p)

data Direction = North | East | South | West
                 deriving (Eq,Ord,Show)

move :: HasPosition a => Direction -> a -> a
move North = moveUp
move East  = moveRight
move South = moveDown
move West  = moveLeft

moveUp, moveDown, moveLeft, moveRight :: HasPosition a => a -> a
moveUp    = over posY (subtract 1)
moveDown  = over posY (+ 1)
moveLeft  = over posX (subtract 1)
moveRight = over posX (+ 1)

declareLenses [d|

  data Character = Character { cHealth :: !Health
                             , cPos    :: !Position
                             , cItems  :: !Inventory
                             } deriving (Show)

  instance HasHealth    Character where health   = cHealth
  instance HasPosition  Character where position = cPos
  instance HasInventory Character where inventory = cItems

  data MonsterType = MonsterType { mtName :: String
                                 } deriving (Show)

  data Monster = Monster { mType   :: MonsterType
                         , mHealth :: !Health
                         , mPos    :: !Position
                         , mItems  :: !Inventory
                         } deriving (Show)

  instance HasHealth    Monster where health    = mHealth
  instance HasPosition  Monster where position  = mPos
  instance HasInventory Monster where inventory = mItems

  type MonsterId = Int

  data State = State { sMonsters  :: Map.Map MonsterId Monster
                     , sCharacter :: !Character
                     , sAbove     :: [Level]
                     , sLevel     :: Level
                     , sBelow     :: [Level]
                     , sGen       :: !StdGen
                     } deriving (Show)

  |]

splitGen :: State -> (State,StdGen)
splitGen s = (set sGen l s, r)
  where
  (l,r) = split (view sGen s)
