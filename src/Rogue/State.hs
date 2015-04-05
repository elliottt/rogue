{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ConstraintKinds #-}

module Rogue.State where

import           Control.Lens
import           Control.Lens.TH
                     (declareLenses,declareClassy,makeClassyFor,declarePrisms)
import           Data.Array
import qualified Data.Map.Strict as Map


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


type Position = (Int,Int)

posX, posY :: HasPosition a => Lens' a Int
posX = position . _1
posY = position . _2

class HasPosition a where
  position :: Lens' a Position

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

  type Level = Array (Int,Int) Cell

  data Cell = Floor
            | Wall
            | Door
            | Void
              deriving (Eq,Show,Ord)


  data State = State { sMonsters  :: Map.Map MonsterId Monster
                     , sCharacter :: Character
                     , sAbove     :: [Level]
                     , sLevel     :: Level
                     , sBelow     :: [Level]
                     } deriving (Show)

  |]


-- Monsters --------------------------------------------------------------------

monster :: MonsterId -> Traversal' State Monster
monster i = sMonsters . ix i

monsterAt :: MonsterId -> Lens' State (Maybe Monster)
monsterAt i = sMonsters . at i


-- Movement --------------------------------------------------------------------

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


-- Damage ----------------------------------------------------------------------

type Damage = Int

applyDamage :: HasHealth a => Damage -> a -> a
applyDamage damage = over health (subtract damage)

isDead :: HasHealth a => a -> Bool
isDead a = view health a <= 0


-- Items -----------------------------------------------------------------------

addItem :: HasInventory a => Item -> a -> a
addItem i = over inventory (i :)


-- Input -----------------------------------------------------------------------

data Command = Move Direction
               deriving (Show)

-- | Process a single player command, and update the state.
playerCommand :: Command -> State -> State
playerCommand (Move dir) s = over sCharacter  (move dir) s

-- | Process a command for a specific monster, and update the state.
monsterCommand :: MonsterId -> Command -> State -> State
monsterCommand i (Move dir) s = over (monster i) (move dir) s


-- Validation ------------------------------------------------------------------

validatePlayer :: State -> Bool
validatePlayer s = positionValid (view sCharacter s) s

validateMonster :: MonsterId -> State -> Bool
validateMonster i s =
  case view (monsterAt i) s of
    Just m  -> positionValid m s
    Nothing -> False


-- Levels ----------------------------------------------------------------------

passable :: Cell -> Bool
passable Floor = True
passable Door  = True
passable _     = False

positionValid :: HasPosition pos => pos -> State -> Bool
positionValid pos s =
  and [ inRange (bounds level) p
      , passable (level ! p)
      ]
  where
  p     = view position pos
  level = view sLevel s


-- Testing ---------------------------------------------------------------------

test = State Map.empty (Character 100 (0,0) [])
             []
             (listArray ((0,0),(1,1)) [ Floor, Wall, Floor, Wall ])
             []
