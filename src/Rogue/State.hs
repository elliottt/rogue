{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ConstraintKinds #-}

module Rogue.State where

import           Rogue.Level (positionValid)
import           Rogue.Types

import           Control.Lens hiding (Level)
import           Control.Lens.TH
                     (declareLenses,declareClassy,makeClassyFor,declarePrisms)
import           Data.Array
import qualified Data.Map.Strict as Map



-- Monsters --------------------------------------------------------------------

monster :: MonsterId -> Traversal' State Monster
monster i = sMonsters . ix i

monsterAt :: MonsterId -> Lens' State (Maybe Monster)
monsterAt i = sMonsters . at i


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
validatePlayer s = positionValid (view sCharacter s) (view sLevel s)

validateMonster :: MonsterId -> State -> Bool
validateMonster i s =
  case view (monsterAt i) s of
    Just m  -> positionValid m (view sLevel s)
    Nothing -> False
