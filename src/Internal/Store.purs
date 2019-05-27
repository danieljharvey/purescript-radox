module Radox.Internal.Store where

import Prelude (Unit, bind)
import Effect (Effect)
import Effect.Ref (Ref, read, write)
import Data.Traversable (traverse)
import Radox.Internal.Types

-- | This takes our action runs it through the reducers, updates listeners with the result
-- | And then updates the ref with the new value
-- | Note actionType must have have been `lift`-ed into the Variant for use here
update 
  :: forall stateType actionType
   . Ref stateType
  -> Listeners stateType
  -> CombinedReducer actionType stateType
  -> actionType
  -> Effect Unit
update stateRef listeners reducers action = do
  -- read current state
  oldState <- read stateRef
  -- calculate new state
  let newState = reducers oldState action
  -- announce new state to listeners
  _ <- traverse (\f -> f newState) listeners
  -- save new state
  write newState stateRef

-- | Read the current state this is saved in the mutable Ref and returns it
getState
  :: forall stateType 
   . Ref stateType
  -> Effect stateType
getState stateRef = read stateRef
