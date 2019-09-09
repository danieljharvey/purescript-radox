module Radox.Internal.Store (update, getState) where

import Prelude (Unit, bind, discard, pure, unit) 
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
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
  -> Effect stateType
  -> CombinedReducer actionType stateType 
  -> actionType
  -> Effect Unit
update stateRef listeners getState' reducers action = do

  -- read current state
  oldState <- read stateRef

  --- create effect functions for the reducers to use
  let passedFuncs = { dispatch: update stateRef listeners getState' reducers
                    , getState: getState'
                    }

  -- calculate new state
  let return = reducers passedFuncs oldState action
      newState = stateFromResponse oldState return
      aff = affFromResponse return

  -- announce new state to listeners
  _ <- traverse (\f -> f newState) listeners

  -- save new state
  write newState stateRef
  
  -- launch side effects 
  launchAff_ aff

-- | calculate new state from response
stateFromResponse
  :: forall stateType
   . stateType
  -> ReducerReturn stateType
  -> stateType
stateFromResponse oldState return
  = case return of
      NoOp -> oldState
      NoEffects state -> state
      WithEffects state _ -> state
      EffectsOnly _ -> oldState

-- | calculate which effects to fire from the response
affFromResponse
  :: forall stateType
   . ReducerReturn stateType
  -> Aff Unit 
affFromResponse return
  = case return of
      NoOp -> pure unit
      NoEffects _ -> pure unit 
      WithEffects _ a -> a
      EffectsOnly a -> a

-- | Read the current state this is saved in the mutable Ref and returns it
getState
  :: forall stateType 
   . Ref stateType
  -> Effect stateType
getState stateRef = read stateRef
