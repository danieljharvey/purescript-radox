module Radox.Internal.Types where

import Prelude
import Effect (Effect)

type RadoxEffects combinedActionType stateType
  = { dispatch :: combinedActionType -> Effect Unit
    , getState :: Effect stateType
    }

-- | Type for any user-created Reducer function that takes an Action for a specific reducer, the entire state, and returns a new copy of the state
type EffectfulReducer actionType stateType combinedActionType
  =  RadoxEffects combinedActionType stateType
  -> actionType
  -> stateType
  -> Effect stateType

-- | Type for a reducer that does need to trigger any side effects
type Reducer actionType stateType
  =  actionType
  -> stateType
  -> stateType

-- | Type for the user-created Combined Reducer function, that takes a Variant of any action, and pipes it to the correct Reducer function, then returns the new state
type CombinedReducer combinedActionType stateType
  =  RadoxEffects combinedActionType stateType
  -> stateType
  -> combinedActionType
  -> Effect stateType

-- | A Listener is a function that takes the new state and returns Effect Unit (so that it can use it to do something interesting, hopefully)
type Listeners stateType
  = Array (stateType -> Effect Unit)

-- | A Dispatcher is the function that allows different parts of our app to send actions to the reducers and make Things Happen.
type Dispatcher combinedActionType
  = combinedActionType -> Effect Unit

-- | Type of store shared around so that the state can be accessed without needing Effect
type RadoxStore combinedActionType stateType
  = { dispatch :: Dispatcher combinedActionType
    , getState :: Effect stateType
    , state    :: stateType
    }

-- | Typeclass that links any given Action sum type to the label it holds in the Combined Reducer / variant
class HasLabel a (p :: Symbol) | a -> p 
