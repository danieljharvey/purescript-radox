module Radox.Internal.CreateStore where

import Prelude (bind, pure, unit, ($))
import Effect (Effect)
import Effect.Ref (new)
import Data.Variant (SProxy(..), Variant, inj)
import Data.Symbol (class IsSymbol)
import Prim.Row (class Cons)
import Radox.Internal.Types
import Radox.Internal.Store (getState, update)

-- | This creates a new Radox store
-- | 
-- | `initialState` is how our state looks when we start
-- |
-- | `listeners` are an array of stateType -> Effect Unit functions that will be
-- | sent the new state everything it is updated
-- |
-- | 'rootReducer` is the function that takes our `actionType` and a `stateType` and returns the new `stateType`
-- |
-- | This returns a RadoxStore, which has a `dispatch` function for sending new actions
-- | and a `getState` function 
createStore 
  :: forall stateType actionType
   . stateType
  -> Listeners stateType
  -> CombinedReducer actionType stateType 
  -> Effect (RadoxStore actionType stateType)
createStore initialState listeners rootReducer = do
  stateRef <- new initialState
  let getState' = getState stateRef
  pure $ { dispatch: update stateRef listeners getState' rootReducer
         , getState: getState'
         , state: initialState
         }

-- | When we `dispatch` an action, we need to first lift it into our main Variant type
-- | Thus, to dispatch an Up action we'd do `dispatch (lift Up)`
lift
  :: forall label action dunno r. 
  Cons label action dunno r 
  => HasLabel action label 
  => IsSymbol label 
  => action 
  -> Variant r
lift  
  = inj (SProxy :: SProxy label) 

-- | Things like React Context require us to provide a starting value, this allows us to pass one
-- | without using Effect.
emptyStore 
  :: forall stateType actionType 
   . stateType 
   -> RadoxStore actionType stateType
emptyStore initial
  = { dispatch: \_ -> pure unit
    , getState: pure initial
    , state: initial
    }
