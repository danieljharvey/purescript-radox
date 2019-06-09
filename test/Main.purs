module Test.Main where

import Prelude (class Eq, class Show, Unit, bind, discard, pure, ($), (+), (-), (<>))

import Data.Variant (Variant, match)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Timer (setTimeout)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

import Radox

--- our state
type State
  = { value     :: Int
    , dog       :: DogState
    , waiting   :: Boolean
    }

data DogState
  = NotTried
  | LookingForADog
  | FoundADog String
  | HeavenKnowsImMiserableNow

derive instance eqDogState :: Eq DogState

instance showDogState :: Show DogState where
  show NotTried = "Not tried"
  show LookingForADog = "Looking for a dog"
  show (FoundADog s) = "Found a dog called " <> s
  show HeavenKnowsImMiserableNow 
    = "Look, I'm not condoning any of the nonsense he bangs on about now by any measure."

defaultState :: State
defaultState
  = { value     : 0
    , dog       : NotTried
    , waiting   : false
    }

--- reducer 1

data Dogs
  = LoadNewDog
  | ApologiesThisDogIsTakingSoLong
  | GotNewDog String
  | DogError String

instance hasLabelDogs :: HasLabel Dogs "dogs"

dogReducer 
  :: EffectfulReducer Dogs State LiftedAction 
dogReducer { dispatch, getState } action state
  = case action of
      LoadNewDog 
        -> do
          _ <- setTimeout 20000 $ dispatch $ lift $ ApologiesThisDogIsTakingSoLong
          pure $ state { dog = LookingForADog
                       , waiting = false 
                       }
      ApologiesThisDogIsTakingSoLong
        -> do
           currentState <- getState
           case currentState.dog of
              LookingForADog -> pure $ state { waiting = true }
              _              -> pure state
      GotNewDog url
        -> pure $ state { dog = (FoundADog url) }
      DogError _
        -> pure $ state { dog = HeavenKnowsImMiserableNow } 

--- reducer 2

data Counting
  = Up
  | Down

instance hasLabelCounting :: HasLabel Counting "counting"

countReducer 
  :: Reducer Counting State
countReducer Up s
  = s { value = s.value + 1 }
countReducer Down s
  = s { value = s.value - 1 }

--- our action type

type LiftedAction 
  = Variant ( counting :: Counting
            , dogs :: Dogs
            )

rootReducer 
  :: CombinedReducer LiftedAction State 
rootReducer dispatch state action' =
  match
    { counting: \action -> 
                    pure $ countReducer action state
    , dogs:     \action -> 
                    dogReducer dispatch action state
    } action'

main :: Effect Unit
main =
  run [consoleReporter] do
    describe "Radox" do
      it "Increments counter twice" $ do
        radox <- liftEffect $ createStore defaultState [] rootReducer
        liftEffect $ radox.dispatch (lift Up)
        liftEffect $ radox.dispatch (lift Up)
        state <- liftEffect $ radox.getState
        state.value `shouldEqual` 2
      it "Increments counter once and loads some dogs" $ do
        radox <- liftEffect $ createStore defaultState [] rootReducer
        liftEffect $ radox.dispatch (lift Up)
        liftEffect $ radox.dispatch (lift $ GotNewDog "dog")
        state <- liftEffect $ radox.getState
        state.value `shouldEqual` 1
        state.dog `shouldEqual` FoundADog "dog"
