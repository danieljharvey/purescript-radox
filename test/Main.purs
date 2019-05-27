module Test.Main where

import Prelude (class Eq, class Show, Unit, bind, discard, ($), (+), (-), (<>))

import Data.Variant (Variant, match)
import Effect (Effect)
import Effect.Class (liftEffect)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

import Radox

--- our state
type State
  = { value     :: Int
    , dog       :: DogState
    }

data DogState
  = NotTried
  | LookingForADog
  | FoundADog String
  | CouldNotFindADog

derive instance eqDogState :: Eq DogState

instance showDogState :: Show DogState where
  show NotTried = "Not tried"
  show LookingForADog = "Looking for a dog"
  show (FoundADog s) = "Found a dog called " <> s
  show CouldNotFindADog = "Could not find a dog"

defaultState :: State
defaultState
  = { value     : 0
    , dog       : NotTried
    }

--- reducer 1

data Dogs
  = LoadNewDog
  | GotNewDog String
  | DogError String

instance hasLabelDogs :: HasLabel Dogs "dogs"

dogReducer 
  :: Reducer Dogs State 
dogReducer LoadNewDog s
  = s { dog = LookingForADog }
dogReducer (GotNewDog url) s
  = s { dog = (FoundADog url) }
dogReducer (DogError e) s
  = s { dog = CouldNotFindADog } 

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
rootReducer s action' =
  match
    { counting: \action -> countReducer action s
    , dogs:     \action -> dogReducer action s
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