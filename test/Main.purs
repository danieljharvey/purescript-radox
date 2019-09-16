module Test.Main where

import Prelude (class Eq, class Show, Unit, bind, discard, pure, unit, ($), (+), (-), (<>)) 

import Data.Variant (Variant, match)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Timer (setTimeout)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

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
  | HeavenKnowsI'mMiserableNow

derive instance eqDogState :: Eq DogState

instance showDogState :: Show DogState where
  show NotTried = "Not tried"
  show LookingForADog = "Looking for a dog"
  show (FoundADog s) = "Found a dog called " <> s
  show HeavenKnowsI'mMiserableNow 
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
dogReducer { dispatch } action state
  = case action of
      LoadNewDog 
        -> UpdateStateAndRunEffect (state { dog = LookingForADog
                               , waiting = false 
                               }) 
                       (warnAfterTimeout dispatch)

      ApologiesThisDogIsTakingSoLong
        -> case state.dog of
              LookingForADog -> UpdateState $ state { waiting = true }
              _              -> NoOp

      GotNewDog url
        -> UpdateState $ state { dog = (FoundADog url) }

      DogError _
        -> UpdateState $ state { dog = HeavenKnowsI'mMiserableNow } 

warnAfterTimeout
  :: (LiftedAction -> Effect Unit)
  -> Aff Unit
warnAfterTimeout dispatch = 
  liftEffect $ do
     let action = dispatch (lift ApologiesThisDogIsTakingSoLong)
     _ <- setTimeout 200 action
     pure unit

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
                    UpdateState $ countReducer action state
    , dogs:     \action -> 
                    dogReducer dispatch action state
    } action'

main :: Effect Unit
main =
  launchAff_ $ runSpec [consoleReporter] do
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
