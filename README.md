# Purescript Radox

## It's Like Redux, But Even Cleaner!

Purescript Radox is a small state management library designed to plug into things like Purescript React. It uses [variants](https://github.com/natefaubion/purescript-variant) to help us keep all our different reducers simple to work with whilst maintaining that sweet type safety.

#### OK. That's painfully vague, could I perhaps have an example?

Let's look at a simple implementation with two reducers, to show how it works.

1. Here is our important application state. It tells us about dogs we may or may not have found, and contains a number.

```haskell
import Prelude
import Radox

data DogState
  = NotTried
  | LookingForADog
  | FoundADog String
  | HeavenKnowsImMiserableNow

type State
  = { value     :: Int
    , dog       :: DogState
    , waiting   :: Boolean
    }

defaultState :: State
defaultState
  = { value     : 0
    , dog       : NotTried
    , waiting   : false
    }
```

2. We'd like to be able to change this data in a nice clean way. Here is how we'd express the arbitrary counting that we'd like to do. Hopefully if you've used Elm or Redux this should seem pretty familiar to you.

A `Reducer` function has the type `action -> state -> state`.

```haskell
data CountingAction
  = Up
  | Down

instance hasLabelCountingAction :: HasLabel CountingAction "counting"

countingReducer 
  :: Reducer CountingAction State
countingReducer action state
  = case action of
      Up 
        -> state { value = state.value + 1 }
      Down 
        -> state { value = state.value - 1 }
```

(As is hopefully clear, `CountingAction` is a sum type of the different counting-related events that may happen, and `countingReducer` is a function that actions them onto the `State`.)

3. We'd like to be able to change the dog portions of this data. But wait! We also need some effectful things to happen in the Real World when this is happening. Here is how we'd express the dog-based logic changes.

```haskell
data Dogs
  = LoadNewDog
  | ApologiesThisDogIsTakingSoLong
  | GotNewDog String
  | DogError String

instance hasLabelDogs :: HasLabel Dogs "dogs"

dogReducer 
  :: EffectfulReducer Dogs State AnyAction 
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
```

An `EffectfulReducer` is similar to `Reducer`, but has the type signature
`(RadoxEffects state action) -> action -> state -> Effect state`.

`RadoxEffects` is a record containing useful functions to use in reducers, and
has the following type:

```
type RadoxEffects state action
  = { dispatch :: (action -> Effect Unit)
    , getState :: Effect state
    }
```

This means that our effectful reducer is able to inspect the current state at
any point, and dispatch further actions.


4. We can now make a combined reducer that works on all of these at once. First we create a type that contains all our action types:

```haskell
type AnyAction 
  = Variant ( counting :: Counting
            , dogs :: Dogs
            )
```

(`Variant` comes from the [variant](https://github.com/natefaubion/purescript-variant) package btw)

5. Now we use `match` from `variant` to pipe the actions to the right place:

```haskell
rootReducer 
  :: CombinedReducer LiftedAction State 
rootReducer dispatch state action' =
  match
    { counting: \action -> 
                    pure $ countReducer action state
    , dogs:     \action -> 
                    dogReducer dispatch action state
    } action'
```

(Our `CombinedReducer` type must return an `Effect` type - hence it adds `pure`
to the regular `Reducer` of `counting`, but does not need to for the already effectful
`EffectfulReducer` of `dogs`.)

6. That's all quite nice - but let's actually save the outcome as well. Let's create a Radox store and use it.

```haskell
import Effect
import Effect.Console (logShow, log)

makeStore :: Effect Unit
makeStore = do
  radox <- createStore defaultState [log] rootReducer
  radox.dispatch (lift LoadNewDog)
  radox.dispatch (lift Up)
  radox.dispatch (lift (GotNewDog "dog"))
  state <- radox.getState
  pure unit
```

#### What should happen then?

We've created a Radox store, passed it out `defaultState` and `rootReducer` we defined earlier, and it has given us two things:

`radox.dispatch :: action -> Effect Unit` - this takes a lifted action type and applies it to the reducers.

`radox.getState :: Effect state` - this retrieves the state at any time.

Because we have passed it `log`, each time the state changes it will be logged into the browser console or terminal.

#### What's this `lift` business?

For us to use our individual action sum types (like `DogAction` or `CountingAction`) with the shared reducer, we have to run `lift` on them (provided by the library) to turn them from normal values into `Variant` values.

#### How could I use this with React or whatever?

I wanted to avoid tying the store itself into a particular library, so a separate library that provides tools to connect this with Context in Purescript React live here as [purescript-react-radox](https://github.com/danieljharvey/purescript-react-radox).

#### How is this cleaner, exactly?

I mean basically it has less features.
