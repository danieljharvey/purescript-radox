# Purescript Radox

## It's like Redux, but even cleaner!

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
  | CouldNotFindADog

type State
  = { value     :: Int
    , dog       :: DogState
    }

defaultState :: State
defaultState
  = { value     : 0
    , dog       : NotTried
    }
```

2. We'd like to be able to change this data but in a nice clean way. Here is how we'd express the dog-based logic changes.

```haskell
data DogAction
  = LoadNewDog
  | GotNewDog String
  | DogError String

instance hasLabelDogAction :: HasLabel DogAction "dogs"

dogReducer 
  :: DogAction -> State -> State
dogReducer LoadNewDog state
  = state { dog = LookingForADog }
dogReducer (GotNewDog url) state
  = state { dog = (FoundADog url) }
dogReducer (DogError e) state
  = state { dog = CouldNotFindADog } 
```

(As is hopefully clear, `DogAction` is a sum type of the different dog-related events that may happen, and `dogReducer` is a function that actions them onto the `State`.)

3. We've also got some arbitrary counting that we'd like to do, so let's smash together another reducer to do that.

```haskell
data CountingAction
  = Up
  | Down

instance hasLabelCountingAction :: HasLabel CountingAction "counting"

countingReducer 
  :: CountingAction -> State -> State
countingReducer Up state
  = state { value = state.value + 1 }
countingReducer Down s
  = state { value = state.value - 1 }
```

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
  :: State -> AnyAction -> State
rootReducer s action' =
  match
    { counting: \action -> countReducer action s
    , dogs:     \action -> dogReducer action s
    } action'
```

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

That is coming, but in a separate library. Worry not.

#### How is this cleaner, exactly.

It has less features.