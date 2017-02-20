## Module Loops

#### `Passage`

``` purescript
newtype Passage s
```

A `Passage` is a finite list of timed samples, with a duration.
Durations in a Track are absolute; in a Passage, they are relative.

`Passage` is a `Semigroup` and a `Monoid` so it is simple to build complex Passages by
concatenating simpler ones:

```purescript
bd <> sn :: Passage
```

Invariant: samples are in increasing time order.

##### Instances
``` purescript
Semigroup (Passage s)
Monoid (Passage s)
```

#### `merge`

``` purescript
merge :: forall s. Passage s -> Passage s -> Passage s
```

Join two `Passage`s to play concurrently as one.

#### `Merge`

``` purescript
newtype Merge s
```

A `newtype` around `Passage` with a `Monoid` instance
which merges passages (concurrently).

##### Instances
``` purescript
Newtype (Merge s) _
Semigroup (Merge s)
Monoid (Merge s)
```

#### `beat`

``` purescript
beat :: forall s. s -> Passage s
```

A unit-length passage consisting of a single sample.

#### `silence`

``` purescript
silence :: forall s. Passage s
```

A unit-length Passage with no sounds.

#### `Millis`

``` purescript
type Millis = Int
```

#### `Event`

``` purescript
type Event s = { sample :: s, time :: Millis }
```

#### `Track`

``` purescript
newtype Track s
```

A `Track` is an infinite list of samples.

A `Track` can be created by using the `loop` function, or played
using the `play` function.

##### Instances
``` purescript
Newtype (Track s) _
```

#### `Warp`

``` purescript
newtype Warp
```

A monotonically increasing function of time.

See `warp`.

##### Instances
``` purescript
Newtype Warp _
```

#### `swing`

``` purescript
swing :: Number -> Warp
```

#### `warp`

``` purescript
warp :: forall s. Warp -> Track s -> Track s
```

A generalization of swing.
To preserve order, requires a monotonic increasing function of numbers.
And unless the function intersects the 45 degree line infinitely often,
this will change the speed of the input Track.

#### `Dur`

``` purescript
data Dur
  = Dur Rational
  | BPM Rational
```

#### `loop`

``` purescript
loop :: forall s. Dur -> Passage s -> Track s
```

Create a `Track` which plays the given `Passage` infinitely, at the specified rate:

```purescript
Passage (BPM 120) (bd <> sn) :: Track
```

A Dur of 1 or a BPM of 60 causes each unit length in the Passage to render as one second.

#### `Audio`

``` purescript
type Audio e = Eff (howler :: HOWLER, ref :: REF, timer :: TIMER | e)
```

The effect monad used by the `track` function.

#### `Playable`

``` purescript
newtype Playable s e
```

A process which enqueues a collection of samples for playback.

#### `track`

``` purescript
track :: forall e s. Track s -> Playable s e
```

Make a `Track` into a `Playable` by enqueing all of its (infinitely-many!)
samples incrementally.

#### `play`

``` purescript
play :: forall e. Playable Howl e -> Audio e Unit
```

Play a `Track`.

This function returns an action which can be used to stop playback.

#### `playWith`

``` purescript
playWith :: forall e s. (s -> Howl) -> Playable s e -> Audio e Unit
```

#### `degrade`

``` purescript
degrade :: forall e s. Number -> Playable s (random :: RANDOM | e) -> Playable s (random :: RANDOM | e)
```

"Degrade" a `Playable` process by ignoring samples with a certain
probability.


