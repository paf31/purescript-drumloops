## Module Loops

#### `Loop`

``` purescript
newtype Loop
```

A `Loop` is a finite list of timed samples, with a duration.

`Loop` is a `Semigroup` and a `Monoid` so it is simple to build loops by
concatenating simpler loops:

```purescript
bd <> sn :: Loop
```

A `Loop` can be turned into a `Track` using the `loop` function.

##### Instances
``` purescript
Semigroup Loop
Monoid Loop
```

#### `bd`

``` purescript
bd :: Loop
```

A loop which consists of a single bass drum sample.

#### `sn`

``` purescript
sn :: Loop
```

A loop which consists of a single snare sample.

#### `hh`

``` purescript
hh :: Loop
```

A loop which consists of a single high hat sample.

#### `silence`

``` purescript
silence :: Loop
```

A loop which is silent for one beat.

#### `Track`

``` purescript
newtype Track
```

A `Track` is an infinite list of samples.

A `Track` can be created by using the `loop` function, or played
using the `track` function.

#### `BPM`

``` purescript
newtype BPM
```

Beats per minute.

Use `Data.Newtype.wrap` to create a value:

```purescript
wrap 120 :: BPM
```

##### Instances
``` purescript
Newtype BPM _
```

#### `loop`

``` purescript
loop :: BPM -> Loop -> Track
```

Create a `Track` which plays the given `Loop` infinitely, at the specified rate:

```purescript
loop (wrap 120) (bd <> sn) :: Track
```

#### `Audio`

``` purescript
type Audio e = Eff (howler :: HOWLER, ref :: REF, timer :: TIMER | e)
```

The effect monad used by the `track` function.

#### `track`

``` purescript
track :: forall e. Track -> Audio e (Audio e Unit)
```

Play a `Track`.

This function returns an action which can be used to stop playback.


