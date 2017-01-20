## Module Loops

#### `Loop`

``` purescript
newtype Loop
```

##### Instances
``` purescript
Semigroup Loop
Monoid Loop
```

#### `bd`

``` purescript
bd :: Loop
```

#### `sn`

``` purescript
sn :: Loop
```

#### `hh`

``` purescript
hh :: Loop
```

#### `silence`

``` purescript
silence :: Loop
```

#### `Track`

``` purescript
newtype Track
```

#### `BPM`

``` purescript
newtype BPM
```

##### Instances
``` purescript
Newtype BPM _
```

#### `loop`

``` purescript
loop :: BPM -> Loop -> Track
```

#### `track`

``` purescript
track :: forall e. Track -> Eff (howler :: HOWLER, ref :: REF, timer :: TIMER | e) Unit
```


