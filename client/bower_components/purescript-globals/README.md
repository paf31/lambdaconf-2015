# Module Documentation

## Module Global


This module defines types for some global Javascript functions
and values.

#### `nan`

``` purescript
nan :: Number
```

Not a number (NaN)

#### `isNaN`

``` purescript
isNaN :: Number -> Boolean
```

Test whether a number is NaN

#### `infinity`

``` purescript
infinity :: Number
```

Positive infinity

#### `isFinite`

``` purescript
isFinite :: Number -> Boolean
```

Test whether a number is finite

#### `readInt`

``` purescript
readInt :: Number -> String -> Number
```

Parse an integer from a `String` in the specified base

#### `readFloat`

``` purescript
readFloat :: String -> Number
```

Parse a floating point value from a `String`