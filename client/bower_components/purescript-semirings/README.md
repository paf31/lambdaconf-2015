# Module Documentation

## Module Data.Semiring.Free

#### `Free`

``` purescript
newtype Free a
```

The free `Semiring` for a type `a`.

#### `runFree`

``` purescript
runFree :: forall a. Free a -> [[a]]
```

Unpack a value of type `Free a`.

#### `free`

``` purescript
free :: forall a. a -> Free a
```

Lift a value of type `a` to a value of type `Free a`

#### `liftFree`

``` purescript
liftFree :: forall a s. (Semiring s) => (a -> s) -> Free a -> s
```

`Free` is left adjoint to the forgetful functor from `Semiring`s to types.

#### `lowerFree`

``` purescript
lowerFree :: forall a s. (Semiring s) => (Free a -> s) -> a -> s
```

`Free` is left adjoint to the forgetful functor from `Semiring`s to types.

#### `showFree`

``` purescript
instance showFree :: (Show a) => Show (Free a)
```


#### `eqFree`

``` purescript
instance eqFree :: (Eq a) => Eq (Free a)
```


#### `ordFree`

``` purescript
instance ordFree :: (Ord a) => Ord (Free a)
```


#### `semiringFree`

``` purescript
instance semiringFree :: Semiring (Free a)
```


#### `functorFree`

``` purescript
instance functorFree :: Functor Free
```


#### `applyFree`

``` purescript
instance applyFree :: Apply Free
```


#### `applicativeFree`

``` purescript
instance applicativeFree :: Applicative Free
```


#### `foldableFree`

``` purescript
instance foldableFree :: Foldable Free
```


#### `traversableFree`

``` purescript
instance traversableFree :: Traversable Free
```