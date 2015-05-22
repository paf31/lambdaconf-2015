# Module Documentation

## Module Control.Monad.Eff.Class

#### `MonadEff`

``` purescript
class (Monad m) <= MonadEff eff m where
  liftEff :: forall a. Eff eff a -> m a
```

The `MonadEff` class captures those monads which support native effects.

Instances are provided for `Eff` itself, and the standard monad transformers.

`liftEff` can be used in any appropriate monad transformer stack to lift an action
of type `Eff eff a` into the monad.

Note that `MonadEff` is parameterized by the row of effects, so type inference can be
tricky. It is generally recommended to either work with a polymorphic row of effects,
or a concrete, closed row of effects such as `(trace :: Trace)`.

#### `monadEffEff`

``` purescript
instance monadEffEff :: MonadEff eff (Eff eff)
```


#### `monadEffMaybe`

``` purescript
instance monadEffMaybe :: (Monad m, MonadEff eff m) => MonadEff eff (MaybeT m)
```


#### `monadEffError`

``` purescript
instance monadEffError :: (Monad m, MonadEff eff m) => MonadEff eff (ErrorT e m)
```


#### `monadEffState`

``` purescript
instance monadEffState :: (Monad m, MonadEff eff m) => MonadEff eff (StateT s m)
```


#### `monadEffWriter`

``` purescript
instance monadEffWriter :: (Monad m, Monoid w, MonadEff eff m) => MonadEff eff (WriterT w m)
```


#### `monadEffReader`

``` purescript
instance monadEffReader :: (Monad m, MonadEff eff m) => MonadEff eff (ReaderT r m)
```


#### `monadEffRWS`

``` purescript
instance monadEffRWS :: (Monad m, Monoid w, MonadEff eff m) => MonadEff eff (RWST r w s m)
```




