## Module Data.Functor.Product

Functor products

#### `Product`

``` purescript
newtype Product f g a
  = Product (Tuple (f a) (g a))
```

`Product f g` is the product of the two functors `f` and `g`.

##### Instances
``` purescript
instance functorProduct :: (Functor f, Functor g) => Functor (Product f g)
instance foldableProduct :: (Foldable f, Foldable g) => Foldable (Product f g)
instance traversableProduct :: (Traversable f, Traversable g) => Traversable (Product f g)
```

#### `runProduct`

``` purescript
runProduct :: forall f g a. Product f g a -> Tuple (f a) (g a)
```

Unwrap a product


