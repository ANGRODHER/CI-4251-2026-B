# Monads

## Motivations

Applicative allows us to have independent computations in
a context. However, what if the computations depends on each other?

A lot of computations are sequential: to compuse the value of `x_n`,
we need to know the value of `x_n-1`, and to compute `x_n-1` we need
might need the values of `x_n-5, x_n-8`, etc.

That is, we would like to "bind" results so they can be used in future computations:

```haskell
do
  x1 <- [[f_1 x1]]
  x2 <- [[f_2 x1]]
  x3 <- [[f_3 x1 x2]]
  x4 <- [[f_4 x1 x2 x3]]
  [[f_5 x1 x2 x3 x4]]
```

## A way of thinking about it

The program above can be viewed as segments:

- `x1 <- [[f_1 x1]]` is a segment that computes `x1` from the context.
- A continuation of the computing that might depend on `x1`.

Thus we can model the program (abusing even further bracket notation) as:

```haskell
let c x1 = do
  x2 <- [[f_2 x1]]
  x3 <- [[f_3 x1 x2]]
  x4 <- [[f_4 x1 x2 x3]]
  [[f_5 x1 x2 x3 x4]]
in  [[ c [[f_1 x1]] ]]
```

And we can apply this framework for every line of the program, rewriting it as:

```haskell
let c4 x1 x2 x3 x4 = [[f5 x1 x2 x3 x4]]
    c3 x1 x2 x3    = [[ c4 x1 x2 x3 [[f4 x1 x2 x3]] ]]
    c2 x1 x2       = [[ c3 x1 x2 [[f3 x1 x2]] ]]
    c1 x1          = [[ c2 x1 [[f2 x1]] ]]
    in  [[ c1 [[f1 x1]] ]]

```

Refactoring the code into something a bit more readable:

```haskell
[[f1 x1]] >>= \x1 ->
  [[f2 x1]] >>= \x2 ->
    [[f3 x1 x2]] >>= \x3 ->
      [[f4 x1 x2 x3]] >>= \x4 ->
        [[f5 x1 x2 x3 x4]]
```

JS programmers might recognize this pattern: they are essentially callbacks!

```js
[[f1 x1]].then(x1 =>
  [[f2 x1]].then(x2 =>
    [[f3 x1 x2]].then(x3 =>
      [[f4 x1 x2 x3]].then(x4 =>
        [[f5 x1 x2 x3 x4]]
      )
    )
  )
)
```

That is, we only need to provide a sensible "then", or more formally
`>>=` (read as "bind") implementation.

## What's the type?

So, what are the rules that govern the behaviour of `>>=`? what's the
type it should have? what properties should it satisfy?

Naively, we might think that `>>=` should have the type:

```haskell
(>>=) :: g a -> (g a -> g b) -> g b
```

However, this is not desirable: sure, the implementation is
trivial: `(>>=) = flip ($)`, but no-one will be able to call it for the
above use-case:

```haskell
(>>=) :: IO a -> (IO a -> IO b) -> IO b
getLine :: IO String
findFolder :: String -> IO Handler
modifyPermissions :: String -> Handler -> IO Bool

do
    x <- getLine
    y <- findFolder x
    b <- modifyPermissions x y
    pure $ if b then "Success" else "Failure"

getLine >>= \(mx :: IO String) ->
    findFolder _? >>= \(my :: IO String) ->
        modifyPermissions _? >>= \(mb :: IO Bool) ->
            pure $ if mb then "Success" else "Failure"
```

What if we let the types of the functions lead the type:

```haskell
getLine           :: ()                -> IO String
findFolder        :: String            -> IO Handler
modifyPermissions :: (String, Handler) -> IO Bool
(>>=)      :: f a -> (a                -> f  b)       -> f b
```



## Rules do not exist to bind you, they exist so you may know your freedoms

Monad laws!

- Left Identity:

```haskell
do
    a' <- pure a
    k a'
```

```haskell
k a
```

- Right identity

```haskell
do
    m' <- m
    pure m'
```

```haskell
m
```

- Associativity

```haskell
do
    x <- m
    y <- k x
    h y
```

```haskell
do
    y <- do
        x <- m
        k x
    h y
```


Naming makes more sense if we consider the kleisi-composition operator:

```haskell
(>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)
f >=> g = \x -> do
    y <- f x
    g y
```

Then the monad laws can be rewritten as:

```haskell
leftIdentity   = pure >=> k == k
rightIdentity  = k >=> pure == k
associativity  = (f >=> g) >=> h == f >=> (g >=> h)

```
