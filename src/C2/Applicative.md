# Applicative

## Motivation

Functors allow us to applu a function to a value that's wrapped in a context.
This is useful to chain "pure" transformations to data that might come from an upstream:
a database, a file, etc.

However, what if the transformation we want to apply comes from another context?
what if the function behaviour depends on some configurations?

Regular functors don't allow us to apply functions
in a context `f :: g (a -> b)` to values in a context `x :: g a`.

Thus, we need a new abstraction that allows us to make this _application_.

## The Dream.

Let us denote a value `x` inside a context `g` as `[[x]]_g`, or
`[[x]]` for short if the context is clear from the situation.

The dream is to be able to apply a function `f` in a context `g` to some values
value `x_1,...,x_n` in the same context `g`, preferable in a "consistent" way.
That is:

```
[[f]] [[x_1]] .... [[x_n]]  = [[f x_1 ... x_n]]
```

For those trained in abstract maths, this should
look like some sort of "homomorphism", that is, it's desirable that:

- Application respects the identity: `[[ ^id x ]] = x`
- Application is "compositional": `[[ ^(.) f g x ]] = [[ f (g x) ]]`
- Application is "structure preserving": `[[ ^f ^x ]] = pure (f x)`
- "exchange" law: `[[ f ^x ]] = [[ ^($ x) f ]]`

## Why is it desirable?

- Guarantees that the application behaves like a "normal" application.
