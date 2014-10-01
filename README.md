cluss
=====

**simple alternative to type classes**

A *cluss* enables you to achieve *function overloading*, or ad-hoc polymorphism,
without creating a new type class.

In order to give ad-hoc polymorphism to a type variable `a`,
you simply use `In` with a list of "type patterns" like `In [Type T, ...] a`,
which indicates that the type matches some of the patterns;
which is analogous to a type class indicating that a type matches some of its "instances".
The constraint `In [Type T, ...] a` is what we call a "cluss".

Unlike type classes, whose instances are open and whose methods are closed,
cluss instances are *closed* and cluss methods are *open*.

Clusses can easily be used in a nested way,
and can even be *recursive*, just like recursive type classes,
and therefore clusses are expressive enough to imitate Haskell-98-style type classes.

The latest version is available at [https://hackage.haskell.org/package/cluss](https://hackage.haskell.org/package/cluss).
