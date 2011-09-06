Tagged tuples of integers are used as coordinate and size vectors. Tag and size
are up to you: {vec3,0,1,0} and {addr,192,168,1,1} are valid 3 and 4 dimensional
vectors.  Each tree should only contain one type of vector. Mixing things like
{cartesian,0,1} and {polar,1,90} won't work.

```erlang
-spec vector(N) :: {term(),integer(),...} when N==size(Tuple)-1.
```
Trees are indexed by an N-dimensional vector, and can contain anything.
```erlang
-type tree(N,A)::{bsp,term(),term()} when N::integer(), A::any().
```

Building
========
```erlang
-spec new(Size::vector(N), Lookup(vector(N))->A)->tree(N,A).
```

Viewing
=======
    todo

Querying
========
    todo

Transforming
============
Factor a tree into a lower-dimension tree of lower-dimension trees. Non-zero
vector elements mark dimensions to use in the top tree.
```erlang
-spec factor(vector(N), tree(N,A))->tree(J,tree(K,A)) where J+K=N.
```
