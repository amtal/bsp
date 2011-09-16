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
```erlang
-spec is_tree(term()) -> true|false.
-spec size(tree(N,A)) -> vector(N).
```

Binary space partioning merges identical regions of data, often storing less values than flat containers.

```erlang
-spec sparsity(tree(N,A)) -> {TreeCount::integer(), FlatCount::integer()}.
```

Building
========
Ideal constructor from flat data (binaries, arrays) accessible via vector lookups.

```erlang
-spec new(Size::vector(N), Lookup::fun(vector(N))->A) -> tree(N,A).
```

Viewing
=======
    todo

Querying
========
    todo

Transforming
============
Maps tend to simplify the data while keeping size the same, making it more
repetitive, thus making the tree much smaller. Mapping a tree to filter out all
non-interesting data prior to working with it, yields impressive efficiecy
gains.

```erlang
-spec map(fun(A)->B, tree(N,A)) -> tree(N,B).
```
Factor a tree into a lower-dimension tree of lower-dimension trees. Non-zero
vector elements mark dimensions to use in the top tree.

```erlang
-spec factor(Selector::vector(N), tree(N,A)) -> tree(J,tree(K,A)) where J+K=N.
```
