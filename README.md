Tagged tuples of integers are used as coordinate and size vectors. Tag and size
are up to you: {vec3,0,1,0} and {addr,192,168,1,1} are valid 3 and 4 dimensional
vectors.  Each tree should only contain one type of vector. Mixing things like
{cartesian,0,1} and {polar,1,90} won't work.

```erlang
-type vector(N) :: {term(),integer(),...} when N==size(Tuple)-1.
```
Trees are indexed by an N-dimensional vector, and can contain anything but
lists. Their width must be a power of two, and be the same for all dimensions.

```erlang
-type tree(N,A)::{bsp,term(),term()} when N::integer(), A::any_but_list().
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
```erlang
-spec at(vector(N), tree(N,A)) -> A.
```
A tree can be reduced to a lower dimension one by selecting one or more of its
indexes. You can select a single Z-slice at X=1, Y=2 from a 3D tree by doing
bsp:select({vec,1,2,undefined}, Tree).

(If all coordinates are defined, this is a point lookup and identical to
at.)

```erlang
-spec select(maybe_vector(M,N), tree(N,A) -> tree(M,A) when M<N | A when M=N.
-type maybe_vector(M,N) :: {term(),integer()|atom(),...} when 
    N==size(Tuple)-1,
    M==length([X<-Tuple, is_integer(X)]).
```

Run length encoding is a one dimensional depth-one tree with boundary rather than binary partitions.

```erlang
-spec to_rle(tree(1,A)) -> [{RunLength::integer(),A}].
```
Factor a tree into a lower-dimension tree of lower-dimension trees. Non-zero
vector elements mark dimensions to use in the top tree.

```erlang
-spec factor(Selector::vector(N), tree(N,A)) -> tree(J,tree(K,A)) when J+K=N.
```

Transforming
============
Maps tend to simplify the data while keeping size the same, making it more
repetitive, thus making the tree much smaller. Mapping a tree to filter out all
non-interesting data prior to working with it, yields impressive efficiecy
gains.

```erlang
-spec map(fun(A)->B, tree(N,A)) -> tree(N,B).
```

```erlang
-spec zipwith(fun(A,B)->C, tree(N,A), tree(N,B)) -> tree(N,C).
```
