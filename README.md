Tagged tuples of integers are used as coordinate and size vectors.

Tag and size are up to you: {vec3,0,1,0} and {addr,192,168,1,1} are valid 3 and
4 dimensional vectors.

Each tree should only contain one type of vector. Mixing things like
{cartesian,0,1} and {polar,1,90} won't work.

```erlang
-spec vec(N) :: {term(),integer(),...} when N==size(Tuple)-1.
```

Building
========
    todo

Viewing
=======
    todo

Querying
========
    todo

Transforming
============
    todo
