# fast-transducers

## Why
Measure the performance cost of volatile synchronization of mutable state in stateful transducers, cf [CLJ-2146](https://clojure.atlassian.net/browse/CLJ-2146).

## What
`fast-transducers.core` defines unsynchronized versions of `clojure.core`'s transducers relying on `volatile!`. The volatile container is replaced with a `clojure.lang.Box`, the rest of the code is the same.

## How
Run the benchmark with `clojure -Aperf`. Numbers are relative performance improvements of unsynchronized versions w.r.t their respective volatile-synchronized counterparts.