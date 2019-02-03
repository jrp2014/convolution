# convolution
## Experiments in convolution in haskell

The starting point for this experiment is https://www.blaenkdenum.com/posts/naive-convolution-in-haskell/
from 2013. The 2019 performance results are, however, very different: the naive
approach is fastest. Note that the `stream-fusion` package is no longer
available.


```
convolution/src$ ghc -O2 -fllvm -threaded -o naive Naive.hs
Loaded package environment from /.ghc/x86_64-linux-8.6.3/environments/default
[1 of 1] Compiling Main             ( Naive.hs, Naive.o )
You are using an unsupported version of LLVM!
Currently only 6.0 is supported.
We will try though...
Linking naive ...
jrp@cosmic:~/Projects/convolution/src$ ./naive +RTS -N4
benchmarking Naive Convolution
time                 2.613 ms   (2.412 ms .. 2.819 ms)
                     0.934 R²   (0.883 R² .. 0.977 R²)
mean                 2.939 ms   (2.737 ms .. 3.136 ms)
std dev              645.5 μs   (498.9 μs .. 769.6 μs)
variance introduced by outliers: 92% (severely inflated)

benchmarking Reduced Convolution
time                 43.57 ms   (30.42 ms .. 51.15 ms)
                     0.878 R²   (0.731 R² .. 0.952 R²)
mean                 57.95 ms   (50.83 ms .. 66.02 ms)
std dev              15.07 ms   (12.07 ms .. 18.99 ms)
variance introduced by outliers: 83% (severely inflated)

benchmarking Parallelized Convolution
time                 24.09 ms   (22.92 ms .. 25.12 ms)
                     0.989 R²   (0.977 R² .. 0.996 R²)
mean                 24.43 ms   (23.83 ms .. 25.14 ms)
std dev              1.445 ms   (1.109 ms .. 1.780 ms)
variance introduced by outliers: 20% (moderately inflated)
```

And testing on a MacBook Pro 2018, the naivest implementation still wins,
even over `Data.Vector`.  `Data.Vector.Unboxed` seems to be slower.

```
./naive +RTS -N6
benchmarking Naive Convolution
time                 2.313 ms   (2.257 ms .. 2.367 ms)
                     0.994 R²   (0.990 R² .. 0.997 R²)
mean                 2.364 ms   (2.325 ms .. 2.405 ms)
std dev              138.2 μs   (119.0 μs .. 174.7 μs)
variance introduced by outliers: 41% (moderately inflated)

benchmarking Reduced Convolution
time                 33.15 ms   (32.20 ms .. 34.53 ms)
                     0.994 R²   (0.987 R² .. 0.998 R²)
mean                 33.98 ms   (33.34 ms .. 34.78 ms)
std dev              1.582 ms   (972.0 μs .. 2.521 ms)
variance introduced by outliers: 12% (moderately inflated)

benchmarking Parallelized Convolution
time                 14.35 ms   (14.15 ms .. 14.50 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 14.23 ms   (14.07 ms .. 14.36 ms)
std dev              358.4 μs   (274.6 μs .. 523.6 μs)

benchmarking Vector Naive Convolution
time                 2.596 ms   (2.586 ms .. 2.603 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 2.609 ms   (2.596 ms .. 2.643 ms)
std dev              73.04 μs   (8.910 μs .. 151.1 μs)
variance introduced by outliers: 13% (moderately inflated)
```

And now with a simple Array
```
cabal new-exec convolution +RTS -N4 -s
benchmarking Naive Convolution
time                 2.292 ms   (2.216 ms .. 2.352 ms)
                     0.989 R²   (0.980 R² .. 0.994 R²)
mean                 2.894 ms   (2.705 ms .. 3.157 ms)
std dev              768.4 μs   (569.8 μs .. 972.9 μs)
variance introduced by outliers: 94% (severely inflated)

benchmarking Reduced Convolution
time                 40.59 ms   (38.46 ms .. 41.80 ms)
                     0.993 R²   (0.978 R² .. 0.999 R²)
mean                 44.97 ms   (43.57 ms .. 47.22 ms)
std dev              3.566 ms   (2.319 ms .. 5.382 ms)
variance introduced by outliers: 27% (moderately inflated)

benchmarking Parallelized Convolution
time                 38.35 ms   (34.08 ms .. 43.17 ms)
                     0.901 R²   (0.738 R² .. 0.984 R²)
mean                 60.53 ms   (51.71 ms .. 72.76 ms)
std dev              20.31 ms   (12.53 ms .. 28.73 ms)
variance introduced by outliers: 91% (severely inflated)

benchmarking Vector Naive Convolution
time                 4.519 ms   (3.845 ms .. 5.058 ms)
                     0.917 R²   (0.890 R² .. 0.952 R²)
mean                 4.022 ms   (3.820 ms .. 4.322 ms)
std dev              715.5 μs   (499.1 μs .. 949.6 μs)
variance introduced by outliers: 86% (severely inflated)

benchmarking Array Naive Convolution
time                 31.23 ms   (28.17 ms .. 34.47 ms)
                     0.948 R²   (0.889 R² .. 0.984 R²)
mean                 41.49 ms   (37.52 ms .. 48.18 ms)
std dev              10.12 ms   (7.093 ms .. 15.47 ms)
variance introduced by outliers: 79% (severely inflated)

      98,965,168 bytes allocated in the heap
      40,748,728 bytes copied during GC
       9,861,296 bytes maximum residency (8 sample(s))
         223,056 bytes maximum slop
              23 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0        87 colls,    87 par    0.326s   0.081s     0.0009s    0.0135s
  Gen  1         8 colls,     7 par    0.019s   0.005s     0.0006s    0.0010s

  Parallel GC work balance: 14.63% (serial 0%, perfect 100%)

  TASKS: 10 (1 bound, 9 peak workers (9 total), using -N4)

  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.003s  (  0.002s elapsed)
  MUT     time    0.069s  ( 26.476s elapsed)
  GC      time    0.345s  (  0.086s elapsed)
  EXIT    time    0.001s  (  0.007s elapsed)
  Total   time    0.418s  ( 26.571s elapsed)

  Alloc rate    1,430,954,250 bytes per MUT second

  Productivity  16.7% of total user, 99.7% of total elapsed

gc_alloc_block_sync: 11818
whitehole_spin: 0
gen[0].sync: 0
gen[1].sync: 1977
```
