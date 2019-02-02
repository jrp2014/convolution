# convolution
Experiments in convolution in haskell


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
