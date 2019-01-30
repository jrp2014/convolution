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
