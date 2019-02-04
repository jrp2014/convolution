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

And now with a simple Array and folding
```
jrp@cosmic:~/Projects/convolution$ cabal new-exec convolution +RTS -N4 -s
Resolving dependencies...
benchmarking Naive Convolution
time                 2.440 ms   (2.343 ms .. 2.525 ms)
                     0.984 R²   (0.972 R² .. 0.992 R²)
mean                 2.795 ms   (2.642 ms .. 3.001 ms)
std dev              584.0 μs   (434.4 μs .. 720.8 μs)
variance introduced by outliers: 90% (severely inflated)

benchmarking Reduced Convolution
time                 46.62 ms   (41.58 ms .. 53.92 ms)
                     0.950 R²   (0.871 R² .. 0.999 R²)
mean                 45.12 ms   (43.81 ms .. 49.20 ms)
std dev              4.153 ms   (1.347 ms .. 7.864 ms)
variance introduced by outliers: 34% (moderately inflated)

benchmarking Parallelized Convolution
time                 42.88 ms   (39.56 ms .. 48.69 ms)
                     0.938 R²   (0.856 R² .. 0.992 R²)
mean                 54.93 ms   (49.05 ms .. 66.46 ms)
std dev              14.19 ms   (7.286 ms .. 20.98 ms)
variance introduced by outliers: 83% (severely inflated)

benchmarking Direct Convolution (foldr)
time                 672.0 ms   (536.3 ms .. 793.3 ms)
                     0.993 R²   (0.992 R² .. 1.000 R²)
mean                 729.8 ms   (698.1 ms .. 749.0 ms)
std dev              31.92 ms   (13.41 ms .. 44.37 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking Direct Convolution (foldl')
time                 6.067 s    (3.703 s .. 7.762 s)
                     0.983 R²   (0.941 R² .. 1.000 R²)
mean                 6.590 s    (6.115 s .. 6.814 s)
std dev              364.3 ms   (164.1 ms .. 464.2 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking Vector Naive Convolution
time                 4.212 ms   (3.750 ms .. 4.819 ms)
                     0.929 R²   (0.896 R² .. 0.986 R²)
mean                 3.812 ms   (3.689 ms .. 4.022 ms)
std dev              504.7 μs   (264.7 μs .. 766.8 μs)
variance introduced by outliers: 77% (severely inflated)

benchmarking Array Naive Convolution
time                 29.64 ms   (25.94 ms .. 31.78 ms)
                     0.947 R²   (0.828 R² .. 0.994 R²)
mean                 41.26 ms   (37.22 ms .. 46.08 ms)
std dev              8.997 ms   (6.774 ms .. 10.83 ms)
variance introduced by outliers: 79% (severely inflated)

   1,693,221,488 bytes allocated in the heap
     738,621,488 bytes copied during GC
      66,198,440 bytes maximum residency (29 sample(s))
       2,416,776 bytes maximum slop
             158 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0      1583 colls,  1583 par    3.235s   0.825s     0.0005s    0.0436s
  Gen  1        29 colls,    28 par    0.051s   0.014s     0.0005s    0.0011s

  Parallel GC work balance: 11.94% (serial 0%, perfect 100%)

  TASKS: 10 (1 bound, 9 peak workers (9 total), using -N4)

  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.003s  (  0.002s elapsed)
  MUT     time    1.041s  (143.141s elapsed)
  GC      time    3.286s  (  0.839s elapsed)
  EXIT    time    0.001s  (  0.009s elapsed)
  Total   time    4.330s  (143.991s elapsed)

  Alloc rate    1,626,957,914 bytes per MUT second

  Productivity  24.1% of total user, 99.4% of total elapsed

gc_alloc_block_sync: 37569
whitehole_spin: 0
gen[0].sync: 0
gen[1].sync: 28919
```
