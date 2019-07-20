# convolution
## Experiments in convolution in haskell

[![Build Status](https://travis-ci.com/jrp2014/convolution.svg?branch=master)](https://travis-ci.com/jrp2014/convolution)

The starting point for this experiment is
https://www.blaenkdenum.com/posts/naive-convolution-in-haskell/
from 2013, corrected for a bug in the original.  The convolution functions
produce inconsistent results when given empty parameters (where the result is
undefined).
Note that the `stream-fusion` package is no longer available.

```
Linking /Users/jrp/Documents/Haskell/convolution/dist-newstyle/build/x86_64-osx/ghc-8.6.5/convolution-0.1.0.0/b/bench-convolution/build/bench-convolution/bench-convolution ...
Running 1 benchmarks...
Benchmark bench-convolution: RUNNING...
benchmarking FFT/Full complex
time                 516.0 ms   (490.0 ms .. 567.8 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 539.4 ms   (523.1 ms .. 563.3 ms)
std dev              23.52 ms   (101.3 μs .. 30.94 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking FFT/Simultaneous real
time                 318.8 ms   (307.0 ms .. 332.5 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 326.4 ms   (322.2 ms .. 335.7 ms)
std dev              7.275 ms   (545.3 μs .. 9.407 ms)
variance introduced by outliers: 16% (moderately inflated)

benchmarking Naive/Naive Convolution
time                 32.39 ms   (31.73 ms .. 33.10 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 32.24 ms   (31.95 ms .. 32.61 ms)
std dev              694.4 μs   (502.7 μs .. 963.5 μs)

benchmarking Naive/Golf
time                 817.4 ms   (789.3 ms .. 857.9 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 810.6 ms   (803.4 ms .. 817.3 ms)
std dev              7.832 ms   (6.446 ms .. 8.685 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking Naive/Conal
time                 824.2 ms   (821.1 ms .. 828.4 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 823.0 ms   (822.4 ms .. 823.8 ms)
std dev              735.7 μs   (59.71 μs .. 919.1 μs)
variance introduced by outliers: 19% (moderately inflated)

benchmarking Naive/Reduced Convolution
time                 30.48 ms   (29.13 ms .. 31.65 ms)
                     0.993 R²   (0.983 R² .. 0.999 R²)
mean                 32.09 ms   (31.52 ms .. 33.10 ms)
std dev              1.674 ms   (1.383 ms .. 1.973 ms)
variance introduced by outliers: 17% (moderately inflated)

benchmarking Naive/Parallelized Convolution
time                 6.622 ms   (6.459 ms .. 6.781 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 6.530 ms   (6.493 ms .. 6.588 ms)
std dev              153.1 μs   (118.7 μs .. 195.6 μs)

benchmarking Naive/Vector Naive Convolution
time                 118.5 ms   (116.2 ms .. 120.6 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 113.8 ms   (109.8 ms .. 116.2 ms)
std dev              4.677 ms   (1.078 ms .. 6.501 ms)
variance introduced by outliers: 11% (moderately inflated)

benchmarking Naive/Unboxed Vector Naive Convolution
time                 45.74 ms   (41.53 ms .. 48.80 ms)
                     0.972 R²   (0.940 R² .. 0.991 R²)
mean                 47.48 ms   (45.26 ms .. 51.08 ms)
std dev              5.217 ms   (3.958 ms .. 7.183 ms)
variance introduced by outliers: 41% (moderately inflated)

benchmarking Naive/Array Naive Convolution
time                 42.78 ms   (42.36 ms .. 43.38 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 43.67 ms   (43.20 ms .. 44.60 ms)
std dev              1.372 ms   (857.4 μs .. 2.071 ms)

benchmarking Naive/Unboxed Array Naive Convolution
time                 18.10 ms   (17.78 ms .. 18.44 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 18.56 ms   (18.31 ms .. 19.53 ms)
std dev              1.039 ms   (422.9 μs .. 1.874 ms)
variance introduced by outliers: 22% (moderately inflated)

benchmarking Naive/Stream Naive Convolution
time                 4.089 s    (3.836 s .. 4.559 s)
                     0.998 R²   (0.997 R² .. 1.000 R²)
mean                 3.978 s    (3.918 s .. 4.039 s)
std dev              77.15 ms   (1.814 ms .. 94.15 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking Naive/Direct Convolution (foldr)
time                 492.4 ms   (490.4 ms .. 494.0 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 496.3 ms   (494.7 ms .. 498.8 ms)
std dev              2.421 ms   (797.0 μs .. 3.248 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking Naive/Direct Convolution (foldl')
time                 3.920 s    (3.606 s .. 4.365 s)
                     0.999 R²   (0.995 R² .. 1.000 R²)
mean                 3.793 s    (3.611 s .. 3.901 s)
std dev              178.6 ms   (53.02 ms .. 239.0 ms)
variance introduced by outliers: 19% (moderately inflated)

 369,268,882,504 bytes allocated in the heap
 107,302,736,240 bytes copied during GC
     845,336,992 bytes maximum residency (1843 sample(s))
      13,046,928 bytes maximum slop
             806 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0     255524 colls, 255524 par   765.490s  82.326s     0.0003s    0.0130s
  Gen  1      1843 colls,  1842 par   114.478s  11.174s     0.0061s    0.4137s

  Parallel GC work balance: 26.97% (serial 0%, perfect 100%)

  TASKS: 33 (1 bound, 32 peak workers (32 total), using -N12)

  SPARKS: 7445720(6917960 converted, 527760 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.001s  (  0.003s elapsed)
  MUT     time  232.087s  (119.502s elapsed)
  GC      time  879.968s  ( 93.500s elapsed)
  EXIT    time    0.000s  (  0.010s elapsed)
  Total   time  1112.057s  (213.015s elapsed)

  Alloc rate    1,591,076,635 bytes per MUT second

  Productivity  20.9% of total user, 56.1% of total elapsed

Benchmark bench-convolution: FINISH
```
