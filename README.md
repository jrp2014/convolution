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
Running 1 benchmarks...
Benchmark bench-convolution: RUNNING...
benchmarking Naive Convolution
time                 35.19 ms   (33.82 ms .. 36.71 ms)
                     0.996 R²   (0.993 R² .. 0.998 R²)
mean                 36.75 ms   (36.27 ms .. 37.44 ms)
std dev              1.276 ms   (911.8 μs .. 1.889 ms)
variance introduced by outliers: 11% (moderately inflated)

benchmarking Golf
time                 994.5 ms   (978.1 ms .. 1.019 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.000 s    (994.2 ms .. 1.006 s)
std dev              6.856 ms   (2.888 ms .. 8.913 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking Reduced Convolution
time                 31.66 ms   (30.41 ms .. 32.60 ms)
                     0.997 R²   (0.995 R² .. 0.999 R²)
mean                 34.40 ms   (33.52 ms .. 35.20 ms)
std dev              1.924 ms   (1.439 ms .. 2.373 ms)
variance introduced by outliers: 18% (moderately inflated)

benchmarking Parallelized Convolution
time                 11.33 ms   (11.14 ms .. 11.52 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 11.31 ms   (11.24 ms .. 11.40 ms)
std dev              246.6 μs   (173.4 μs .. 322.5 μs)

benchmarking Vector Naive Convolution
time                 157.1 ms   (155.9 ms .. 158.6 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 157.3 ms   (156.4 ms .. 158.2 ms)
std dev              1.400 ms   (1.005 ms .. 1.946 ms)
variance introduced by outliers: 12% (moderately inflated)

benchmarking Unboxed Vector Naive Convolution
time                 50.64 ms   (48.03 ms .. 53.24 ms)
                     0.991 R²   (0.975 R² .. 0.998 R²)
mean                 50.95 ms   (49.74 ms .. 52.24 ms)
std dev              2.365 ms   (1.889 ms .. 3.155 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking Array Naive Convolution
time                 25.07 ms   (24.55 ms .. 26.00 ms)
                     0.995 R²   (0.987 R² .. 1.000 R²)
mean                 25.17 ms   (24.83 ms .. 25.94 ms)
std dev              1.062 ms   (459.9 μs .. 1.759 ms)
variance introduced by outliers: 15% (moderately inflated)

benchmarking Unboxed Array Naive Convolution
time                 16.30 ms   (16.12 ms .. 16.54 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 16.27 ms   (16.21 ms .. 16.37 ms)
std dev              207.1 μs   (126.5 μs .. 348.0 μs)

benchmarking Stream Naive Convolution
time                 6.614 s    (6.528 s .. 6.704 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 6.712 s    (6.652 s .. 6.743 s)
std dev              49.39 ms   (20.77 ms .. 65.52 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking Direct Convolution (foldr)
time                 602.3 ms   (580.8 ms .. 625.6 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 600.6 ms   (597.3 ms .. 604.5 ms)
std dev              4.042 ms   (1.750 ms .. 5.611 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking Direct Convolution (foldl')
time                 4.002 s    (3.568 s .. 4.629 s)
                     0.997 R²   (0.992 R² .. 1.000 R²)
mean                 3.990 s    (3.823 s .. 4.057 s)
std dev              124.3 ms   (7.903 ms .. 155.0 ms)
variance introduced by outliers: 19% (moderately inflated)

 404,444,918,384 bytes allocated in the heap
 128,093,923,664 bytes copied during GC
   1,058,096,256 bytes maximum residency (6645 sample(s))
      16,285,560 bytes maximum slop
            1009 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0     264157 colls, 264157 par   779.245s  92.522s     0.0004s    0.0036s
  Gen  1      6645 colls,  6644 par   180.998s  19.587s     0.0029s    0.3983s

  Parallel GC work balance: 22.87% (serial 0%, perfect 100%)

  TASKS: 32 (1 bound, 31 peak workers (31 total), using -N12)

  SPARKS: 4883556(3872987 converted, 1010569 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.001s  (  0.003s elapsed)
  MUT     time  235.293s  (121.755s elapsed)
  GC      time  960.243s  (112.109s elapsed)
  EXIT    time    0.000s  (  0.005s elapsed)
  Total   time  1195.537s  (233.871s elapsed)

  Alloc rate    1,718,897,672 bytes per MUT second

  Productivity  19.7% of total user, 52.1% of total elapsed

Benchmark bench-convolution: FINISH
```
