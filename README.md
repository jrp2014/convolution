# convolution
## Experiments in convolution in haskell

The starting point for this experiment is https://www.blaenkdenum.com/posts/naive-convolution-in-haskell/
from 2013.  Note that the `stream-fusion` package is no longer available.

```
Benchmark bench-convolution: RUNNING...
benchmarking Naive Convolution
time                 38.14 ms   (34.13 ms .. 42.18 ms)
                     0.968 R²   (0.940 R² .. 0.995 R²)
mean                 56.81 ms   (49.48 ms .. 71.67 ms)
std dev              18.31 ms   (10.48 ms .. 25.83 ms)
variance introduced by outliers: 84% (severely inflated)

benchmarking Reduced Convolution
time                 48.88 ms   (41.64 ms .. 57.24 ms)
                     0.943 R²   (0.905 R² .. 0.999 R²)
mean                 47.84 ms   (45.62 ms .. 50.89 ms)
std dev              5.207 ms   (3.167 ms .. 7.138 ms)
variance introduced by outliers: 37% (moderately inflated)

benchmarking Parallelized Convolution
time                 20.10 ms   (14.80 ms .. 23.80 ms)
                     0.847 R²   (0.609 R² .. 0.959 R²)
mean                 31.72 ms   (28.35 ms .. 36.56 ms)
std dev              9.288 ms   (7.542 ms .. 11.73 ms)
variance introduced by outliers: 88% (severely inflated)

benchmarking Direct Convolution (foldr)
time                 925.5 ms   (702.5 ms .. 1.429 s)
                     0.965 R²   (NaN R² .. 1.000 R²)
mean                 705.5 ms   (628.8 ms .. 818.8 ms)
std dev              110.6 ms   (7.067 ms .. 146.3 ms)
variance introduced by outliers: 46% (moderately inflated)

benchmarking Direct Convolution (foldl')
time                 4.841 s    (4.618 s .. 5.127 s)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 5.265 s    (5.097 s .. 5.559 s)
std dev              287.6 ms   (10.88 ms .. 378.5 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking Vector Naive Convolution
time                 263.2 ms   (144.6 ms .. 388.1 ms)
                     0.942 R²   (0.645 R² .. 1.000 R²)
mean                 270.7 ms   (240.8 ms .. 305.1 ms)
std dev              41.00 ms   (28.24 ms .. 51.26 ms)
variance introduced by outliers: 38% (moderately inflated)

benchmarking Array Naive Convolution
time                 35.62 ms   (30.67 ms .. 39.79 ms)
                     0.952 R²   (0.909 R² .. 0.981 R²)
mean                 37.77 ms   (35.53 ms .. 43.42 ms)
std dev              6.470 ms   (3.127 ms .. 10.14 ms)
variance introduced by outliers: 65% (severely inflated)

  96,314,850,720 bytes allocated in the heap
  93,428,272,408 bytes copied during GC
   1,279,160,608 bytes maximum residency (298 sample(s))
     124,303,592 bytes maximum slop
            1219 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0     77173 colls, 77173 par   195.698s  49.745s     0.0006s    0.0197s
  Gen  1       298 colls,   297 par   62.068s  16.587s     0.0557s    0.6970s

  Parallel GC work balance: 35.22% (serial 0%, perfect 100%)

  TASKS: 13 (1 bound, 12 peak workers (12 total), using -N4)

  SPARKS: 2100809(1590934 converted, 509714 overflowed, 0 dud, 0 GC'd, 161 fizzled)

  INIT    time    0.003s  (  0.002s elapsed)
  MUT     time   69.020s  ( 55.755s elapsed)
  GC      time  257.767s  ( 66.332s elapsed)
  EXIT    time    0.001s  (  0.002s elapsed)
  Total   time  326.791s  (122.091s elapsed)

  Alloc rate    1,395,453,836 bytes per MUT second

  Productivity  21.1% of total user, 45.7% of total elapsed

Benchmark bench-convolution: FINISH
```
