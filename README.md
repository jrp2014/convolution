# convolution
## Experiments in convolution in haskell

The starting point for this experiment is https://www.blaenkdenum.com/posts/naive-convolution-in-haskell/
from 2013.  Note that the `stream-fusion` package is no longer available.

```
Running 1 benchmarks...
Benchmark bench-convolution: RUNNING...
benchmarking Naive Convolution
time                 53.75 ms   (40.59 ms .. 69.58 ms)
                     0.865 R²   (0.765 R² .. 0.989 R²)
mean                 55.17 ms   (49.59 ms .. 64.50 ms)
std dev              13.21 ms   (8.991 ms .. 17.54 ms)
variance introduced by outliers: 76% (severely inflated)

benchmarking Reduced Convolution
time                 43.01 ms   (42.28 ms .. 43.72 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 44.14 ms   (43.68 ms .. 44.59 ms)
std dev              902.7 μs   (656.2 μs .. 1.272 ms)

benchmarking Parallelized Convolution
time                 23.30 ms   (22.72 ms .. 23.95 ms)
                     0.997 R²   (0.993 R² .. 0.999 R²)
mean                 23.64 ms   (23.37 ms .. 24.36 ms)
std dev              944.2 μs   (511.3 μs .. 1.580 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking Direct Convolution (foldr)
time                 567.8 ms   (358.9 ms .. 812.5 ms)
                     0.979 R²   (0.930 R² .. 1.000 R²)
mean                 673.9 ms   (619.3 ms .. 721.2 ms)
std dev              65.75 ms   (32.88 ms .. 79.47 ms)
variance introduced by outliers: 22% (moderately inflated)

benchmarking Direct Convolution (foldl')
time                 4.679 s    (4.384 s .. 5.041 s)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 5.038 s    (4.861 s .. 5.354 s)
std dev              306.1 ms   (29.77 ms .. 379.9 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking Vector Naive Convolution
time                 235.8 ms   (126.4 ms .. 343.1 ms)
                     0.934 R²   (0.607 R² .. 1.000 R²)
mean                 243.7 ms   (222.7 ms .. 276.6 ms)
std dev              34.00 ms   (13.24 ms .. 47.99 ms)
variance introduced by outliers: 37% (moderately inflated)

benchmarking Array Naive Convolution
time                 34.25 ms   (25.28 ms .. 44.14 ms)
                     0.824 R²   (0.718 R² .. 0.972 R²)
mean                 45.50 ms   (38.80 ms .. 55.19 ms)
std dev              16.10 ms   (11.34 ms .. 20.18 ms)
variance introduced by outliers: 93% (severely inflated)

  96,496,437,312 bytes allocated in the heap
  90,963,744,376 bytes copied during GC
   1,273,309,280 bytes maximum residency (303 sample(s))
     135,738,328 bytes maximum slop
            1214 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0     77310 colls, 77310 par   193.783s  49.134s     0.0006s    0.0189s
  Gen  1       303 colls,   302 par   58.310s  15.516s     0.0512s    0.6744s

  Parallel GC work balance: 34.42% (serial 0%, perfect 100%)

  TASKS: 13 (1 bound, 12 peak workers (12 total), using -N4)

  SPARKS: 2551768(1929381 converted, 622364 overflowed, 0 dud, 0 GC'd, 23 fizzled)

  INIT    time    0.005s  (  0.004s elapsed)
  MUT     time   67.497s  ( 54.423s elapsed)
  GC      time  252.093s  ( 64.650s elapsed)
  EXIT    time    0.003s  (  0.005s elapsed)
  Total   time  319.598s  (119.082s elapsed)

  Alloc rate    1,429,639,844 bytes per MUT second

  Productivity  21.1% of total user, 45.7% of total elapsed

Benchmark bench-convolution: FINISH
```
