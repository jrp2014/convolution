#!/bin/sh
cabal v2-clean
cabal v2-configure -O2 -fllvm --enable-tests --enable-benchmarks --enable-profiling --profiling-detail=all-functions --enable-coverage 
