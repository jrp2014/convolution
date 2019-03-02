#!/bin/sh
cabal v2-configure --enable-tests --enable-benchmarks --enable-profiling --profiling-detail=all-functions --enable-coverage 
