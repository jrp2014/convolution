module Massiv where

import Data.Massiv.Array as A

xs :: A.Array A.U A.Ix1 Int
xs = A.fromList A.Seq [1, 2, 3, 4]

ys :: A.Array A.U A.Ix1 Int
ys = A.fromList A.Seq [1, 2, 3, 4, 5]

zs :: A.Array A.D A.Ix1 Int
zs = A.range A.Seq 20001 30000

stencil :: A.Stencil A.Ix1 Int Int
stencil = makeConvolutionStencilFromKernel ys

result :: Array U Ix1 Int
result = A.computeAs A.U (A.mapStencil (A.Fill 0) stencil xs)

