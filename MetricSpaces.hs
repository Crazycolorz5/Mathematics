{-# LANGUAGE ExistentialQuantification #-}
module MetricSpaces where

import ArbitrarySet

data MetricSpace x = MetricSpace {metric :: x -> x -> Double} --X being the collection of elements, where the Type X represents the set of elements.

if' p t f = if p then t else f

euclideanR2 :: MetricSpace (Double, Double)
euclideanR2 = MetricSpace {metric = euclideanDist}

euclideanDist :: (Double, Double) -> (Double, Double) -> Double
euclideanDist (x1, x2) (y1, y2) = sqrt ((x1-y1)**2 + (x1-y2)**2)

standardDiscrete :: (Eq x) => MetricSpace x
standardDiscrete = MetricSpace {metric = standardDiscreteMetric}

standardDiscreteMetric :: (Eq x) => x -> x -> Double
standardDiscreteMetric a b = if a == b then 0 else 1

lpNormR2 :: Double -> (Double, Double) -> Double
lpNormR2 p (x1, x2) = (x1**p+x2**p)**(1/p)

lpSpace p = MetricSpace {metric = \(x1, x2) (y1,y2) -> lpNormR2 p (x1-y1) (x2-y2)}

boxMetric (x1, x2) (y1, y2) = max (x1-y1) (x2-y2)
box = MetricSpace {metric = boxMetric} -- = lim as p -> infinity of lpSpace p

taxicab = lpSpace 1


ball :: MetricSpace x -> x -> Double -> Set x
ball MetricSpace{metric = d} a r = fromFunction ((<r) . d a)

