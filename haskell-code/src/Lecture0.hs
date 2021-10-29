module Lecture0 where

-- gcd and foldr1 are defined in the Prelude
-- Can you figure out what they do?

gcdN :: Integral a => [a] -> a
gcdN = foldr1 gcd 