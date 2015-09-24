# compactmap

[![Build Status](https://travis-ci.org/bartavelle/compactmap.svg?branch=master)](https://travis-ci.org/bartavelle/compactmap)

Memory efficient read-only key-value stores. It is built by sorting the input list and converting it to a `Vector`.

Example usage:

```
module Main where

import qualified Data.CompactMap as CM

kvlist :: [(String, Int)]
kvlist = zip (map return ['a' .. 'z']) [5..]

main :: IO ()
main = do
    let compactmap = CM.fromList kvlist fst
    print (CM.lookup "b" compactmap)
    print (CM.lookup "aa" compactmap)
```
