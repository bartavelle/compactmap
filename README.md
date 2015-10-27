# compactmap

[![Build Status](https://travis-ci.org/bartavelle/compactmap.svg?branch=master)](https://travis-ci.org/bartavelle/compactmap)
[![compactmap on Stackage LTS 3](http://stackage.org/package/compactmap/badge/lts-3)](http://stackage.org/lts-3/package/compactmap)
[![compactmap on Stackage Nightly](http://stackage.org/package/compactmap/badge/nightly)](http://stackage.org/nightly/package/compactmap)

Memory efficient read-only key-value stores. It is built by sorting the input list and converting it to a `Vector`. This module also exports a generic version that works like the one in `Vector`.

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
