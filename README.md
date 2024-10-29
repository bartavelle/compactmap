# compactmap

[![Haskell-CI](https://github.com/bartavelle/compactmap/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/bartavelle/compactmap/actions/workflows/haskell-ci.yml)
[![compactmap on Stackage LTS](http://stackage.org/package/compactmap/badge/lts)](http://stackage.org/lts/package/compactmap)
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
