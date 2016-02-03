graph-generators
============

A Haskell library for creating regular and random graphs in a graph-library agnostic way.

Generate sample DOT files by using:

```bash
cabal sandbox init
cabal install --only-dependencies
cabal exec runghc TestGen.hs # See output for location of graphs
```