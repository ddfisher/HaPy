import HaPy

mod = HaPy.LoadHaskellModule("GHC.Paths")
print mod.ghc
