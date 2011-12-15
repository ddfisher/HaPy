import HaPy
mod = HaPy.loadHaskellModule("GHC.Paths")
print mod.libdir
