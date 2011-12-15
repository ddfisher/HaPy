import HaPy

mod = HaPy.LoadHaskellModule("Mod")
print mod.one

print mod.plus(1, 2)

print mod.greeting

haskellObject = mod.haskellObject
print haskellObject
print mod.mySum(haskellObject)
