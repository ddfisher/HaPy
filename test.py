import HaPy

mod = HaPy.LoadHaskellModule("./Mod.o")
print mod.one.toInt()
print mod.plus(19, mod.one).toInt()
print mod.greeting.toString()
