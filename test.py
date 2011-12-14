import HaPy

mod = HaPy.LoadHaskellModule("Mod")
print mod.one.toInt()

print mod.plus(1, mod.one).toInt()


print mod.greeting.toString()
