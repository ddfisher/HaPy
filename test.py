import HaPy

mod = HaPy.LoadHaskellModule("./Mod.o")
print mod.one.toInt()

print mod.plus(1, "cows").toInt()


print mod.greeting.toString()
