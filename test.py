import HaPy

mod = HaPy.LoadHaskellModule("./Mod.o")
print mod.one.toInt()

for i in range(3):
  for j in range(5):
    print mod.plus(i, j).toInt()

print mod.greeting.toString()
