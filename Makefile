
haskell: HaPy.hs
	ghc -c HaPy.hs

test: haskell testModule test.c
	ghc --make test.c HaPy.o HaPy_stub.o -o test -package plugins -no-hs-main

python: HaPyModule.c
	gcc -c HaPyModule.c -I/usr/include/python2.7/

testModule: haskell Mod.hs
	ghc --make Mod.hs

clean:
	rm *.o
	rm *.hi
	rm *_stub*
	rm test
