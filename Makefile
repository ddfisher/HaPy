
haskell: HaPy.hs
	ghc -c HaPy.hs
	ghc test.c HaPy.o HaPy_stub.o -o test 

python: HaPyModule.c
	gcc -c HaPyModule.c -I/usr/include/python2.7/

testModule: haskell Mod.hs
	ghc --make Mod.hs

test: haskell testModule
	./test

clean:
	rm *.o
	rm *.hi
	rm *_stub*
	rm test
