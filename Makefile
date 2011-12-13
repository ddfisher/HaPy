C: haskell HaPy.c
	ghc -fPIC -c HaPy.c -o HaPy.o
	ghc -dynamic -shared -package plugins \
		HaPy.o HaPy_Haskell.o HaPy_Haskell_stub.o -o libhapy.so \
		-lHSrts-ghc7.0.3 -optl-Wl,-rpath,/usr/lib/ghc/ghc-7.0.3/

haskell: HaPy.hs
	ghc -fPIC -dynamic -c HaPy.hs -o HaPy_Haskell.o

testModule:
	ghc --make Mod.hs

test: C testModule
	python test.py

clean:
	rm -f *.o
	rm -f *.hi
	rm -f *_stub*
	rm -f *.pyc
	rm -f *.so

paper:
	pdflatex -shell-escape paper.tex
