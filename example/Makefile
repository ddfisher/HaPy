all:
	ghc --make -no-hs-main -optl '-shared' HaPy_init.c -o ExampleModule.so Export.hs

test: all
	python example.py

clean:
	rm *.hi *.o *.so *_stub.h
