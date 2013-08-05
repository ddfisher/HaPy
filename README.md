HaPy
====

Call Haskell functions from Python!  HaPy is set of Haskell bindings for Python.  Initially written in 2011 as a final project for Stanford's CS240H Haskell class by Ashwin Siripurapu, William Rowan, and David Fisher.  Now rewritten mostly from scratch by David Fisher with different tradeoffs (gaining far more stability at the expense of initial setup).

Usage:
------
* Install the Haskell module by running `cabal install` in the `haskell` directory. The dependencies are currently more restrictive than necessary, so you may need to relax them if you have compilation problems.
* Make a FFI export Haskell module.  Due to Template Haskell restrictions, this must be in a different module from any functions you are exporting.  See: `example/Export.hs`.
* Copy the `c/HaPy_init.c` file to your Haskell project directory.  Build your project with:
	`ghc --make -no-hs-main -optl '-shared' HaPy_init.c -o LIB_NAME.so HASKELL_FFI_FILE`
* Install the Python module by running `python setup.py install` in the `python` directory.
* Put the resulting `LIB_NAME.so` file in the main directory of your Python project.
* Add `from HaPy import LIB_NAME` to the top of your Python file
* Call Haskell functions from Python, just like you would any other Python module.

Restrictions:
-------------
* Only functions of certain types can be exported.  Currently supported types:
 * Bool
 * Int
 * Double

Future development:
-------------------
* Near term:
 * Add support for String and List types
 * Allow the compiled Haskell binary to be in appropriate other directories
* Under consideration:
 * Automatically compile Haskell library from Python
 * Automatically generate Template Haskell export file in Python
