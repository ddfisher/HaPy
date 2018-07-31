HaPy
====

Call Haskell functions from Python!  HaPy is set of Haskell bindings for Python.  Initially written in 2011 as a final project for Stanford's CS240H Haskell class by Ashwin Siripurapu, William Rowan, and David Fisher.  Now rewritten mostly from scratch by David Fisher with different tradeoffs (gaining far more stability at the expense of initial setup).

**Table of Contents**

- [Ubuntu Installation and Usage](#ubuntu-installation-and-usage)
- [OS X/General Installation and Usage](#os-xgeneral-installation-and-usage)
- [Common Errors](#common-errors)
- [FAQ](#faq)
- [Caveats](#caveats)
- [Future development](#future-development)

Ubuntu Installation and Usage:
------------------
* Install the dynamic base libraries with `sudo apt-get install ghc-dynamic`.
* Follow the steps of the OS X/General Installation below.

OS X/General Installation and Usage:
--------------------------
* Pre-installation assumptions: (if you don't meet these, please consult the appropriate project's documentation for more information)
    * You have [pip](https://pypi.python.org/pypi/pip) installed.
    * You are using [cabal](http://www.haskell.org/cabal/) as your Haskell build tool.  You are running cabal 1.18 or above.
    * You are using [GHC](http://www.haskell.org/ghc/) version 7.6 or above. (This project might work with earlier versions, but this has not been tested.)
* In the directory of your Haskell project:
    * Update your cabal file:
        * Add HaPy to your build-depends.  You should have a line that looks like this: `build-depends: [other haskell dependencies, if any], HaPy == 0.1.*`.
        * Add the GHC RTS library to your extra-libraries.  The line should look like this: `extra-libraries: HSrts-ghc7.6.3`.  *IMPORTANT*: You must update this line to refer to your current version of GHC. (This is slightly annoying, but I haven't been able to find a better way to do this.)
        * Make a FFI export Haskell module.  Due to Template Haskell restrictions, this must be in a different module from any functions you are exporting.  See: `example/haskell/Export.hs`.
    * If you're not already using one, create a [cabal sandbox](http://coldwa.st/e/blog/2013-08-20-Cabal-sandbox.html) with `cabal sandbox init`.
    * Build and install your Haskell module to your local sanbox with `cabal install --enable-shared`.
    * Copy the compiled .so (on Linux) or .dylib (on OS X) file from `dist/dist-sandbox-*/build/` to your Python project directory.
* In your Python project:
    * Install the Python library with `sudo pip install hapy-ffi`.
    * Import your Haskell module with `from HaPy import HASKELL_MODULE_NAME`.
    * Call Haskell functions from Python, just like you would any other Python module.

Common Errors:
-------------
* During cabal install:
    * Error: `cabal: Could not resolve dependencies` -->  HaPy might not be in your local package index; run `cabal update` and try again.  If this doesn't help, the problem probably isn't HaPy related.
    * Error: `Missing C library: HSrts-ghc7.8.2` (or similar) --> The version of the GHC RTS library that you specified in your cabal file doesn't match the version of GHC you're running.
* When running your Python project:
    * `Symbol not found: _stg_IND_STATIC_info` or `undefined symbol: stg_forkOnzh` --> The GHC RTS library isn't specified as one of the extra-libraries in the cabal file of your Haskell project.  See the [General Installation and Usage](#os-xgeneral-installation-and-usage) section above.
    * ImportError on HaPy --> You need to install HaPy Python library to your PYTHONPATH.
    * Cannot find ExampleModule error --> You probably need to put libHSExampleModule*.dylib or .so on your path or in this directory by symlink:
        ```
        $ ln -s `find ../ -iname '*.dylib'` .
        ```
    * dlopen error on `_stg_*` symbol --> Please check that `extra-libs:` section in `../haskell/*.cabal` file contains proper GHC RTS library reference (name varies between GHC versions.)
* Anything else: please make an issue and I'll take a look!

FAQ:
-----
None yet!  Feel free to start an issue if there's something you don't understand.

Caveats:
-------------
* Only functions of certain types can be exported.  Currently supported types:
 * Bool
 * Char
 * Int
 * Double
 * String
 * Lists (at any level of nesting) of all the above types (e.g. [Int], [[Int]], etc.)
* The FFI adds some overhead: all values are copied twice (e.g. from the Python representation to the C representation to the Haskell representation).

Future development:
-------------------
* Near term:
 * Allow the compiled Haskell binary to be in appropriate other directories
* Under consideration:
 * Automatically compile Haskell library from Python
 * Automatically generate Template Haskell export file in Python
 * Add support for passing Python functions
 * Add support for tuples
