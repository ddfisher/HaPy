#include "HaPy_stub.h"
#include <stdio.h>
extern void __stginit_HaPy(void);

// Module constructor and destructor defined to initialize and
// deinitialize haskell runtime for us. Necessary when using
// haskell code as a library from another language.
static void hapy_init() __attribute__((constructor));
static void hapy_init() {
	static char *argv[] = {"HaPy.so", 0};
	static char **argv_ = argv;
	static int argc = 1;
	
	hs_init(&argc, &argv_);
	hs_add_root(__stginit_HaPy);
}

static void hapy_end() __attribute__((destructor));
static void hapy_end() {
    hs_exit();
}

// We can define here any other C functions to be part of our library.
