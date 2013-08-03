#include "HsFFI.h"

static void initialize() __attribute__((constructor));
static void initialize() {
	static char *argv[] = {"HaPy", 0};
	static char **argv_ = argv;
	static int argc = 1;
	
	hs_init(&argc, &argv_);
}

static void finalize() __attribute__((destructor));
static void finalize() {
    hs_exit();
}
