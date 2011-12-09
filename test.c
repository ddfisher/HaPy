#include <stdio.h>
#include "HaPy_stub.h"

extern void __stginit_HaPy(void);

int main(int argc, char **argv) {

    hs_init(&argc, &argv);
	hs_add_root(__stginit_HaPy);

	char *mod = "Mod.o";
	char *arg = "greeting";
	if(argc > 1) arg = argv[1];

    HsStablePtr fun = getSymbol_c(mod, arg);
	if(fun == NULL) {
		printf("symbol not found\n");
	} else {
		printf("symbol found!\n");
	}

	char *result = retrieveString_c(fun);
	printf("%s\n", result);

    hs_exit();
}
