#include <stdio.h>
#include "HaPy_stub.h"

int main(int argc, char **argv) {

    hs_init(&argc, &argv);

    HsStablePtr fun = getSymbol_c("plus");
    fun = applyInt_c(fun, 6);
    fun = applyInt_c(fun, 8);

    HsInt result = retrieveInt_c(fun);

    hs_exit();

    printf("%d\n", result);
}
