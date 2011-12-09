#include <Python.h>

/* 
 * Python module that provides a way to import and run Haskell modules
 * from you python scripts.
 */

static PyObject *haskell_gimmeone(PyObject *self, PyObject *args) {
    return Py_BuildValue("i", 1);
}

static PyMethodDef HaskellMethods[] = {
    {"gimmeone", haskell_gimmeone, METH_VARARGS,
        "Returns 1."},
    {NULL, NULL, 0, NULL}
};

PyMODINIT_FUNC
inithaskell() {
    Py_InitModule("haskell", HaskellMethods);
}
