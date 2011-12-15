import sys
from ctypes import *

# Load libhapy
haPy = cdll.LoadLibrary("./libhapy.so")

# Set libhapy typeinfo
haPy.retrieveInt.restype = c_int
haPy.retrieveBool.restype = lambda i: False if i is 0 else True #TODO: not working, must debug
haPy.retrieveDouble.restype = c_double
haPy.retrieveString.restype = c_char_p

# Define finder and loader
class HaPyImporter:
    def find_module(self, fullname, path=None):
        print "Find module called"
        return None        

    def load_module(self, fullname):
        return None

def useHaskellImporter(pathItem):
    if pathItem is haskellPathItem:
        return HaPyImporter()
    else:
        raise ImporterError

haskellPathItem = "<<Haskell>>"
__path__ = [haskellPathItem]
sys.path_hooks.append(useHaskellImporter)

class HaskellObject:
    def __init__(self, objPtr):
        self._ptr = objPtr
    
    def __call__(self, arg, *args):
        applyFun = None
        if isinstance(arg, int):
            applyFun = haPy.applyInt
        elif isinstance(arg, float):
            applyFun = haPy.applyDouble
        elif isinstance(arg, bool):
            applyFun = haPy.applyBool
        elif isinstance(arg, str):
            applyFun = haPy.applyString
        elif isinstance(arg, HaskellObject):
            applyFun = haPy.applyOpaque
            arg = arg._ptr
        else:
            raise TypeError("Unknown Object Type!")
        self._ptr = applyFun(self._ptr, arg)
        return self(*args) if args else self

    def toInt(self):
        return haPy.retrieveInt(self._ptr)
    
    def toBool(self):
        return haPy.retrieveBool(self._ptr)

    def toDouble(self):
        return haPy.retrieveDouble(self._ptr)

    def toString(self):
        return haPy.retrieveString(self._ptr)


class HaskellModule:
    def __init__(self, modulePath):
        self.path = modulePath

    def __getattr__(self, name):
        symPtr = haPy.getSymbol(self.path, name)
        if symPtr is 0:
            raise AttributeError

        return HaskellObject(symPtr)

def loadHaskellModule(moduleName):
    ''' moduleName should be of the standard "Data.Int"
        format using in Haskell import statements
        So we need to search the package.conf?
    '''
    return HaskellModule(moduleName)
