import sys
from ctypes import *
import os.path
import subprocess

# Load libhapy
hapy = cdll.LoadLibrary("./libhapy.so")

# Set libhapy typeinfo
hapy.retrieveInt.restype = c_int
hapy.retrieveBool.restype = lambda i: False if i is 0 else True #TODO: not working, must debug
hapy.retrieveDouble.restype = c_double
hapy.retrieveString.restype = c_char_p

class HaskellType:
    def __init__(self, name, cls, applyFun, retrieveFun):
        self.name = name
        self.cls = cls
        self.applyFun = applyFun
        self.retrieveFun = retrieveFun

    def __repr__(self):
        return self.__class__.__name__ + "(" + self.name + ")"

_types = [ HaskellType("Int"    , int   , hapy.applyInt    , hapy.retrieveInt)
         , HaskellType("Bool"   , bool  , hapy.applyBool   , hapy.retrieveBool)
         , HaskellType("Double" , float , hapy.applyDouble , hapy.retrieveDouble)
         , HaskellType("String" , str   , hapy.applyString , hapy.retrieveString)
         ]
_KNOWN_TYPES = { type.name:type for type in _types }

def _isGeneric(type):
    return type[0].islower()

def _knownTypeFromClass(cls):
    for type in _KNOWN_TYPES.values():
        if type.cls == cls:
            return type
    return None
                
class HaskellFunctionType:
    def __init__(self, name, argTypes, returnType):
        self.name = name
        self.argTypes = argTypes
        self.returnType = returnType

    def __repr__(self):
        return self.__class__.__name__ + "(" + repr(self.name) + ", " + repr(self.argTypes) + ", " + repr(self.returnType) + ")"

    @staticmethod
    def parse(s):
        [name, rest] = s.split("::")
        name = name.strip()
        types = rest.split("->")
        types = map(str.strip, types)
        return HaskellFunctionType(name, types[:-1], types[-1])


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
    def __init__(self, objPtr, name, argTypes, returnType):
        self._ptr = objPtr
        self.name = name
        self.argTypes = argTypes
        self.returnType = returnType

    def __repr__(self):
        return self.__class__.__name__ + "(" + repr(self.name) + ", " + repr(self.argTypes) + ", " + repr(self.returnType) + ")"

    def isFullyApplied(self):
        return self.argTypes == []

    def __call__(self, arg, *args):
        # make sure object is still valid
        if self._ptr is None:
            raise RuntimeError("Value already retrieved: object no longer callable")

        if self.isFullyApplied():
            return RuntimeError("Too many arguments")

        # adjust generic type if it exists
        if _isGeneric(self.argTypes[0]):
            nextTypeInfo = _knownTypeFromClass(arg.__class__)
            oldType = self.argTypes[0]
            if nextTypeInfo is not None:
                newType = nextTypeInfo.name
            elif isinstance(arg, HaskellObject):
                newType = arg.returnType
            else:
                raise TypeError("Cannot handle " + arg.__class__.__name__)
            self.argTypes = [newType if t == oldType else t for t in self.argTypes]
            if self.returnType == oldType:
                self.returnType = newType

        # apply argument as appropriate
        nextType = self.argTypes[0]
        if nextType in _KNOWN_TYPES:
            nextTypeInfo = _KNOWN_TYPES[nextType]
            if isinstance(arg, nextTypeInfo.cls):
                self._ptr = nextTypeInfo.applyFun(self._ptr, arg)
                del self.argTypes[0]
            else:
                # type mismatch!
                raise TypeError("Expected argument of type " + nextTypeInfo.cls.__name__
                                + " but received type " + arg.__class__.__name__ + ".")
        elif isinstance(arg, HaskellObject):
            if arg.isFullyApplied() and arg.returnType == nextType:
                self._ptr = hapy.applyOpaque(self._ptr, arg._ptr) # TODO: currently, both pointers are freed. instead of always freeing pointers, use finalizers
                arg._ptr = None
                del self.argTypes[0]
            else:
                raise TypeError("Expected haskell object of type " + nextType
                                + "but received type " + arg.returnType)
        else:
            raise TypeError("Cannot handle " + arg.__class__.__name__)


        if args:
            return self(*args)
        else:
            return self._retrieve()

    def _retrieve(self):
        # TODO: case that the last argument is an unresolved generic type (rare)
        if self.isFullyApplied() and self.returnType in _KNOWN_TYPES:
            retVal = _KNOWN_TYPES[self.returnType].retrieveFun(self._ptr)
            self._ptr = None
            return retVal
        else: 
            return self

class HaskellModule:
    def __init__(self, name, dirs):
        self.path = os.path.join(dirs[0], name) #TODO: support multiple directories (the load command in haskell already does this: we just need to pass the string list through)
        self.interface = _getInterface(self.path)

    def __getattr__(self, name):
        if name in self.interface:
            symPtr = hapy.getSymbol(self.path, name)
            if symPtr is not None:
                return HaskellObject(symPtr, *self.interface[name])._retrieve()
            else:
                raise AttributeError("Function not found: interface mismatch")
        else:
            raise AttributeError("Function not found")

def _getInterface(file):
    interfaceOutput = subprocess.check_output(["ghc", file, "-e", ":browse"])
    functions = interfaceOutput.splitlines()
    functions = map(_parseInterfaceLine, functions)
    return {func[0]: func for func in functions}

def _parseInterfaceLine(s):
    # TODO: parse class if present e.g. "Number a => a"
    if "=>" in s:
        raise TypeError("typeclasses not yet supported")
    # TODO: parse functions properly
    if "(" in s:
        raise TypeError("functions not yet supported")
    [name, rest] = s.split("::")
    name = name.strip()
    types = rest.split("->")
    types = map(str.strip, types)
    return (name, types[:-1], types[-1])
    

def loadHaskellModule(moduleName, dirs=["."]):
    ''' moduleName should be of the standard "Data.Int"
        format using in Haskell import statements
        So we need to search the package.conf?
    '''
    return HaskellModule(moduleName, dirs)
