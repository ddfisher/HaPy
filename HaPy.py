from ctypes import *
import sys
import subprocess
import weakref
import os

# Load libhapy
hapy = cdll.LoadLibrary("./libhapy.so")

# Set libhapy typeinfo
hapy.retrieveInt.restype = c_int
hapy.retrieveBool.restype = lambda i: False if i == 0 else True
hapy.retrieveDouble.restype = c_double
hapy.retrieveString.restype = c_char_p
hapy.getInterfaceFilePath.restype = c_char_p

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
         , HaskellType("Double" , float , lambda p, f: hapy.applyDouble(p, c_double(f)) , hapy.retrieveDouble)
         # , HaskellType("Double" , float , hapy.applyDouble , hapy.retrieveDouble)
         , HaskellType("String" , str   , hapy.applyString , hapy.retrieveString)
         ]
_KNOWN_TYPES = { type.name:type for type in _types }

def _isGeneric(type):
    return isinstance(type, str) and type[0].islower()

def _knownTypeFromClass(cls):
    for type in _KNOWN_TYPES.values():
        if type.cls == cls:
            return type
    return None

# Define importer for haskell modules as part of the python import mechanism
class HaPyImporter:
    def find_module(self, fullname, path=None):
        if (fullname.split('.'))[0] == "HaPy":
            return self
        else:
            raise ImportError

    def load_module(self, fullname):
        modName = '.'.join((fullname.split('.'))[1:])
        if hapy.doesModuleExist(modName):
            mod = loadHaskellModule(modName)
            mod.__file__ = "<HaskellModule>"
            mod.__path__ = None
        else:
            mod = HaPyImporter()
            mod.__file__ = "<Stub>"
            mod.__path__ = None
        mod.__loader__ = self
        sys.modules.setdefault(fullname, mod)
        return mod

    def __getattr__(self, name):
        raise AttributeError

# After much experimentation it has been found that the combination
# of __path__ and adding the importer to the meta_path works.
__path__ = []
sys.meta_path.append(HaPyImporter())

class HaskellObject:
    def __init__(self, objPtr, name, argTypes, returnType):
        self._ptr = objPtr
        self.name = name
        self.argTypes = argTypes
        self.returnType = returnType
        self._finalizer = weakref.ref(self, lambda wr, ptr=self._ptr: hapy.freePtr(ptr))

    def __repr__(self):
        return self.__class__.__name__ + "(" + repr(self.name) + ", " + repr(self.argTypes) + ", " + repr(self.returnType) + ")"

    def isFullyApplied(self):
        return not self.argTypes

    def __call__(self, *allArgs, **kwargs):
        typecheck = kwargs.get("typecheck", True)

        if not allArgs:
            return self._retrieve()

        arg = allArgs[0]
        args = allArgs[1:]
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
            argTypes = [newType if t == oldType else t for t in self.argTypes]
            if self.returnType == oldType:
                returnType = newType
        else:
            argTypes = self.argTypes
            returnType = self.returnType

        # apply argument as appropriate
        nextType = argTypes[0]
        if nextType in _KNOWN_TYPES:
            nextTypeInfo = _KNOWN_TYPES[nextType]
            if isinstance(arg, nextTypeInfo.cls):
                return HaskellObject(nextTypeInfo.applyFun(self._ptr, arg), self.name, argTypes[1:], returnType)(*args)
            else:
                # type mismatch!
                raise TypeError("Expected argument of type " + nextTypeInfo.cls.__name__
                                + " but received type " + arg.__class__.__name__ + ".")
        elif isinstance(arg, HaskellObject):
            if not typecheck or (arg.isFullyApplied() and arg.returnType == nextType) or tuple(arg.argTypes + [arg.returnType]) == nextType:
                return HaskellObject(hapy.applyOpaque(self._ptr, arg._ptr), self.name, argTypes[1:], returnType)(*args)
            else:
                raise TypeError("Expected haskell object of type " + str(nextType)
                                + " but received type " + str(arg.returnType))
        else:
            raise TypeError("Cannot handle " + arg.__class__.__name__)


        if args:
            return self(*args)
        else:
            return self._retrieve()

    def _retrieve(self):
        # TODO: case that the last argument is an unresolved generic type (rare)
        if self.isFullyApplied() and self.returnType in _KNOWN_TYPES:
            return _KNOWN_TYPES[self.returnType].retrieveFun(self._ptr)
        else: 
            return self

class HaskellModule:
    def __init__(self, name, dirs):
        self.name = name 
        self.interface = _getInterface(self.name)
        self.__str__ = name
        self.__path__ = []

    def __getattr__(self, name):
        if not self.interface:
            raise AttributeError("Interface could not be loaded")
        if name in self.interface:
            symPtr = hapy.getSymbol(self.name, name)
            if symPtr is not None:
                return HaskellObject(symPtr, *self.interface[name])()
            else:
                raise AttributeError("Function not found: interface mismatch")
        else:
            raise AttributeError("Function not found")

def _getInterface(modName):
    moduleLocation = hapy.getInterfaceFilePath(modName)[:-3]
    try:
        if (moduleLocation):
            interfaceOutput = subprocess.check_output(["ghc", moduleLocation, "-e", ":browse"])
        else:
            interfaceOutput = subprocess.check_output(["ghc", "-e", ":browse " + modName])
    except subprocess.CalledProcessError:
        return None

    if not interfaceOutput:
        return None

    functions = interfaceOutput.splitlines()
    functions = map(_parseInterfaceLine, functions)
    functions = filter(None, functions)
    return {func[0]: func for func in functions}



def _parseInterfaceLine(s):
    def avoidParenSplit(string, sep):
        if "(" not in string:
            return string.split(sep)

        frontParts = string.split("(", 1)
        backParts = frontParts[1].rsplit(")",1)
        front = frontParts[0]
        middle = "(" + backParts[0] + ")"
        back = backParts[1]
        return front.split(sep) + [middle] + back.split(sep)
    def parsePossibleFunction(string):
        if string[0] == "(" and string[-1] == ")":
            string = string[1:-1]
            types = avoidParenSplit(string, "->")
            types = map(str.strip, types)
            types = filter(None, types)
            types = map(parsePossibleFunction, types)
            return tuple(types)
        else:
            return string

    if "::" not in s:
        return None
    # TODO: parse class if present e.g. "Number a => a"
    if "=>" in s:
        # silently ignore
        s = s.split("=>")[1]
    [name, rest] = s.split("::")
    name = name.strip()
    types = avoidParenSplit(rest, "->")
    types = map(str.strip, types)
    types = filter(None, types)
    types = map(parsePossibleFunction, types)
    return (name, types[:-1], types[-1])
    

def loadHaskellModule(moduleName, dirs=["."]):
    ''' moduleName should be of the standard "Data.Int"
        format using in Haskell import statements
        So we need to search the package.conf?
    '''
    return HaskellModule(moduleName, dirs)
