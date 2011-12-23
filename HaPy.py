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

def applyListFun(cType, applyFun):
    def convertIntList(fun, lst):
        array = (cType * len(lst))()
        array[:] = lst
        return applyFun(fun, len(lst), array)
    return convertIntList

def retrieveListFun(cType, retrieveFun):
    def retrieveList(obj):
        retrieveFun.restype = POINTER(cType)
        array = retrieveFun(obj)
        length = hapy.retrieveListLength(obj)
        lst = array[:length]
        hapy.freeArray(array)
        return lst
    return retrieveList

_types = [ HaskellType("Int"    , int   , hapy.applyInt    , hapy.retrieveInt)
         , HaskellType("Bool"   , bool  , hapy.applyBool   , hapy.retrieveBool)
         , HaskellType("Double" , float , lambda p, f: hapy.applyDouble(p, c_double(f)) , hapy.retrieveDouble)
        # , HaskellType("Double" , float , hapy.applyDouble , hapy.retrieveDouble)
         , HaskellType("String" , str   , hapy.applyString , hapy.retrieveString)
         , HaskellType("[Char]" , str   , hapy.applyString , hapy.retrieveString)
        , HaskellType("[Bool]"   , list  , applyListFun(c_bool, hapy.applyBoolList)   , retrieveListFun(c_bool, hapy.retrieveBoolList))
        , HaskellType("[Int]"   , list  , applyListFun(c_int, hapy.applyIntList)   , retrieveListFun(c_int, hapy.retrieveIntList))
        , HaskellType("[Double]"   , list  , applyListFun(c_double, hapy.applyDoubleList)   , retrieveListFun(c_double, hapy.retrieveDoubleList))
         ]
_KNOWN_TYPES = { type.cls:type.name for type in _types }
_TYPE_INFO = { type.name:type for type in _types }

def _isGeneric(type):
    return isinstance(type, str) and type[0].islower()

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
    def __init__(self, objPtr, typeStr):
        self._ptr = objPtr
        self.typeStr = typeStr
        self._finalizer = weakref.ref(self, lambda wr, ptr=self._ptr: hapy.freePtr(ptr))

    def __repr__(self):
        return self.__class__.__name__ + "(" + self.typeStr + ")"

    def isFullyApplied(self):
        return "->" not in self.typeStr

    def __call__(self, *allArgs, **kwargs):
        typecheck = kwargs.get("typecheck", True)

        if not allArgs:
            return self._retrieve()

        arg = allArgs[0]
        args = allArgs[1:]
        if self.isFullyApplied():
            return RuntimeError("Too many arguments")

        argType = _getType(arg)
        if argType is None:
            raise TypeError("Cannot handle python object of class " + arg.__class__)

        newType = _typecheck(self.typeStr, argType)
        if newType:
            if argType in _TYPE_INFO:
                applyFun = _TYPE_INFO[argType].applyFun
                return HaskellObject(applyFun(self._ptr, arg), newType)(*args)
            else:
                return HaskellObject(hapy.applyOpaque(self._ptr, arg._ptr), newType)(*args)
        else:
            raise TypeError("Function of type \"" + self.typeStr + "\" does not accept argument of type \"" + argType + "\"")


    def _retrieve(self):
        # TODO: case that the last argument is an unresolved generic type (rare)
        if self.isFullyApplied() and self.typeStr in _TYPE_INFO:
            return _TYPE_INFO[self.typeStr].retrieveFun(self._ptr)
        else: 
            return self

def _getType(arg):
    if isinstance(arg, HaskellObject):
        return arg.typeStr
    elif arg.__class__ in _KNOWN_TYPES:
        return _KNOWN_TYPES[arg.__class__]
    else:
        return None

def _typecheck(typeStr, argType):
    fullTypeStr = "(undefined :: " + typeStr + ") (undefined :: " + argType + ")"
    typeCheckOutput = subprocess.check_output(["ghc", "-e", ":type " + fullTypeStr])
    print fullTypeStr
    print typeCheckOutput
    if "<interactive>" in typeCheckOutput:
        return None
    else:
        return typeCheckOutput.split("::")[-1].replace("\n", "").strip()

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
                return HaskellObject(symPtr, self.interface[name])()
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

    interfaceOutput = interfaceOutput.replace("\n ", " ")
    functions = interfaceOutput.splitlines()
    functions = map(_parseInterfaceLine, functions)
    functions = filter(None, functions)
    return dict(functions)



def _parseInterfaceLine(s):
    if "::" not in s or s.startswith("data") or s.startswith("class"):
        return None
    print s
    [name, typeStr] = s.split("::")
    name = name.strip()
    typeStr = typeStr.strip()
    return (name, typeStr)
    

def loadHaskellModule(moduleName, dirs=["."]):
    ''' moduleName should be of the standard "Data.Int"
        format using in Haskell import statements
        So we need to search the package.conf?
    '''
    return HaskellModule(moduleName, dirs)
