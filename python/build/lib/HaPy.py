from ctypes import *
import sys

class HaPyImporter:
  def find_module(self, fullname, path=None):
    if (fullname.split('.'))[0] == "HaPy":
      return self
    else:
      raise ImportError

  def load_module(self, fullname):
    module_name = '.'.join((fullname.split('.'))[1:])
    already_loaded = fullname in sys.modules
    if not already_loaded:
      mod = HaskellModule(module_name)
      sys.modules[fullname] = mod
    mod.__file__ = "<%s>" % self.__class__.__name__
    mod.__loader__ = self
    mod.__package__ = fullname.rpartition('.')[0]
    mod.__path__ = None
    return mod

class HaskellModule:
  def __init__(self, module_name):
    self.module_name = module_name
    self.__str__ = module_name
    self.__path__ = []

    self.__lib = cdll.LoadLibrary(module_name + ".so")
    self.__funcs = {}
    self.foreign_types = [c_bool, c_int, c_double, c_char_p]

  def __getattr__(self, name):
    def fun(*args):
      return self.__call_function(name, *args)
    return fun

  def __from_type_enum(self, t):
    return self.foreign_types[t]

  def __func_info(self, name):
    info = self.__lib[name + "__info"]
    info.restype = None

    arr_type = c_int * 100
    arr = arr_type()
    info(arr)
    types = list(arr)
    types = types[:types.index(-1)]
    types = map(self.__from_type_enum, types)

    return_type = types.pop()
    return (types, return_type)

  def __init_function(self, name):
    func = self.__lib[name + "__export"]
    func.argtypes, func.restype = self.__func_info(name)
    return func

  def __call_function(self, name, *args):
    if name not in self.__funcs:
      self.__funcs[name] = self.__init_function(name)
    func = self.__funcs[name]
    if len(args) != len(func.argtypes):
      raise TypeError("this function takes exactly " + str(len(func.argtypes)) + " arguments (" + str(len(args)) + " given)")
    return func(*args)

__path__ = []
sys.meta_path.append(HaPyImporter())
