from ctypes import *
import sys
import glob

class HaPyImporter:
  def find_module(self, fullname, path=None):
    if (fullname.split('.'))[0] == "HaPy":
      return self
    else:
      return None

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

    module_paths = (glob.glob("./" + module_name + ".so")
                 + glob.glob("./" + module_name + ".dylib")
                 + glob.glob("./libHS" + module_name + "*.so")
                 + glob.glob("./libHS" + module_name + "*.dylib"))
    if not module_paths:
        raise RuntimeError("Haskell module '" + module_name + "' not found!")
    module_path = module_paths[0]
    self.__lib = cdll.LoadLibrary(module_path)
    self.__funcs = {}
    self.__ftype_map = { "Bool"   : c_bool
                       , "Char"   : c_char
                       , "Int"    : c_int
                       , "Double" : c_double
                       }
    self.__free = self.__lib["free"]
    self.__free.argtypes = [c_void_p]
    self.__free.restype = None

  def __getattr__(self, name):
    def fun(*args):
      return self.__call_function(name, *args)
    return fun

  def __get_types(self, info_str):
    def convert_type(type_str):
      container_strs = type_str.split()
      basic_type_str = container_strs.pop()
      basic_type = self.__ftype_map.get(basic_type_str)
      if not basic_type:
        raise TypeError("Unknown type: " + basic_type_str)
      # Lists are currently the only supported container
      for c in container_strs:
        if c != "List":
          raise TypeError("Unknown container type: " + c)
      if not container_strs:
        return SimpleType(basic_type)
      else:
        return ListType(basic_type, len(container_strs), self.__free)

    types = info_str.split(";")
    types = map(convert_type, types)
    return_type = types.pop()
    return (types, return_type)

  def __func_info(self, name):
    info = HaskellFunction(self.__lib[name + "__info"],
                           [],
                           ListType(c_char, 1, self.__free))
    return self.__get_types(info())

  def __init_function(self, name):
    func = self.__lib[name + "__export"]
    arg_types, ret_type = self.__func_info(name)
    return HaskellFunction(func, arg_types, ret_type)

  def __call_function(self, name, *args):
    if name not in self.__funcs:
      self.__funcs[name] = self.__init_function(name)
    return self.__funcs[name](*args)

class HaskellFunction:
  def __init__(self, func, arg_types, ret_type):
    func.argtypes = map(lambda t: t.ctype(), arg_types)
    func.restype = ret_type.ctype()

    self.__func = func
    self.__arg_types = arg_types
    self.__ret_type = ret_type

  def __call__(self, *args):
    if len(args) != len(self.__arg_types):
      raise TypeError("this function takes exactly "
                      + str(len(func.argtypes))
                      + " arguments (" + str(len(args)) + " given)")
    conv_args = [t.convert_arg(a) for (t, a) in zip(self.__arg_types, args)]
    ret = self.__func(*conv_args)
    return self.__ret_type.convert_ret(ret)

__path__ = []
sys.meta_path.append(HaPyImporter())


def list_struct_t(typ, length):
  class ListStruct(Structure):
    _fields_ = [ ("length", c_int)
               , ("array", typ * length)
               ]
  return ListStruct

def extract_array(struct, typ):
  length = cast(struct, POINTER(c_int)).contents.value
  if typ == c_char:
    # handle strings separately because otherwise they'll be NULL terminated
    # (which isn't correct in this case as we have the exact length)
    contents = string_at(struct + sizeof(c_int), length)
  else:
    contents = cast(struct, POINTER(list_struct_t(typ, length))).contents.array
  return contents

def list_to_struct(lst, depth, base_type):
  length = len(lst)
  StructT = list_struct_t(base_type if depth == 1 else c_void_p, length)
  struct = StructT()
  struct.length = length
  if depth == 1:
    # For reasons unknown, ctypes changes arrays of c_chars into strings when
    # they're in structs.  Strings are immutable, so we cannot assign to their
    # indices: we replace them whole instead.  Furthermore, we need to copy
    # the string byte for byte - otherwise NULL characters will cause the string
    # copy to terminate early.
    if base_type == c_char:
      memmove(byref(struct, sizeof(c_int)), create_string_buffer(lst, length), length)
    else:
      for i in range(length):
        struct.array[i] = lst[i]
  else:
    for i in range(length):
      struct.array[i] = cast(list_to_struct(lst[i], depth - 1, base_type), c_void_p)
  return pointer(struct)

def struct_to_list(struct, depth, base_type):
  if depth == 1:
    return list(extract_array(struct, base_type))
  else:
    return map(lambda s: struct_to_list(s, depth - 1, base_type),
               extract_array(struct, c_void_p))

def free_struct(struct, depth, free):
  if depth == 1:
    free(struct)
  else:
    for s in extract_array(struct, c_void_p):
      free_struct(s, depth - 1, free)
    free(struct)

class SimpleType:
  def __init__(self, typ):
    self.__type = typ

  def ctype(self):
    return self.__type

  def convert_arg(self, arg):
    return arg

  def convert_ret(self, ret):
    return ret

class ListType:
  def __init__(self, base_type, depth, free):
    self.__base_type = base_type
    self.__depth = depth
    self.__free = free

  def ctype(self):
    return c_void_p

  def convert_arg(self, arg):
    return list_to_struct(arg, self.__depth, self.__base_type)

  def convert_ret(self, ret_struct):
    ret = struct_to_list(ret_struct, self.__depth, self.__base_type)
    free_struct(ret_struct, self.__depth, self.__free)
    if self.__base_type == c_char and self.__depth >= 1:
      return self.__fix_string_list(ret)
    else:
      return ret

  def __fix_string_list(self, lst):
    def wrap(f):
      return lambda x: map(f, x)
    def conv(lst):
      return "".join(lst)
    c = conv
    for i in range(self.__depth - 1):
      c = wrap(c)
    return c(lst)
