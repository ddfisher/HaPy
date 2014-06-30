{-# LANGUAGE ForeignFunctionInterface, TemplateHaskell #-}

module Export where
import Foreign.HaPy
import ExampleModule

initHaPy

pythonExport 'foo
pythonExport 'bar
pythonExport 'baz

pythonExport 'arr_arg
pythonExport 'arr_ret
pythonExport 'arr_complex

pythonExport 'string_fun
pythonExport 'char_test
