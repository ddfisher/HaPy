{-# LANGUAGE ForeignFunctionInterface, TemplateHaskell #-}

module Export where
import Foreign.HaPy
import ExampleModule

pythonExport 'foo
pythonExport 'bar
pythonExport 'baz
