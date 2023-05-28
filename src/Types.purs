module Types where

import Prelude

foreign import data Client :: Type
foreign import data Chat :: Type
foreign import data Message :: Type
foreign import data Contact :: Type

data TrafficLight = Red | Yellow | Green | White

instance Show TrafficLight where
  show Red = "🔴"
  show Yellow = "🟡"
  show Green = "🟢"
  show White = "⚪"
