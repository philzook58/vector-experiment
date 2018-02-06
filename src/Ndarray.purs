module Ndarray where
import Data.ArrayBuffer.Types

foreign import data Ndarray :: Type -> Type

foreign import fill :: Float64 -> Int -> NDarray Float64
--foreign import fill64 :: Number -> Int -> NDarray Number
-- forall tag. NDArray tag Number
-- Does this allow one that add type level tags but you don't have to if you don't want to? 
-- Proxy -> --
-- or Can 
-- fillInt
class BufferType a where


instance float64Type :: BufferType Float64 where


foreign import add_internal :: Float64 -> Int -> NDarray Float64

foreign import ndarray :: Array -> NDArray  
-- perhaps a typeclass? Array Array Array -> NDArray also

-- slices as foreign imported lens




