module Data.ArrayBuffer.Types where


-- | Represents a JS ArrayBuffer object
foreign import data ArrayBuffer :: *

-- | Represents a JS DataView on an ArrayBuffer (a slice into the ArrayBuffer)
foreign import data DataView :: *

-- | The unifying representation for the different typed arrays
foreign import data ArrayView :: * -> *


-- | Offset in bytes into a DataView or ArrayBufer
type ByteOffset = Number

-- | Length in bytes of a DataView or ArrayBuffer
type ByteLength = Number


data Int8
data Int16
data Int32
data Uint8
data Uint16
data Uint32
data Uint8Clamped
data Float32
data Float64

type Int8Array = ArrayView Int8
type Int16Array = ArrayView Int16
type Int32Array = ArrayView Int32
type Uint8Array = ArrayView Uint8
type Uint16Array = ArrayView Uint16
type Uint32Array = ArrayView Uint32
type Uint8ClampedArray = ArrayView Uint8Clamped
type Float32Array = ArrayView Float32
type Float64Array = ArrayView Float64 





       
