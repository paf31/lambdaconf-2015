-- | This module defines types for some global Javascript functions
-- | and values.

module Global
  ( nan
  , isNaN
  , infinity
  , isFinite
  , readInt
  , readFloat
  ) where

-- | Not a number (NaN)
foreign import nan "var nan = NaN;" :: Number

-- | Test whether a number is NaN
foreign import isNaN :: Number -> Boolean

-- | Positive infinity
foreign import infinity "var infinity = Infinity;" :: Number

-- | Test whether a number is finite
foreign import isFinite :: Number -> Boolean

-- | Parse an integer from a `String` in the specified base
foreign import readInt
  """
  function readInt(radix) {
    return function(n) {
      return parseInt(n, radix);
    };
  }
  """ :: Number -> String -> Number

-- | Parse a floating point value from a `String`
foreign import readFloat "var readFloat = parseFloat;" :: String -> Number

