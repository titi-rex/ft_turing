module Encoding
  ( encodeSymbol,
    unaryEncodeNumber,
    unaryEncodeAction,
  )
where

import Data.List
import Data.Maybe
import Tape

-- Encoded symbol representation
type EncodedSymbol = Int

-- Encode Symbol to EncodedSymbol
-- example: '1' -> 1, '-' -> 2, '.' -> 3
encodeSymbol :: [Symbol] -> Symbol -> EncodedSymbol
encodeSymbol alp symbol = (fromMaybe (2) $ elemIndex symbol alp) + 1

-- Encode a String into its [EncodedSymbol] representation
encodeSymbolString :: [Symbol] -> String -> [EncodedSymbol]
encodeSymbolString alp str = map (encodeSymbol alp) str

-- Return an unary encoding of an Int
unaryEncodeNumber :: EncodedSymbol -> String
unaryEncodeNumber 0 = ""
unaryEncodeNumber q = "0" ++ unaryEncodeNumber (q - 1)

-- Return an unary encoding of an Action
unaryEncodeAction :: Action -> String
unaryEncodeAction LEFT = "0"
unaryEncodeAction RIGHT = "00"
