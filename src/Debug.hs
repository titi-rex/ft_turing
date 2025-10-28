module Debug (debug) where

import Debug.Trace

-- Print label and value
debug label value =  trace (show label ++ " " ++ show value)
