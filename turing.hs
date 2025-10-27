import Prelude
import Data.List
import Debug.Trace
import Turing.Tape 
import Turing.State
import Turing.Machine
-- import Control.Lens

-- type -> alias for type
-- data -> new type


debug label value =  trace (label ++ show value)

input = "11-11="





main :: IO ()
main = do
    let m = Machine{q=0, tape=fromString input,transitions=tQ, alphabet = "1-=."}

    let q1t1f = func ((tQ !! 0) !! 0)
    let q1t2f = func ((tQ !! 0) !! 1)
    let q1t3f = func ((tQ !! 0) !! 2)
    let q1t4f = func ((tQ !! 0) !! 3)


    let nfunc = choose m
    print nfunc

    print . q $ m
    print . tape $ m
   
    print . q  . next $ m
    print . tape . next $ m
--
--    print . q  . q1t1f . q1t1f $ m
--    print . tape  . q1t1f . q1t1f $ m
--
--    print . q  . q1t2f . q1t1f . q1t1f $ m
--    print . tape  . q1t2f . q1t1f . q1t1f $ m



-- writeTape tape head letter = set (element head) letter tape


--let (a,b) = splitAt 3 "foobar" in b ++ a 
-- newTape tape position newSymbol =
--    take (position) tape ++ newSymbol : drop (position+1) tape






