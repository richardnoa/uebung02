import Data.Char  (digitToInt)
import Data.Maybe (listToMaybe)
import Numeric    (readInt)

type Nibble = (Bool, Bool, Bool, Bool)

showNibble :: Nibble -> String
showNibble (a0,a1,a2,a3)
 = show(fromEnum a0 )++show(fromEnum a1)++show(fromEnum a2)++show(fromEnum a3)++ " : "++show(fromEnum a0 * 8 + fromEnum a1 *4 + fromEnum a2 * 2 + fromEnum a3 * 1)++ " : " ++show(fromEnum a0 *(-1)*( fromEnum a1 *4 + fromEnum a2 * 2 + fromEnum a3 * 1))

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

fulladder :: Bool -> Bool -> Bool ->(Bool, Bool)
fulladder a b c =  (xor c (xor a b) , (a&&b) || ((xor a b) && c) )

rippleCarryAdder :: Nibble -> Nibble -> Nibble
rippleCarryAdder (a0,a1,a2,a3) (b0,b1,b2,b3)
 = (fst (fulladder a0 b0 False), fst (fulladder a1 b1 (snd (fulladder a0 b0 False))), fst (fulladder a2 b2 (snd (fulladder a1 b1 (snd (fulladder a0 b0 False))))), fst (fulladder a3 b3 (snd(fulladder a2 b2 (snd (fulladder a1 b1 (snd (fulladder a0 b0 False))))))))

listOfTuToTu :: [(Nibble,Nibble)] -> (Nibble,Nibble)
listOfTuToTu [(x,y)] = (x,y)

tableAdder :: (Nibble -> Nibble -> Nibble) -> [(Nibble, Nibble)] -> String
tableAdder f [(x,y)]
 = f fst(listOfTuToTu [(x,y)]) snd (listOfTuToTu [(x,y)])

aufgabe1 = do 
    putStrLn("#### Aufgabe 1 ####")
    putStrLn("#### showNibble ####")
    putStrLn(showNibble (True,True,True,True))
    putStrLn(showNibble (False,False,False,False))
    putStrLn(showNibble (True,False,False,False))
    putStrLn(showNibble (True,False,False,True))
    putStrLn("#### Aufgabe 2 ####")
    putStrLn("#### fulladder ####")
    putStrLn(show(fulladder True True True))
    putStrLn(show(fulladder False False True))
    putStrLn(show(fulladder False True False))
    putStrLn(show(fulladder True False False))
    putStrLn(show(fulladder False False False))
    putStrLn("#### Aufgabe 3 ####")
    putStrLn("#### rippleCarryAdder ####")
    putStrLn(show(rippleCarryAdder (True,True,True,True) (True,True,True,True)))
    putStrLn(show(rippleCarryAdder (True,True,True,True) (False,False,False,False)))
    putStrLn(show(rippleCarryAdder (False,False,False,False) (True,True,True,True)))
    putStrLn(show(rippleCarryAdder (True,False,False,False) (True,False,False,False)))
    putStrLn(show(rippleCarryAdder (True,False,False,False) (True,False,False,True)))
    putStrLn("#### Aufgabe 4 ####")
    putStrLn("#### tableAdder ####")
    putStrLn(show(listOfTuToTu [((True , False, False, True),(False, False, False, True))]))
