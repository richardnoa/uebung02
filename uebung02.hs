import Data.Char  (digitToInt)
import Data.Maybe (listToMaybe)
import Numeric    (readInt)

type Nibble = (Bool, Bool, Bool, Bool)

showNibble :: Nibble -> String
showNibble (a0,a1,a2,a3)
 = show(fromEnum a0 )++show(fromEnum a1)++show(fromEnum a2)++show(fromEnum a3)++ " : "++show(fromEnum a0 * 8 + fromEnum a1 *4 + fromEnum a2 * 2 + fromEnum a3)++ " : " ++show(komplement (a0,a1,a2,a3))

komplement :: Nibble -> Int
komplement (a,b,c,d)
 | a == True = -8 + fromEnum b *4 + fromEnum c * 2 + fromEnum d * 1
 | a == False = fromEnum a *8 + fromEnum b * 4 + fromEnum c * 2 + fromEnum d * 1

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

fulladder :: Bool -> Bool -> Bool ->(Bool, Bool)
fulladder a b c =  ((c && (xor a b)) || (a&&b) , xor (xor a b) c)

-- rippleCarryAdder :: Nibble -> Nibble -> Nibble
-- rippleCarryAdder (a0,a1,a2,a3) (b0,b1,b2,b3)
-- = (fst (fulladder a0 b0 False), fst (fulladder a1 b1 (snd (fulladder a0 b0 False))), fst (fulladder a2 b2 (snd (fulladder a1 b1 (snd (fulladder a0 b0 False))))), fst (fulladder a3 b3 (snd(fulladder a2 b2 (snd (fulladder a1 b1 (snd (fulladder a0 b0 False))))))))

rippleCarryAdder :: Nibble -> Nibble -> Nibble
rippleCarryAdder (a,b,c,d) (w,x,y,z) = (snd(fulladder a w (fst(fulladder b x (fst(fulladder c y (fst(fulladder d z False))))))), snd(fulladder b x (fst(fulladder c y (fst(fulladder c z False ))))), snd(fulladder c y (fst(fulladder d z False))), snd(fulladder d z False))

tableAdder :: (Nibble -> Nibble -> Nibble) -> [(Nibble , Nibble)] -> String
tableadder _ [] =""
tableAdder f xs
 |tail (xs) == [] = show(showNibble(fst(head (xs)))) ++ ";" ++ show(showNibble(snd(head(xs)))) ++ "=" ++ show (showNibble(f (fst(head(xs))) (snd(head(xs)))))
 |otherwise = show(showNibble(fst(head (xs)))) ++ ";" ++ show(showNibble(snd(head(xs)))) ++ "=" ++ show(showNibble(f(fst(head(xs))) (snd(head(xs))))) ++ "\n" ++ tableAdder f (tail(xs))

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
    putStrLn ( tableAdder rippleCarryAdder[(( True , False , False , True ),( False , False , False , True ))] )
