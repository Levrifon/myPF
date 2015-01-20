--DEBUE Rémy
--Sujet http://www.fil.univ-lille1.fr/~hym/e/pf/tdtp/tp2.html
import Graphics.Gloss

alternate :: [a] -> [a]
alternate [] = []
alternate [x] = [x]
alternate (x:y:xs) = x : alternate xs

combine :: (a -> b -> c) -> [a] -> [b] -> [c]
combine f x  [] = [] 
combine f [] x  = []
combine f (x:xs) (y:ys) = f x y : combine f (xs) (ys) 

pasPascal :: [Integer] -> [Integer]
pasPascal [] = []
pasPascal xs = zipWith (+) ([0] ++ xs)(xs ++ [0]) 

pascal :: [[Integer]]
pascal = iterate pasPascal [1]

pointAintercaler :: Point -> Point -> Point
pointAintercaler (x1,y1) (x2,y2) = ((x1 + x2)/2 + (y2 - y1)/2,(y1+y2)/2 + (x1 - x2)/2)

pasDragon :: Path -> Path
pasDragon [] 		= []
pasDragon [x] 		= [x]
pasDragon [x,y]		= x : pointAintercaler x y  : [y]
pasDragon (x:y:z:s)	= x : pointAintercaler x y : y : pointAintercaler z y : pasDragon (z:s)


main = animate (InWindow "Dragon" (500, 500) (0, 0)) white (dragonAnime (50,250) (450,250))

dragonAnime a b t = Line (dragon a b !! (round t `mod` 20))

dragon x y = iterate pasDragon [x,y]

dragonOrdre :: Point -> Point -> Int -> Path

dragonOrdre p1 p2  0 	= [p1,p2]
dragonOrdre p1 p2 n	= let c = (pointAintercaler p1 p2)
                          in (dragonOrdre p1 c (n-1))++(dragonOrdre p2 c (n-1))

