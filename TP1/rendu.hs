-- Version Décuryfiée
sommeDeXaY(x,y) = if x <= y then
			x + sommeDeXaY(x+1,y)
		else
			0
-- Version Curyfiée			
sommeDeXaY' x y = if x <= y then
			x + sommeDeXaY' (x+1) y
		else
			0
-- Version Simple			
somme xs = sum[head xs.. last xs]
-- Version manuelle
somme' [] = 0
somme' (x:xs) = x + somme' (xs)

mylast xs = head(reverse xs)

myinit xs = reverse (drop 1 (reverse xs))

mygetindex (x:xs) 0 = x
mygetindex (x:xs) n = mygetindex(xs) (n-1)
mygetindex [] n = 0

myconcatenate [] ys = ys
myconcatenate (x:xs) ys = x:(myconcatenate xs ys)

mylistconcat [] = []
mylistconcat (x:xs) = myconcatenate x(mylistconcat xs)

mymap t (x:xs) = t x : mymap t xs 
mymap t [] = []

--Q7 permet de créer notre propre fonction qui permet d'avoir n'importe quel element de la liste
-- ex : x 2 -> 3 (avec l = [1,2,3])

--Q8

mylength xs = sum (map (\x -> 1) xs)

--Q9
myfunction f 0 x = []
myfunction f 1 x = [x]
myfunction f n x = [x]++(myfunction f (n-1) (f x))

myfunction' f n x = take n (iterate f x)


mylist n = myfunction (+1) (n+1) 0

  
  
  
	
