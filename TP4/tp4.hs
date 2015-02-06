--TP4 DEBUE Rémy
data Arbre coul val = 	Feuille | 
			Noeud coul val (Arbre coul val) (Arbre coul val) deriving Show
--Exemple d'arbre
a = Noeud 'B' 1 (Noeud 'N' 2 (Feuille) (
				Noeud 'B' 5 (Feuille) (Feuille))
		) 
		(Noeud 'B' 3 (
				Noeud 'N' 4 (Feuille) (Feuille)
				) 
			(Feuille)
		)
maptree :: (a -> b) -> Arbre c a -> Arbre c b
maptree f (Feuille) 		= Feuille
maptree f (Noeud c v (a1) (a2)) = Noeud c (f v) (maptree f a1) (maptree f a2)

foldtree :: (a -> b -> b) -> b -> Arbre c a -> b
foldtree f  n Feuille 			= n
foldtree f  n (Noeud c v (a1) (a2))	= f n a1 f n a2 		
