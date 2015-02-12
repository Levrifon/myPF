--TP4 DEBUE Rémy
--Sujet : http://www.fil.univ-lille1.fr/~hym/e/pf/tdtp/tp-arbres.html

import Test.QuickCheck

data Arbre coul val = 	Feuille | 
			Noeud coul val (Arbre coul val) (Arbre coul val) deriving Show
--Exemple d'arbres
a = Noeud 'B' 1 (Noeud 'N' 2 (Feuille) (
				Noeud 'B' 5 (Feuille) (Feuille))
		) 
		(Noeud 'B' 3 (
				Noeud 'N' 4 (Feuille) (Feuille)
				) 
			(Feuille)
		)

peigne = [('B',1),('N',2),('B',3)]
maptree :: (a -> b) -> Arbre c a -> Arbre c b
maptree f (Feuille) 		= Feuille
maptree f (Noeud c v (a1) (a2)) = Noeud c (f v) (maptree f a1) (maptree f a2)

foldtree :: (a -> b -> b -> b) -> b -> Arbre c a -> b
foldtree f base Feuille 		= base
foldtree f base (Noeud c v (a1) (a2))	= f v (foldtree f base a1) (foldtree f base a2)

--Hauteur version récursive
hauteur :: Arbre c v -> Int
hauteur (Feuille) = 0
hauteur (Noeud coul val (ag) (ad)) = 1 + max (hauteur ag) (hauteur ad)

--Hauteur version fold et utilisation de la fonction fhauteur
hauteur' :: Arbre c v -> Int
hauteur' a = foldtree (fhauteur) 0 a
fhauteur _ b c = 1 + max b c

--Taille version récursive
taille :: Arbre c v -> Int
taille (Feuille) = 0
taille (Noeud coul val (ag) (ad))= 1 + taille ag + taille ad
--Taille version avec fold
taille' :: Arbre c v -> Int
taille' a = foldtree (ftaille) 0 a
--fonction ftaille appellé dans la fonction taille' (version fold)
ftaille :: a -> Int -> Int-> Int 
ftaille _ b c = 1 + b + c 

--fonction peigneGauche
peigneGauche :: [(c,a)] -> Arbre c a
peigneGauche [] 	= Feuille
peigneGauche ((c,a):as)	= Noeud c a (peigneGauche as) (Feuille) 


prop_hauteurPeigne xs 	= length xs == hauteur (peigneGauche xs)

estComplet :: Arbre c a -> Bool
estComplet (Feuille) = True
estComplet a = if ((taille a `log` 2) == 0 )
		then  True
		else
		 False
