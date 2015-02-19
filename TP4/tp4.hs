--TP4 DEBUE Rémy
--Sujet : http://www.fil.univ-lille1.fr/~hym/e/pf/tdtp/tp-arbres.html

import Test.QuickCheck

data Arbre coul val = 	Feuille | 
			Noeud coul val (Arbre coul val) (Arbre coul val) deriving Show
--Exemple d'arbres
a = Noeud 'B' 1 (Noeud 'N' 2 (Feuille) (Noeud 'B' 5 (Feuille) (Feuille))
		) 
		(Noeud 'B' 3 (
				Noeud 'N' 4 (Feuille) (Feuille)
				) 
			(Feuille)
		)

--arbre complet numero 1
acomplet = Noeud 'B' 1 (Noeud 'N' 2 (Feuille) (Feuille)) (Noeud 'B' 3 (Feuille) (Feuille))

--arbre complet numero 2
acomplet2 = Noeud 'B' 1 (Noeud 'N' 2 (Noeud 'B' 3 (Feuille) (Feuille)) (Noeud 'B' 4 (Feuille) (Feuille))) (Noeud 'B' 3 (Noeud 'N' 5 (Feuille) (Feuille)) (Noeud 'B' 6 (Feuille) (Feuille)))

--exemple peigne
peigne = [('B',1),('N',2),('B',3)]

--fonction maptree
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
--version avec Recursion
estComplet :: Arbre c a -> Bool
estComplet (Feuille) 				= False
estComplet (Noeud _ _ (Feuille) (Feuille))	= True
estComplet (Noeud coul val (ag) (ad)) 		= if (taille ag == taille ad )
						then  estComplet ag && estComplet ad
						else
						False

--estComplet' :: Arbre c a -> Bool
--estComplet' a = foldtree (fComplet) (Feuille) a

--fComplet :: Arbre c v -> Arbre c v -> Bool
--fComplet a b = (taille a == taille b)
--Question 9 
complet :: Int -> [(c,a)] -> Arbre c a
complet 0 (c,a) = (Noeud c a (Feuille) (Feuille))
complet n (c,a):rs = (Noeud
--Question 10

fmyst :: a -> [a]
fmyst a = a : fmyst a

fmyst' :: a -> [a]
fmyst' a = iterate (id) a
--fonction qui zip d'un côté les parenthèses et de l'autre côté la liste des caractère de a à .. infini
createCharList :: Char -> [((),Char)]
createCharList a = zip (fmyst ()) ['a' ..]
--Question 12
aplatit :: Arbre c a -> [(c,a)]
aplatit (Feuille) = []
aplatit (Noeud c v (ag) (ad)) = [(c,v)] ++ aplatit (ag) ++ aplatit (ad)

--testaplatit :: (Arbre c a -> [(c,a)]) -> Bool
--testaplatit = return (map snd (aplatit acomplet2) == "abcdefghijklmno")

--Question 13
element :: Eq a => a -> Arbre c a -> Bool
element a Feuille = False
element a (Noeud c v (ag) (ad)) = (a == v) || (element a ag) || (element a ad)
