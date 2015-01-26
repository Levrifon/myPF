import Graphics.Gloss
type Symbole  	= Char
type Mot     	= [Symbole]
type Axiome  	= Mot
type Regles   	= Symbole -> Mot
type LSysteme 	= [Mot]
type EtatTortue = (Point, Float)
type EtatDessin = (EtatTortue, Path)
motSuivant :: Regles -> Mot -> Mot
--Question 1 : Recursive
motSuivant reg [] 		= []
motSuivant reg  (lettre:mot) 	= reg lettre ++ motSuivant reg mot
--Question 1.1 : Liste Comprehension
motSuivant' :: Regles -> Mot -> Mot
motSuivant' reg [] 	= []
motSuivant' reg xs 	= [lettre2 | lettre <- xs, lettre2 <- reg lettre]
--Question 1.2 : Fonction Prelude
motSuivant'' :: Regles -> Mot -> Mot
motSuivant'' reg [] 	= []
motSuivant'' reg mot 	= concat (map reg mot)

--Question 2 
flocon '+' = "+"
flocon '-' = "-"
flocon 'F' = "F-F++F-F"

--Question 3
lsysteme :: Axiome -> Regles -> LSysteme 
lsysteme ax reg = iterate (motSuivant'' reg) ax
--Question 4
type Config = (EtatTortue -- État initial de la tortue
              ,Float      -- Longueur initiale d’un pas
              ,Float      -- Facteur d’échelle
              ,Float      -- Angle pour les rotations de la tortue
              ,[Symbole]) -- Liste des symboles compris par la tortue
              
etatInitial :: Config -> EtatTortue
etatInitial (etat,_,_,_,_) = etat

longueurPas :: Config -> Float
longueurPas (_,linitial,_,_,_) = linitial

facteurEchelle :: Config -> Float
facteurEchelle (_,_,echelle,_,_) = echelle

angle :: Config -> Float
angle (_,_,_,angle,_) = angle

symbolesTortue :: Config -> [Symbole]
symbolesTortue (_,_,_,_,symbtort) = symbtort
--Question 5
avance :: Config -> EtatTortue -> EtatTortue
avance c ((x,y),cap) = ( ( (x+longueurPas c*(cos cap)) ,(y+longueurPas c*(sin cap) ) ), cap )   
--Question 6
tourneAGauche :: Config -> EtatTortue -> EtatTortue
tourneAGauche c ((x,y),cap) = ((x,y),cap + angle c)
tourneADroite :: Config -> EtatTortue -> EtatTortue
tourneADroite c ((x,y),cap) = ((x,y),cap - angle c)
--Question 7
filtreSymbolesTortue :: Config -> Mot -> Mot
filtreSymbolesTortue c mot = [s | s <- mot, s `elem` (symbolesTortue c) ]
--Question 8
interpreteSymbole :: Config -> EtatDessin -> Symbole -> EtatDessin
interpreteSymbole c (etatortue,path) '+' = (tourneAGauche c etatortue,path)
interpreteSymbole c (etatortue,path) '-' = (tourneADroite c etatortue,path)
interpreteSymbole c (etatortue,path) 'F' = (avance c etatortue,path)
--Question 9
--interpreteMot :: Config -> Mot -> Picture
--interpreteMot cfg (c:m) = line (cfg)
--dessin = interpreteMot (((-150,0),0),100,1,pi/3,"F+-") "F+F--F+F"

main = display (InWindow "L-système" (1000, 1000) (0, 0)) white dessin
--enieme = round instant `mod` 10

 

