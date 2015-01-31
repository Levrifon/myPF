import Graphics.Gloss
type Symbole  	= Char
type Mot     	= [Symbole]
type Axiome  	= Mot
type Regles   	= Symbole -> Mot
type LSysteme 	= [Mot]
type EtatTortue = (Point, Float)
type EtatDessin = ([EtatTortue], [Path])
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

getActualEtatDessin :: EtatDessin -> (EtatTortue,Path)
getActualEtatDessin (e:ets , p:pths) = (e,p)

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
interpreteSymbole c (etatortue,path) 'F' = ((avance c etatortue), getPointFromEtatTortue etatortue : getPointFromEtatTortue(avance c etatortue):[])

getPointFromEtatTortue :: EtatTortue -> Point
getPointFromEtatTortue (p,_) = p

--Question 9
interpreteMot :: Config -> Mot -> Picture
--interpreteMot prend en parametre une config et un mot
interpreteMot cfg mot = line (interpreteMot' cfg mot ((etatInitial cfg,[])))

interpreteMot' :: Config -> Mot -> EtatDessin -> Path
interpreteMot' cfg [] (ett,pth) = pth
--j'interpreteMot sur l'ensemble des symboles : donc je concatene le path avec l'interpretation du prochain Symbole "tourne a droite" (par exemple) qui mettra à jour la tortue et son path.
interpreteMot' cfg (carac:mot) (ett,pth) = pth ++ (interpreteMot' cfg mot (interpreteSymbole cfg (ett,pth) carac))
--(ett,pth) correspond a l'etat du Dessin (etatortue,path)

dessin = interpreteMot (((-150,0),0),100,1,pi/3,"F+-") "F+F--F+F+"

main = animate(InWindow "L-systeme" (1000, 1000) (0, 0)) white vonKoch1Anime
--Question 10 
lsystemeAnime :: LSysteme -> Config -> Float -> Picture
lsystemeAnime lsys cfg t = interpreteMot cfg (lsys !! (round t `mod` 10))


vonKoch1 :: LSysteme
vonKoch1 = lsysteme "F" regles
    where regles 'F' = "F-F++F-F"
          regles  s  = [s]

vonKoch2 :: LSysteme
vonKoch2 = lsysteme "F++F++F++" regles
    where regles 'F' = "F-F++F-F"
          regles  s  = [s]

hilbert :: LSysteme
hilbert = lsysteme "X" regles
    where regles 'X' = "+YF-XFX-FY+"
          regles 'Y' = "-XF+YFY+FX-"
          regles  s  = [s]

dragon :: LSysteme
dragon = lsysteme "FX" regles
    where regles 'X' = "X+YF+"
          regles 'Y' = "-FX-Y"
          regles  s  = [s]

vonKoch1Anime :: Float -> Picture
vonKoch1Anime = lsystemeAnime vonKoch1 (((-400, 0), 0), 800, 1/3, pi/3, "F+-")

vonKoch2Anime :: Float -> Picture
vonKoch2Anime = lsystemeAnime vonKoch2 (((-400, -250), 0), 800, 1/3, pi/3, "F+-")

hilbertAnime :: Float -> Picture
hilbertAnime = lsystemeAnime hilbert (((-400, -400), 0), 800, 1/2, pi/2, "F+-")

dragonAnime :: Float -> Picture
dragonAnime = lsystemeAnime dragon (((0, 0), 0), 50, 1, pi/2, "F+-")


