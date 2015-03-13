import Parser
--Debue Rémy Groupe 3
type Nom = String

data Expression = Lam Nom Expression
                | App Expression Expression
                | Var Nom
                | Lit Litteral
                deriving (Show,Eq)

data Litteral = Entier Integer
              | Bool   Bool
              deriving (Show,Eq)

--Question 1 on detecte 0 ou n espaces si c'est le cas on continue sinon on retourne directement l'etat actuel
espacesP :: Parser ()
espacesP = (zeroOuPlus (car ' ') >>= \_ -> return ()) ||| (return ())

--Question 2 on verifie si il y a un ou n caractères de l'alphabet on enlève ensuite les espaces et on retourne ce resultat
nomP :: Parser Nom
nomP = (unOuPlus (carCond (flip elem ['a'..'z']))) >>= \r ->
       (espacesP)				   >>= \_ ->
       return r
       
--Question 3 on apelle simplement nomP on recupère le resultat et on renvoie une variable avec le resultat
varP :: Parser Expression
varP = nomP >>= \r -> return (Var r)

--Question 4 je distingue ici les différents cas (1 / 2 / n expreessions)
applique :: [Expression] -> Expression
applique [] = undefined
applique (e:[]) = e
applique (e:e2:[]) = App e e2
applique es = App (applique (init es)) (last es)

--Question 5 (obsolete) 
--exprP :: Parser Expression
--exprP = varP

exprsP :: Parser Expression
exprsP = (unOuPlus exprP) >>= \r -> return (applique r)

--Question 6 ici on verifie si il y a bien le caractere 'fleche' précédés/suivis d'eventuels espaces
lambdaP :: Parser Expression
lambdaP = (car '\\' >>= \_ ->
          espacesP      >>= \_ 	->
          nomP 		>>= \r 	-> 
          car '-' 	>>= \_ 	->
          car '>' 	>>= \_ 	->
          espacesP 	>>= \_	->
          exprsP 	>>= \s 	-> return (Lam r s))
          

lambdaP' = do car '\\' 
              espacesP      
              r <- nomP
              car '-'
              car '>'
              espacesP
              s <- exprsP
              return (Lam r s)
          
--Question 7

--Question 8 coresspond a une parenthese ouvrante suivie d'une expression avec espaces après ou non puis la parenthèse fermante
exprParentheseeP :: Parser Expression
exprParentheseeP =	(	car '(' >>= \_	-> 
				exprsP 	>>= \r	->
				espacesP>>= \_	->
				car ')' >>= \_	->
				return r
			)
		
--exprP = (varP ||| lambdaP ||| exprParentheseeP) >>= \r -> espacesP >>= \_ -> return r

nombreP :: Parser Expression
nombreP = (unOuPlus (carCond (flip elem ['0'..'9'])))	>>= \r -> espacesP >>= \_ -> return (Lit (Entier (read r)))
-- Pour booleen je n'ai pas trouvé d'autres solutions que créer deux fonctions intermédiaires, Alexandre MOEVI m'a aidé pour cette question
booleenP :: Parser Expression
booleenP = (isTrue ||| isFalse)

isTrue ::  Parser Expression
isTrue	= (chaine "True" >>= \_ 	-> return (Lit (Bool (True))))

isFalse :: Parser Expression
isFalse = (chaine "False" >>= \_	-> return (Lit (Bool (False))))
--Question 10 on étend simplement comme précédemment
exprP = (varP ||| lambdaP ||| exprParentheseeP ||| nombreP ||| booleenP) >>= \r -> espacesP >>= \_ -> return r

expressionP :: Parser Expression
expressionP = espacesP >>= \_ -> exprsP
--Question 12
ras :: String -> Expression
ras ch = let res = parse expressionP ch in
	if (complet res) 
	then resultat res 
	else error "Erreur d’analyse syntaxique"
--Question 13	
data ValeurA = VLitteralA Litteral
	     | VFonctionA (ValeurA -> ValeurA) 
--Le probleme est que on ne peut pas afficher la valeur car sa definition est "récursive"
--Question 14

instance Show ValeurA where
    show (VFonctionA _) 		= "λ"
                       -- ^ ou "VFonctionA _", ou "<fun>" ou toute
                       --   autre représentation des fonctions
                       -- ici précédemment je faisais show VLitteralA a = show a mais cela affichait aussi le type, je l'ai donc remplacé par show VLitteralA (Type a)
    show (VLitteralA  (Bool a)) 	= show a
    show (VLitteralA  (Entier a)) 	= show a
    
type Environnement a = [(Nom, a)]
   
