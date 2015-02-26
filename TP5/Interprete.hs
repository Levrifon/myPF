import Parser
--Debue Rémy
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

--Question 4
applique :: [Expression] -> Expression
applique [] = undefined
applique [e] = e
applique es = App (applique (init es)) (last es)

--Question 5
--exprP :: Parser Expression
--exprP = varP

exprsP :: Parser Expression
exprsP = (unOuPlus exprP) >>= \r -> return (applique r)

--Question 6
lambdaP :: Parser Expression
lambdaP = (car '\\' >>= \_ ->
          espacesP      >>= \_ 	->
          nomP 		>>= \r 	-> 
          car '-' 	>>= \_ 	->
          car '>' 	>>= \_ 	->
          espacesP 	>>= \_	->
          exprsP 	>>= \s 	-> return (Lam r s))
--Question 7
exprP = varP ||| lambdaP

--Question 8

