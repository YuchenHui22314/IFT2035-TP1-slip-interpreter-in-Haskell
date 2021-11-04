-- TP-1  --- Implantation d'une sorte de Lisp          -*- coding: utf-8 -*-
{-# OPTIONS_GHC -Wall #-}
{--auteur : Stefan Monnier(professeur aggrege a l<universit/ de montr/al)
 - date : 3/11/2021
--------------------------------------------------------------------------
--------------------------------------------------------------------------
-- contributeur : Yuchen Hui,                                        ----- 
--                Yuyang Xiong                                    -} -----
--------------------------------------------------------------------------
--------------------------------------------------------------------------
-- Ce fichier défini les fonctionalités suivantes:
-- - Analyseur lexical
-- - Analyseur syntaxique
-- - Évaluateur
-- - Pretty printer

---------------------------------------------------------------------------
-- Importations de librairies et définitions de fonctions auxiliaires    --
---------------------------------------------------------------------------

import Text.ParserCombinators.Parsec -- Libraire d'analyse syntaxique (et lexicale).
import Data.Char        -- Conversion de Chars de/vers Int et autres
-- import Numeric       -- Pour la fonction showInt
import System.IO        -- Pour stdout, hPutStr
-- import Data.Maybe    -- Pour isJust and fromJust

---------------------------------------------------------------------------
-- La représentation interne des expressions de notre language           --
---------------------------------------------------------------------------
data Sexp = Snil                        -- La liste vide
          | Scons Sexp Sexp             -- Une paire
          | Ssym String                 -- Un symbole
          | Snum Int                    -- Un entier
          -- Génère automatiquement un pretty-printer et une fonction de
          -- comparaison structurelle.
          deriving (Show, Eq)

-- Exemples:
-- (+ 2 3) == (+ . (2 . (3 . ())))
--         ==> Scons (Ssym "+")
--                   (Scons (Snum 2)
--                          (Scons (Snum 3) Snil))
--
-- (/ (* (- 68 32) 5) 9)
--     ==>
-- Scons (Ssym "/")
--       (Scons (Scons (Ssym "*")
--                     (Scons (Scons (Ssym "-")
--                                   (Scons (Snum 68)
--                                          (Scons (Snum 32) Snil)))
--                            (Scons (Snum 5) Snil)))
--              (Scons (Snum 9) Snil))

---------------------------------------------------------------------------
-- Analyseur lexical                                                     --
---------------------------------------------------------------------------

pChar :: Char -> Parser ()
pChar c = do { _ <- char c; return () }

-- Les commentaires commencent par un point-virgule et se terminent
-- à la fin de la ligne.
pComment :: Parser ()
pComment = do { pChar ';'; _ <- many (satisfy (\c -> not (c == '\n')));
                pChar '\n'; return ()
              }
-- N'importe quelle combinaison d'espaces et de commentaires est considérée
-- comme du blanc.
pSpaces :: Parser ()
pSpaces = do { _ <- many (do { _ <- space ; return () } <|> pComment); return () }

-- Un nombre entier est composé de chiffres.
integer     :: Parser Int
integer = do c <- digit
             integer' (digitToInt c)
          <|> do _ <- satisfy (\c -> (c == '-'))
                 n <- integer
                 return (- n)
    where integer' :: Int -> Parser Int
          integer' n = do c <- digit
                          integer' (10 * n + (digitToInt c))
                       <|> return n

-- Les symboles sont constitués de caractères alphanumériques et de signes
-- de ponctuations.
pSymchar :: Parser Char
pSymchar    = alphaNum <|> satisfy (\c -> c `elem` "!@$%^&*_+-=:|/?<>")
pSymbol :: Parser Sexp
pSymbol= do { s <- many1 (pSymchar);
              return (case parse integer "" s of
                        Right n -> Snum n
                        _ -> Ssym s)
            }

---------------------------------------------------------------------------
-- Analyseur syntaxique                                                  --
---------------------------------------------------------------------------

-- La notation "'E" est équivalente à "(quote E)"
pQuote :: Parser Sexp
pQuote = do { pChar '\''; pSpaces; e <- pSexp;
              return (Scons (Ssym "quote") (Scons e Snil)) }

-- Une liste est de la forme:  ( {e} [. e] )
pList :: Parser Sexp
pList  = do { pChar '('; pSpaces; pTail }
pTail :: Parser Sexp
pTail  = do { pChar ')'; return Snil }
     <|> do { pChar '.'; pSpaces; e <- pSexp; pSpaces;
              pChar ')' <|> error ("Missing ')' after: " ++ show e);
              return e }
     <|> do { e <- pSexp; pSpaces; es <- pTail; return (Scons e es) }

-- Accepte n'importe quel caractère: utilisé en cas d'erreur.
pAny :: Parser (Maybe Char)
pAny = do { c <- anyChar ; return (Just c) } <|> return Nothing

-- Une Sexp peut-être une liste, un symbol ou un entier.
pSexpTop :: Parser Sexp
pSexpTop = do { pSpaces;
                pList <|> pQuote <|> pSymbol
                <|> do { x <- pAny;
                         case x of
                           Nothing -> pzero
                           Just c -> error ("Unexpected char '" ++ [c] ++ "'")
                       }
              }

-- On distingue l'analyse syntaxique d'une Sexp principale de celle d'une
-- sous-Sexp: si l'analyse d'une sous-Sexp échoue à EOF, c'est une erreur de
-- syntaxe alors que si l'analyse de la Sexp principale échoue cela peut être
-- tout à fait normal.
pSexp :: Parser Sexp
pSexp = pSexpTop <|> error "Unexpected end of stream"

-- Une séquence de Sexps.
pSexps :: Parser [Sexp]
pSexps = do pSpaces
            many (do e <- pSexpTop
                     pSpaces
                     return e)

-- Déclare que notre analyseur syntaxique peut-être utilisé pour la fonction
-- générique "read".
-- Yuchen's best friend is Liu Tianchang and Zhang Buze,
instance Read Sexp where
    readsPrec _p s = case parse pSexp "" s of
                      Left _ -> []
                      Right e -> [(e,"")]

---------------------------------------------------------------------------
-- Sexp Pretty Printer                                                   --
---------------------------------------------------------------------------

showSexp' :: Sexp -> ShowS
showSexp' Snil = showString "()"
showSexp' (Snum n) = showsPrec 0 n
showSexp' (Ssym s) = showString s
showSexp' (Scons e1 e2) =
    let showTail Snil = showChar ')'
        showTail (Scons e1' e2') =
            showChar ' ' . showSexp' e1' . showTail e2'
        showTail e = showString " . " . showSexp' e . showChar ')'
    in showChar '(' . showSexp' e1 . showTail e2

-- On peut utiliser notre pretty-printer pour la fonction générique "show"
-- (utilisée par la boucle interactive de Hugs).  Mais avant de faire cela,
-- il faut enlever le "deriving Show" dans la déclaration de Sexp.
{-
instance Show Sexp where
    showsPrec p = showSexp'
-}

-- Pour lire et imprimer des Sexp plus facilement dans la boucle interactive
-- de Hugs:
readSexp :: String -> Sexp
readSexp = read
showSexp :: Sexp -> String
showSexp e = showSexp' e ""

---------------------------------------------------------------------------
-- Représentation intermédiaire L(ambda)exp(ression)                     --
---------------------------------------------------------------------------

type Var = String
type Tag = String
type Pat = Maybe (Tag, [Var])
data BindingType = Lexical | Dynamic
                   deriving (Show, Eq)
    
data Lexp = Lnum Int            -- Constante entière.
          | Lvar Var            -- Référence à une variable.
          | Lfn Var Lexp        -- Fonction anonyme prenant un argument.
          | Lpipe Lexp Lexp     -- Appel de fonction, avec un argument.
          | Lcons Tag [Lexp]    -- Constructeur d'une structure de données.
          | Lcase Lexp [(Pat, Lexp)] -- Expression conditionelle.
          | Llet BindingType Var Lexp Lexp -- Déclaration de variable locale
          deriving (Show, Eq)

-- Première passe simple qui analyse un Sexp et construit une Lexp équivalente.


-----------------------------------------------------------------------
--                              fonction s2l.
--------------------------------------------------------------------
s2l:: Sexp -> Lexp
s2l (Snum n) = Lnum n
s2l (Ssym s) = Lvar s
s2l (Scons (Ssym "cons") (Scons (Ssym a) x)) =
     s2l_foldl consConcate (Lcons a []) x
s2l (Scons (Ssym "lambda") x) = case x of
        (Scons _ Snil) -> error "déclaration de fonction anonyme invalide..." 
        --erreur : pas de variable/corps dans la déclaration.(ci-dessus)
        (Scons (Scons a Snil) (Scons c Snil)) -> case a of
            Ssym s -> Lfn s (s2l c) 
            _ -> error "déclaration de fonction anonyme invalide..."
            --erreur : variable n'est pas un Ssym.
        --s'il n'y a pas de sucre syntaxique du genre 'lambda'.(ci-dessus)
        (Scons (Scons (Ssym a) b) (Scons e Snil) ) -> let lexp = (s2l e) in
            s2l_foldr imbriquerLambda lexp (Scons (Ssym a) b)
        --s'il y a des sucre syntaxique du genre 'lambda'.
        --càd, lambda (x1 x2 x3...)(ci-dessus)
        _ -> error "Syntax invalide"

s2l (Scons (Ssym "slet") (Scons a (Scons e Snil))) = case a of
        autre -> let lexp2 = (s2l e) in
            s2l_foldr_let imbriquerSlet lexp2 autre    
        
s2l (Scons (Ssym "dlet") (Scons a (Scons e Snil))) = case a of
        autre -> let lexp2 = (s2l e) in
            s2l_foldr_let imbriquerDlet lexp2 autre

s2l (Scons (Ssym "case") (Scons e listbr)) = 
    let struct = (s2l e) in
        s2l_foldl_case caseConcate (Lcase struct []) listbr   
    
s2l (Scons (Ssym "if") (Scons e1 (Scons e2 (Scons e3 Snil)))) =
    (s2l (Scons (Ssym "case") 
      (Scons e1 
             (Scons (Scons (Scons (Ssym "true") Snil) 
                    (Scons e2 Snil)) 
                    (Scons (Scons (Scons (Ssym "false") Snil) 
                    (Scons e3 Snil)) Snil)))))

s2l (Scons a b) = 
    let la = (s2l a) in
        s2l_foldl pipeline la b


s2l se = error ("Malformed Sexp: " ++ (showSexp se))
----------------------------------------------------------------------
----------------------------------------------------------------------
        

--fonctions auxiliaires générales----------------------------------------------
------------------------------------------------------------------------------
s2l_foldl :: (Lexp -> Lexp -> Lexp) -> Lexp -> Sexp -> Lexp
s2l_foldl f lexp sexp = 
              case sexp of 
                (Scons (Scons a c) b) -> 
                    (s2l_foldl f (f lexp (s2l (Scons a c))) b)
                (Snil) -> lexp
                (Scons vOrn m)-> (s2l_foldl f (f lexp (s2l vOrn)) m)
                _ -> error "Utilisation incorrecte de la fonction s2l_foldl." 
                
s2l_foldl_case :: (Lexp -> (Pat,Lexp) -> Lexp) -> Lexp -> Sexp -> Lexp
s2l_foldl_case f caseLexp brList =
    case brList of
        Snil -> caseLexp
        (Scons (Scons bCorps (Scons e1 Snil)) b2) -> 
            s2l_foldl_case f (f caseLexp ((caseBrancheAnalyse bCorps),(s2l e1))) b2
        _ -> error "Utilisation incorrecte de la fonction s2l_foldl_case." 

s2l_foldr :: (Var -> Lexp -> Lexp) -> Lexp -> Sexp -> Lexp
s2l_foldr f lexp sexp = 
    case sexp of 
        Snil -> lexp
        (Scons (Ssym s) b) -> f s (s2l_foldr f lexp b)
        _ -> error "Syntax invalide"

s2l_foldr_let :: ((Var,Lexp) -> Lexp -> Lexp) -> Lexp -> Sexp ->Lexp
s2l_foldr_let f lexp2 sexp =
    case sexp of
        Snil -> lexp2
        (Scons d c) -> f (letDeclaJduge d) (s2l_foldr_let f lexp2 c)
        _ -> error "Syntax invalide!!"
--------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------

--fonctions auxiliaires pour les let----------------
letDeclaJduge :: Sexp -> (Var, Lexp)
letDeclaJduge (Scons (Scons (Ssym s) t) (Scons e1 Snil))  = 
    (s,(s2l_foldr imbriquerLambda (s2l e1) t))  
letDeclaJduge (Scons (Ssym s) (Scons e1 Snil)) =
    (s, (s2l e1))
letDeclaJduge _ = error "Syntax invalide"

imbriquerSlet :: (Var, Lexp) -> Lexp -> Lexp
imbriquerSlet (var,lexp1) lexp2 = Llet Lexical var lexp1 lexp2

imbriquerDlet :: (Var, Lexp) -> Lexp -> Lexp
imbriquerDlet (var,lexp1) lexp2 = Llet Dynamic var lexp1 lexp2
---------------------------------------------------------------
-----------------------------------------------------------


-------------fonctions auxiliaires pour les lambda expressions----------------
imbriquerLambda :: Var -> Lexp -> Lexp
imbriquerLambda var lexp = Lfn var lexp
----------------------------------------------------------------------



----------------fonctions auxiliaires pour les constructeurs des listes-------
consConcate :: Lexp -> Lexp -> Lexp
consConcate (Lcons tag list) lexp2 = Lcons tag (list ++ [lexp2])
consConcate _ _ = error "utilisation incorrecte."
---------------------------------------------------------------------

------------------------------fonction auxiliaire pour case--------------
caseConcate :: Lexp -> (Pat,Lexp) -> Lexp
caseConcate (Lcase lexp list) newPair = (Lcase lexp (list ++ [newPair]))
caseConcate _ _ = error "utilisation incorrecte de la fonction caseConcate" 

caseBrancheAnalyse :: Sexp -> Pat
caseBrancheAnalyse (Ssym "_") = Nothing
caseBrancheAnalyse pattern = 
    let struct = (s2l (Scons (Ssym "cons") pattern)) in
        case struct of
            (Lcons tag listLexp) -> Just (tag, (map deLexp listLexp)) 
            _ -> error "utilisation incorrecte de la fonction caseBranche..."

deLexp :: Lexp -> Var
deLexp (Lvar a) = a
deLexp _ = error "utilisation incorrecte de la fonction deLexp"
----------------------------------------------------------------------
--------------------------------------------------------------------


---------------------fonctions auxiliaires pour les appels de foncitons--------
pipeline ::  Lexp-> Lexp -> Lexp 
pipeline lexp1 lexp2 = Lpipe lexp1 lexp2 
----------------------------------------------------------------------------
-- ¡¡ COMPLETER !!


---------------------------------------------------------------------------
-- Représentation du contexte d'exécution                                --
---------------------------------------------------------------------------

type Arity = Int

-- Type des valeurs manipulée à l'exécution.
data Value = Vnum Int
           | Vcons Tag [Value]
           | Vfn (Env -> Value -> Value)

instance Show Value where
    showsPrec p (Vnum n) = showsPrec p n
    showsPrec p (Vcons tag vs) =
        let showTail [] = showChar ']'
            showTail (v : vs') =
                showChar ' ' . showsPrec p v . showTail vs'
        in showChar '[' . showString tag . showTail vs
    showsPrec _ (Vfn _)
        = showString ("<function>")

type Env = [(Var, Value)]

-- L'environnement initial qui contient les fonctions prédéfinies.
env0 :: Env
env0 = let false = Vcons "false" []
           true = Vcons "true" []
           mkbop (name, op) =
               (name, Vfn (\ _ (Vnum x)
                           -> Vfn (\ _ (Vnum y)
                                   -> Vnum (x `op` y))))
           mkcmp (name, op) =
               (name, Vfn (\ _ (Vnum x)
                           -> Vfn (\ _ (Vnum y)
                                   -> if x `op` y then true else false)))
       in [("false", false),
           ("true", true)]
          ++ map mkbop
              [("+", (+)),
               ("*", (*)),
               ("/", div),
               ("-", (-))]
          ++ map mkcmp
              [("<=", (<=)),
               ("<", (<)),
               (">=", (>=)),
               (">", (>)),
               ("=", (==))]

---------------------------------------------------------------------------
-- Évaluateur                                                            --
---------------------------------------------------------------------------

--fonction auxiliaire qui cherche dans l'environnement
elookup :: Var -> Env -> Maybe Value
elookup x ((x1,v1):env) =
    if x == x1 then Just v1 else elookup x env
elookup _ [] = Nothing 


--fonctions auxiliaireis pour l'évaluation des Lcase-------------------
---------------------------------------------------------------------

searchBranch :: Var -> Int -> [(Maybe (Var, [Var]), Lexp)]
                           -> (Maybe (Var, [Var]), Lexp)
searchBranch _ _ [] = error "no suitable branch"
searchBranch consTag consArgNum (branch:restBranch)
    | compareBranch consTag consArgNum branch = branch
    | otherwise = searchBranch consTag consArgNum restBranch

compareBranch :: Var -> Int -> (Maybe (Var, [Var]), Lexp) -> Bool
compareBranch consTag consArgNum (pat, _) =
    case pat of
        Nothing -> True 
        Just (branchTag, branchArgList) -> consTag == branchTag 
                                     && consArgNum == length branchArgList

extractArgList :: (Maybe (Var, [Var]), a) -> [Var]
extractArgList (pat, _) = 
    case pat of
        Nothing -> []
        Just (_, branchArgList) -> branchArgList

extractExp :: (a, Lexp) -> Lexp
extractExp (_, branchExp) = branchExp

evalBranch :: [(Var, Value)] -> [(Var, Value)] -> [Value] -> [Var] 
                                                  -> Lexp -> Value
evalBranch sEnv dEnv _ [] branchExp = eval sEnv dEnv branchExp
evalBranch sEnv dEnv (consArg:restConsArg) (branchArg:restBranchArg) branchExp = 
    let sEnv' = (branchArg, consArg):sEnv
        dEnv' = (branchArg, consArg):dEnv
    in evalBranch sEnv' dEnv' restConsArg restBranchArg branchExp


------    ---------------------------------------------------------------------
-----------------------------------------------------------------------------------

----------------------------------------------------------------
                  --fonction eval
------------------------------------------------------------------------

eval :: Env -> Env -> Lexp -> Value
eval _senv _denv (Lnum n) = Vnum n
--cherche d'abord dans l'environnement statique puis l'environnement dynamique.
eval _senv _denv (Lvar v) = case (elookup v _senv) of
    Nothing -> case (elookup v _denv) of
        Nothing -> error ("la variable \""++ v ++ "\" n'existe pas")
        Just value -> value
    Just value -> value
--evaluation des appels de fonctions.
eval _senv _denv (Lpipe lexp1 lexp2) = 
    let vlexp1 = (eval _senv _denv lexp1) 
        vlexp2 = (eval _senv _denv lexp2)
    in case vlexp2 of
        Vfn f -> f _denv vlexp1
        _ -> error ("c'est pas une fonction!!")
--evaluation des let
eval _senv _denv (Llet bindingType var lexp1 lexp2)=
   case bindingType of 
       Dynamic -> eval _senv ((var,value1):_denv) lexp2
           where value1 = eval _senv _denv lexp1
       Lexical -> eval ((var,value1):_senv) _denv lexp2
           where value1 = eval _senv _denv lexp1
--Évaluation des fonctions
eval _senv _denv (Lfn var lexp) = 
    Vfn (\ denv valeurVar -> eval ((var,valeurVar):_senv) denv lexp)
--Évaluation des structures de données
eval _senv _denv (Lcons tag list) = Vcons tag (map (eval _senv _denv) list)
--Évaluation des filtrages
eval sEnv dEnv (Lcase cons branchList) = 
    let Vcons consTag consArgList = eval sEnv dEnv cons
        numArg = length consArgList  
        -- (ci-dessus)nombre de paramètres dans la stucture passée en paramètre
        -- trouver une branche dont le pattern se conforme à celui de la structure
        branchSearched = searchBranch consTag numArg branchList
        branchArgList = extractArgList branchSearched
        branchExp = extractExp branchSearched
    in evalBranch sEnv dEnv consArgList branchArgList branchExp
-- ¡¡ COMPLETER !!
eval _ _ e = error ("Can't eval: " ++ show e)


------------------------------------------------------------------------------
                               --end----
-------------------------------------------------------------------------------

---------------------------------------------------------------------------
-- Toplevel                                                              --
---------------------------------------------------------------------------

--Stefan Monnier shishijieshangzuikuaidenanren
evalSexp :: Sexp -> Value
evalSexp = eval env0 [] . s2l

-- Lit un fichier contenant plusieurs Sexps, les évalues l'une après
-- l'autre, et renvoie la liste des valeurs obtenues.
run :: FilePath -> IO ()
run filename =
    do s <- readFile filename
       (hPutStr stdout . show)
           (let sexps s' = case parse pSexps filename s' of
                             Left _ -> [Ssym "#<parse-error>"]
                             Right es -> es
            in map evalSexp (sexps s))
