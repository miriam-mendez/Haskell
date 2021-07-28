{-# LANGUAGE RecordWildCards #-}    -- per utilitzar fields

import Data.Char (isUpper)
import Data.List (nub, isInfixOf)
import Data.List.Split (splitOn)    -- cal instal·lar: cabal install split
import Data.String.Utils (strip)    -- cal instal·lar: cabal install MissingH
import Data.Maybe (mapMaybe, fromMaybe)

type Programa = [ Regla ]
data Regla = Regla { _cap::Atom, _cos::[ Atom ] } 
data Atom = Atom { _nomPredicat::String, _termes::[ Term ] } deriving (Eq, Show)
data Term = Var String | Sym String deriving (Eq, Show) 

type BaseConeixement = [ Atom ] 
type Sustitucio = [ (Term, Term) ]     -- [(variable, constant), (variable, constant), ...]

-------------------------------------------------------------------------
----------------                  MAIN                  -----------------
-------------------------------------------------------------------------
main :: IO ()
main = do
    contingut <- getContents
    putStrLn "BENVINGUT A PALLÚS! Aquí té les seves RESPOSTES:"
    let xs = splitOn "end." contingut
        pr = parsePrograma $ map (init) $ lines (head xs)
        q = parsePrograma $ map (init) $ tail $ lines (xs !! 1)
        solucio = respostes q pr
    mapM_ imprimir $ zip (tail $ lines (xs !! 1)) solucio
    
-------------------------------------------------------------------------
----------------                 OUTPUT                 -----------------
-------------------------------------------------------------------------
imprimir :: (String , [Sustitucio]) -> IO ()
imprimir all@(pregunta, solucio) = do
  putStrLn $ "+ " ++ pregunta
  putStrLn $ case solucio of  [[]]  -> "  true"
                              []    -> "  false"
                              solucio -> "  " ++ show(solucio)

-------------------------------------------------------------------------
----------------                 PARSER                 -----------------
-------------------------------------------------------------------------
parsePrograma :: [String] -> Programa
parsePrograma  pr = map parseRegla pr

parseRegla :: String -> Regla
parseRegla regla 
  | isInfixOf "=>" regla = Regla {_cap = parseAtom $ last x, _cos = map parseAtom (splitOn "&" $ head x)}
  | otherwise = Regla {_cap = parseAtom regla, _cos = []}
  where x = splitOn "=>" regla

parseAtom :: String -> Atom
parseAtom atom = Atom {_nomPredicat = head x, _termes = map parseTerm (tail x)}
  where x = words atom

parseTerm :: String -> Term
parseTerm x  
  | isUpper $ head x = Var x
  | otherwise = Sym x

-------------------------------------------------------------------------
----------------       GENERADOR DE RESPOSETES          -----------------
-------------------------------------------------------------------------
respostes :: Programa -> Programa -> [[ Sustitucio]]
respostes queries regles = do
  query <- queries
  let kb = consequencia (regles ++ [query]) []
      termesQ = filter (\x -> _nomPredicat x == "query") kb
      resposta = map (\x -> zip (_termes $ _cap query) (_termes x)) termesQ
  return resposta

-------------------------------------------------------------------------
-----------------       GNERADOR D'ATOM GROUNDS         -----------------
-------------------------------------------------------------------------
consequencia :: Programa -> BaseConeixement -> BaseConeixement
consequencia rules kb =
  let kb' = nub . (++) kb . run $ rules 
          where run = foldr ((++) . avaluaRegla kb) []
  in if kb == kb' then kb else  consequencia rules kb'

-------------------------------------------------------------------------
----------------------        AVALUAR REGLES          -------------------
-------------------------------------------------------------------------
avaluaRegla :: BaseConeixement -> Regla -> BaseConeixement
avaluaRegla kb (Regla cap cos) = map (sustitueix cap) $ run cos
      where run = foldr (avaluaAtom kb) [sustitucioBuida]

-------------------------------------------------------------------------
----------------------          UNIFICACIO         ----------------------
-------------------------------------------------------------------------
avaluaAtom :: BaseConeixement -> Atom -> [ Sustitucio ] -> [ Sustitucio ]
avaluaAtom kb atom llista = do
  sustitucio <- llista
  let sustituit = sustitueix atom sustitucio
  result <- mapMaybe (unifica sustituit) kb
  return $ (++) sustitucio result

unifica :: Atom -> Atom -> Maybe Sustitucio
unifica  (Atom pred termes) (Atom predGround termesGround) 
  | pred == predGround = unifica' $ zip termes termesGround
  | otherwise = Nothing
  where 
  unifica' :: [(Term,Term)] -> Maybe Sustitucio
  unifica' xs = foldl avalua (Just sustitucioBuida) xs
    where avalua acc (sym@Sym{}, sym'@Sym{}) 
            | sym /= sym' = Nothing 
            | otherwise = acc
          avalua acc x@(var@Var{}, sym@Sym{}) =
            case acc >>= lookup var  of Just resultat | sym /= resultat   -> Nothing
                                        _  ->  acc `mappend` Just [x]

sustitueix :: Atom -> Sustitucio -> Atom 
sustitueix atom sustitut = atom {_termes = map f (_termes atom)}
  where 
    f sym@Sym{}  = sym
    f var@Var{} = fromMaybe var (var `lookup` sustitut)

sustitucioBuida :: Sustitucio
sustitucioBuida = []