-- C:\Users\Josue\Documents\GDrive\TEC\4 Semestre 2020\Lenguajes de Programacion\Proyectos\TP1-Haskell\Solution
import Data.Map (Map, member, (!), fromList)

-- structure and types
type Line = [Token]
data Token = Word String | Blank | HypWord String
             deriving (Eq,Show)
-- utils
get (Word w) = w
get Blank = " "
get (HypWord w) = w ++ "-"

--	(a)	Defina una función que convierta una tira de entrada en una Line. 
--  Puede asumir que la entrada no contiene palabras separadas.
--  string2line text 
--  ⇒ [Word "Aquel",Word "que",Word "controla", ...]
string2line :: String -> Line
string2line text = map (\wrd-> Word wrd) $ words text

--  (b)	Defina una función que convierta de vuelta un Line en una tira. 
--  Se supone que hay un espacio en blanco luego de cada palabra menos la última
--  line2string [Word "Aquel",Word "que"] 
--  ⇒ "Aquel que"
line2string :: Line -> String
line2string line = unwords $ map (\tkn-> get tkn) line

--  (c)	Defina una función que calcule la longitud de un token:
-- tokenLength :: Token -> Int
-- tokenLength (Word "Aquel") ⇒ 4
-- tokenLength (HypWord "contro") ⇒ 7   -- Tomar en cuenta el "-"
-- tokenLength (Blank) ⇒ 1
tokenLength :: Token -> Int
tokenLength (Word w) = length w
tokenLength (HypWord w) = length w + 1
tokenLength (Blank) = 1

--  (d)	Defina una función que calcule la longitud de un Line:
-- lineLength [Word "Aquel",Word "que",HypWord "contro",Word "la"]⇒ 20
-- lineLength [Word "Aquel",Word "que",HypWord "con"] ⇒ 14
lineLength :: Line -> Int
lineLength line = sum (map (\tkn-> tokenLength tkn) line) + ( if (length line) <= 1 then 0 else (length line)-1 )


--  (e)	Defina un función que parta una línea de modo que no sea más larga que
--  una longitud dada. La función retorna un par de líneas: la primera es la 
--  línea con la longitud limitada, y la segunda es su continuación.
calcItemsAmount :: Int -> Line -> Int 
calcItemsAmount 0 _ = 0
calcItemsAmount _ [] = 0
calcItemsAmount len (l:line) = if ((tokenLength l) <= len) then 1 + calcItemsAmount (len - tokenLength l) line else 0

-- breakLine :: Int -> Line -> (Line,Line)
-- breakLine 1 [Word "Aquel",Word "que",Word "controla"]
--      ⇒ ([],[Word "Aquel",Word "que",Word "controla"])
-- breakLine 4 [Word "Aquel",Word "que",Word "controla"]
--      ⇒ ([],[Word "Aquel",Word "que",Word "controla"])
-- breakLine 5 [Word "Aquel",Word "que",Word "controla"]
--      ⇒ ([Word "Aquel"],[Word "que",Word "controla"]
-- breakLine 9 [Word "Aquel",Word "que",Word "controla"]
--      ⇒ ([Word "Aquel",Word "que"],[Word "controla"])
-- breakLine 100 [Word "Aquel",Word "que",Word "controla"]
--      ⇒ ([Word "Aquel",Word "que",Word "controla"],[])
-- breakLine 3 [] ⇒ ([],[])
breakLine :: Int -> Line -> (Line,Line)
breakLine _ [] = ([], [])
breakLine len line = ((take ( if wrdsAmount < (length line) then wrdsAmount else length line ) line), (drop wrdsAmount line) ) where wrdsAmount = calcItemsAmount len line

--  (f)	Defina una función auxiliar mergers que tome una lista de tiras y 
--  genere todas las formas de concatenarlas en orden dejando sin 
--  concatenar dos tiras acumuladas:
-- mergers ["co","nt","ro","la"]
-- 		⇒ [("co","ntrola"),("cont","rola"),("contro","la")]
-- mergers ["co","nt"] 
--		⇒ [("co","nt")]
-- mergers ["co"] 
--      ⇒ []
mergers :: [String] -> [(String, String)]
mergers [x] = []
mergers (str:strList) = [(str, concat strList)] ++ mergers (concat [[str++(head strList)], tail strList])

--  (g)	Definir una función de separación de palabras que separe un token Word 
--  de todas las formas posibles que se pueden derivar del Data.Map de arriba.
--  Aproveche la función mergers solicitada antes.
-- hyphenate enHyp (Word "controla")
--     ⇒ [(HypWord "con",Word "trola"),(HypWord "contro",Word "la")]
-- hyphenate enHyp (Word "firefox")
--     ⇒ [] -- no está en el map

type HypMap = Data.Map.Map String [String]
hyphenate :: HypMap -> Token -> [(Token,Token)]
hyphenate wrdMap tkn = if ( member wrdWithoutPoints wrdMap ) then ( map (\(x, y)->( (HypWord x), (Word (y ++ points)))) (mergers wrdParts) ) else [] 
 where
  wrdParts = wrdMap ! wrdWithoutPoints
  points = dropWhile (\ch->ch /= '.') (get tkn)
  wrdWithoutPoints = takeWhile (\ch->ch /= '.') (get tkn)

--  (h)	Definir ahora una función que encuentre las diferentes formas en que se puede separar 
--  una línea usando las diferentes formas en que la última palabra de la línea se pueda separar. 
--  Adicionalmente, la línea separada al inicio no puede sobrepasar una longitud dada.

-- lineBreaks :: HypMap -> Int -> Line -> [(Line,Line)]
-- lineBreaks enHyp 17 [Word "Aquel",Word "que", Word "controla"]
--     ⇒ [([Word "Aquel",Word "que"], [Word "controla"]),
--        ([Word "Aquel",Word "que",HypWord "con"], [Word "trola"]),
--        ([Word "Aquel",Word "que",HypWord "contro"], [Word "la"])]
-- lineBreaks enHyp 12 [Word "Aquel"]
--     ⇒ [([Word "Aquel"],[])]
enHyp :: HypMap
enHyp = Data.Map.fromList [("controla",["con","tro","la"]), ("futuro",["fu","tu","ro"]),("presente",["pre","sen","te"]), ("futuro", ["fu", "tu", "ro"])]

lineBreaks :: HypMap -> Int -> Line -> [(Line,Line)]
lineBreaks wrdMap len line = if (lineLength line) < len then [breakLine len line] else []

-- (i)	Definir una función insertBlanks que distribuya un número dado de espacios en blanco 
-- entre las palabras. La distribución se hará colocando un Blank en posiciones consecutivas
-- de izquierda a derecha. Esto se repite de nuevo de izquierda a derecha hasta que se haya
--  colocado la cantidad de espacios en blanco pedidos.
-- insertBlanks 2 [] 
--	   ⇒ []
-- insertBlanks 2 [Word "hola"] 
--     ⇒ [Word "hola"]
-- insertBlanks 2 [Word "hola", Word "mundo", Word "cruel"]
--     ⇒ [Word "hola", Blank, Word "mundo", Blank, Word "cruel"]
-- insertBlanks 3 [Word "hola", Word "mundo", Word "cruel"]
--     ⇒ [Word "hola", Blank,Blank, Word "mundo", Blank, Word "cruel"]
-- insertBlanks 5 [Word "hola", Word "mundo", Word "cruel"]
--     ⇒ [Word "hola", Blank,Blank,Blank, Word "mundo", Blank,Blank,Word "cruel"]
-- insertBlanks 5 [Word "hola", Word "mundo", Word "cruel", Word "adios"]
--     ⇒ [Word "hola",Blank,Blank, Word "mundo", Blank,Blank, Word "cruel", Blank, Word "adios"]
insertBlanks :: Int -> [Token] -> [Token]
insertBlanks _ [] = []
insertBlanks _ [wrd] = [wrd]