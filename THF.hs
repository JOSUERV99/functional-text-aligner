-- C:\Users\Josue\Documents\GDrive\TEC\4 Semestre 2020\Lenguajes de Programacion\Proyectos\TP1-Haskell\Solution
import Data.Map (Map, member, (!))

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
string2line :: String -> Line
string2line text = map (\wrd-> Word wrd) $ words text

--  (b)	Defina una función que convierta de vuelta un Line en una tira. 
--  Se supone que hay un espacio en blanco luego de cada palabra menos la última
line2string :: Line -> String
line2string line = unwords $ map (\tkn-> get tkn) line 

--  (c)	Defina una función que calcule la longitud de un token:
tokenLength :: Token -> Int
tokenLength (Word w) = length w
tokenLength (HypWord w) = length w + 1
tokenLength (Blank) = 1

--  (d)	Defina una función que calcule la longitud de un Line:
lineLength :: Line -> Int
lineLength line = sum (map (\tkn-> tokenLength tkn) line) + ( if (length line) <= 1 then 0 else (length line)-1 )
--lineLength [Word "Aquel",Word "que",HypWord "contro",Word "la"]⇒ 20
--lineLength [Word "Aquel",Word "que",HypWord "con"] ⇒ 14


--  (e)	Defina un función que parta una línea de modo que no sea más larga que
--  una longitud dada. La función retorna un par de líneas: la primera es la 
--  línea con la longitud limitada, y la segunda es su continuación.
calcItemsAmount :: Int -> Line -> Int 
calcItemsAmount 0 _ = 0
calcItemsAmount _ [] = 0
calcItemsAmount len (l:line) = if ((tokenLength l) <= len) then 1 + calcItemsAmount (len - tokenLength l) line else 0

breakLine :: Int -> Line -> (Line,Line)
breakLine _ [] = ([], [])
breakLine len line = 
 ( (take ( if ((calcItemsAmount len line) < length line) then (calcItemsAmount len line) else length line ) line), (drop (calcItemsAmount len line) line) )


--  (f)	Defina una función auxiliar mergers que tome una lista de tiras y 
--  genere todas las formas de concatenarlas en orden dejando sin 
--  concatenar dos tiras acumuladas:
mergers :: [String] -> [(String, String)]
mergers [x] = []
mergers (str:strList) = [(str, concat strList)] ++ mergers (concat [[str++((take 1 strList)!!0)], tail strList])


--  (g)	Definir una función de separación de palabras que separe un token Word 
--  de todas las formas posibles que se pueden derivar del Data.Map de arriba.
--  Aproveche la función mergers solicitada antes.
type HypMap = Data.Map.Map String [String]
hyphenate :: HypMap -> Token -> [(Token,Token)]
hyphenate wrdMap tkn = 
 if ( member (get tkn) wrdMap ) then ( map (\(x, y)->( (HypWord x), (Word y))) (mergers (wrdMap ! (get tkn))) ) else []

--  (h)	Definir ahora una función que encuentre las diferentes formas en que se puede separar 
--  una línea usando las diferentes formas en que la última palabra de la línea se pueda separar. 
--  Adicionalmente, la línea separada al inicio no puede sobrepasar una longitud dada.

-- lineBreaks :: HypMap -> Int -> Line -> [(Line,Line)]
-- lineBreaks enHyp 17 [Word "Aquel",Word "que",Word "controla"]
--     ⇒ [([Word "Aquel",Word "que"], [Word "controla"]),
--        ([Word "Aquel",Word "que",HypWord "con"], [Word "trola"]),
--        ([Word "Aquel",Word "que",HypWord "contro"], [Word "la"])]
-- lineBreaks enHyp 12 [Word "Aquel"] ⇒ [([Word "Aquel"],[])]
lineBreaks :: HypMap -> Int -> Line -> [(Line,Line)]


