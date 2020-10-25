{-- 
	Text Handling Functions
	@author: JosueRV99
--}
import Data.Map (Map, member, (!), fromList)

enHyp :: HypMap
enHyp = Data.Map.fromList [("controla",["con","tro","la"]), ("futuro",["fu","tu","ro"]),("presente",["pre","sen","te"]), ("futuro", ["fu", "tu", "ro"])]

type Line = [Token]
data Token = Word String | Blank | HypWord String
             deriving (Eq,Show)
get (Word w) = w
get Blank = " "
get (HypWord w) = w ++ "-"

string2line :: String -> Line
string2line text = map (\wrd-> Word wrd) $ words text

line2string :: Line -> String
line2string line = unwords $ map (\tkn-> get tkn) line

tokenLength :: Token -> Int
tokenLength (Word w) = length w
tokenLength (HypWord w) = length w + 1
tokenLength (Blank) = 1

lineLength :: Line -> Int
lineLength line = sum (map (\tkn-> tokenLength tkn) line) + ( if (length line) <= 1 then 0 else (length line)-1 )

breakLine :: Int -> Line -> (Line,Line)
breakLine _ [] = ([], [])
breakLine len line = (take breakIndex line, drop breakIndex line) 
 where
  calcBreakIdx len (l:line) = if ((tokenLength l)<len && line/=[]) then 1 + (calcBreakIdx (len - (tokenLength l)) line) else 0
  breakIndex = calcBreakIdx len line

mergers :: [String] -> [(String, String)]
mergers [x] = []
mergers (str:strList) = [(str, concat strList)] ++ mergers (concat [[str++(head strList)], tail strList])

type HypMap = Data.Map.Map String [String]
hyphenate :: HypMap -> Token -> [(Token,Token)]
hyphenate wrdMap tkn = if ( member wrdWithoutPoints wrdMap ) then ( map (\(x, y)->( (HypWord x), (Word (y ++ points)))) (mergers wrdParts) ) else [] 
 where
  wrdParts = wrdMap ! wrdWithoutPoints
  points = dropWhile (\ch->ch /= '.') (get tkn)
  wrdWithoutPoints = takeWhile (\ch->ch /= '.') (get tkn)

lineBreaks :: HypMap -> Int -> Line -> [(Line, Line)]
lineBreaks wrdMap len line = if (lineLength line) < len then [breakLine len line] else map (\ele->(breakLine len ele)) resultList
 where
  finalWrdSet = hyphenate wrdMap (last line)
  resultList = line : (map (\w->(init line)++([fst w,snd w])) finalWrdSet)

insertBlanks :: Int -> [Token] -> [Token]
insertBlanks _ [] = []
insertBlanks _ [wrd] = [wrd]
insertBlanks spaces (w:wrds) = [w] ++ (take requiredSpaces (repeat Blank)) ++ insertBlanks (spaces - requiredSpaces) wrds
 where requiredSpaces = ceiling $ (fromIntegral spaces) / (fromIntegral (length wrds))

 {--
	(j)	Finalmente, definir una función separarYalinear que reciba una tira
	de caracteres y un tamaño de línea, y que devuelva una lista de tiras
	de caracteres que no sean más largas que el tamaño especificado La 
	función también recibe dos banderas. La primera indica si debe usar 
	separación de palabras o no: el valor de NOSEPARAR indica que las 
	líneas se crean sin separar palabras;  el valor de SEPARAR indica que
	para cada línea se debe intentar separar la palabra que no cabe, 
	usando la separación que incluya la mayor parte de la palabra en la 
	línea anterior. La segunda bandera indica si se deben ajustar las 
	líneas, esto es, se deben insertar espacios en blanco para que las 
	líneas sean exactamente del tamaño dado; con excepción de la última 
	línea, a la cual nunca se le inserta un espacio en blanco. Para esta 
	segunda bandera, el valor de NOAJUSTAR indica que no deben insertarse
	espacios en blanco para alinear el texto; si el valor es AJUSTAR 
	entonces se insertan espacios en blanco en las líneas.
 --}

-- TODO: go ahead!