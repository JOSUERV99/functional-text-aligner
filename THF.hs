{-- 
	Text Handling Functions
	@author: JosueRV99
--}
import Data.Map (Map, member, (!), fromList)

enHyp :: HypMap
enHyp = Data.Map.fromList [("controla",["con","tro","la"]), ("futuro",["fu","tu","ro"]),("presente",["pre","sen","te"]), ("futuro", ["fu", "tu", "ro"])]

data SPFlag = SEPARAR | NOSEPARAR -- Separacion de palabras
data ALFlag = AJUSTAR | NOAJUSTAR -- Ajuste de lineas

type Line = [Token]
data Token = Word String | Blank | HypWord String
             deriving (Eq,Show)

get (Word w) = w
get Blank = " "
get (HypWord w) = w ++ "-"

string2line :: String -> Line
string2line text = map (\wrd-> Word wrd) $ words text

line2string :: Line -> String
line2string line = unwords $ map (\tkn->if tkn/=Blank then get tkn else "") (reverse (dropWhile cleanFunc (reverse (dropWhile cleanFunc line))))
 where cleanFunc = (\x->x==Blank)

tokenLength :: Token -> Int
tokenLength (Word w) = length w
tokenLength (HypWord w) = length w + 1
tokenLength (Blank) = 1 

lineLength :: Line -> Int
lineLength line = sum (map (\tkn-> tokenLength tkn) line) + if (length line) <= 1 then 0 else (length line)-1

breakLine :: Int -> Line -> (Line,Line)
breakLine _ [] = ([], [])
breakLine len line = (take breakIndex line, drop breakIndex line) 
 where
  calcBreakIdx _ [] = 0
  calcBreakIdx len (l:line) = if ((tokenLength l)<=len) then 1 + (calcBreakIdx (len - (tokenLength l)-1) line) else 0
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
lineBreaks wrdMap len line = if (lineLength line) <= len then [breakLine len line] else map (\l->(breakLine len l)) lines
 where
  finalWrdSet = hyphenate wrdMap (last line)
  lines = line : (map (\(wrdStart, wrdEnd)->(init line)++([wrdStart,wrdEnd])) finalWrdSet)

insertBlanks :: Int -> [Token] -> [Token]
insertBlanks _ [] = []
insertBlanks _ [wrd] = [wrd]
insertBlanks spaces (w:wrds) = [w] ++ (take requiredSpaces (repeat Blank)) ++ insertBlanks (spaces - requiredSpaces) wrds
 where requiredSpaces = ceiling $ (fromIntegral spaces) / (fromIntegral (length wrds))

-- utils
divideTextByLines :: (Line, Line) -> Int -> [Line]
divideTextByLines (firstPart, secondPart) maxLength = 
 if (lineLength secondPart) <= maxLength then [firstPart, secondPart] else [firstPart] ++ divideTextByLines (breakLine maxLength secondPart) maxLength

divideTextByLinesWithSeparation :: [(Line, Line)] -> Int -> [Line]
divideTextByLinesWithSeparation (set:lineOptions) maxLength = 
 if (lineLength secondPart) <= maxLength then [firstPart, secondPart] else [firstPart] ++ divideTextByLinesWithSeparation (lineBreaks enHyp maxLength secondPart) maxLength
 where
  selectOneFunc x y = if lineLength (fst y) <= maxLength && lineLength (fst y) <= lineLength (fst x) then x else y
  selectedLine lineSet = foldr selectOneFunc (head lineSet) (tail lineSet)
  (firstPart, secondPart) = selectedLine [set]

separarYalinear :: Int -> SPFlag -> ALFlag -> String -> [String]
--------------------------------------------------------------------------------
separarYalinear maxLength NOSEPARAR NOAJUSTAR text = map line2string lineSet
 where
  lineSet = divideTextByLines (breakLine maxLength (string2line text)) maxLength
--------------------------------------------------------------------------------
separarYalinear maxLength NOSEPARAR AJUSTAR text = (map adjustFunc (tail lineSet)) ++ [line2string (last lineSet)]
 where
  adjustFunc line = line2string (insertBlanks (maxLength - (lineLength line)) line)
  lineSet = divideTextByLines (breakLine maxLength (string2line text)) maxLength
--------------------------------------------------------------------------------
separarYalinear maxLength SEPARAR NOAJUSTAR text = []
 where
  lineSet = divideTextByLines (breakLine maxLength (string2line text)) maxLength

--------------------------------------------------------------------------------
separarYalinear maxLength SEPARAR AJUSTAR text = []



