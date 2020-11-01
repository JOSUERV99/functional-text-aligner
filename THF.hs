{--
	Text Handling Functions for TextAligner 
	@author: JosueRV99
  @since: 1/11/2020
--}

module THF where
import Data.Map (Map, member, (!), fromList)

data SPFlag = SEPARAR | NOSEPARAR -- Separacion de palabras
data ALFlag = AJUSTAR | NOAJUSTAR -- Ajuste de lineas

type Line = [Token]
type HypMap = Data.Map.Map String [String]
data Token = Word String | Blank | HypWord String
             deriving (Eq,Show)

get :: Token -> String
get (Word w) = w
get Blank = " "
get (HypWord w) = w ++ "-"

string2line :: String -> Line
string2line text = map (\wrd-> Word wrd) $ words text

line2string :: Line -> String
line2string line = unwords $ map (\tkn->if tkn/=Blank then get tkn else "") (reverse (dropWhile (\x->x==Blank) (reverse (dropWhile (\x->x==Blank) line))))

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

hyphenate :: HypMap -> Token -> [(Token,Token)]
hyphenate wrdMap tkn = if ( member wrdWithoutPoints wrdMap ) 
 then ( map (\(x, y)->( (HypWord x), (Word (y ++ points)))) (mergers wrdParts) ) else [] 
 where
  wrdParts = wrdMap ! wrdWithoutPoints
  (wrdWithoutPoints, points) = (takeWhile (\ch->ch /= '.') (get tkn), dropWhile (\ch->ch /= '.') (get tkn))

lineBreaks :: HypMap -> Int -> Line -> [(Line, Line)]
lineBreaks wrdMap len line = if (lineLength line) <= len then [breakLine len line] else map (\l->(breakLine len l)) lines
 where
  finalWrdSet = hyphenate wrdMap (last line)
  lines = line : (map (\(wrdStart, wrdEnd)->(init line)++([wrdStart,wrdEnd])) finalWrdSet)

insertBlanks :: Int -> [Token] -> [Token]
insertBlanks _ [] = []
insertBlanks _ [wrd] = [wrd]
insertBlanks spaces (w:wrds) = w:(replicate requiredSpaces Blank) ++ insertBlanks (spaces - requiredSpaces) wrds
 where requiredSpaces = ceiling $ (fromIntegral spaces) / (fromIntegral (length wrds))

divideTextByLines :: (Line, Line) -> Int -> [Line]
divideTextByLines (firstPart, secondPart) maxLength = 
 if (lineLength secondPart) <= maxLength then [firstPart, secondPart] else [firstPart] ++ divideTextByLines (breakLine maxLength secondPart) maxLength

findLargestSeparation :: Line -> Token -> Int -> HypMap -> (Line, Line)
findLargestSeparation currentLine tkn maxLength wrdMap = if (member wrdWithoutPoints wrdMap) then findIt optionsSet else (currentLine, [tkn])
 where 
  findIt separationSet = foldr findLargestFunc (head separationSet) (tail separationSet)
  findLargestFunc x y = if lineLength (fst x) <= maxLength && lineLength (fst x) > lineLength (fst y) then x else y 
  wrdWithoutPoints = takeWhile (\ch->ch /= '.') (get tkn)
  optionsSet = lineBreaks wrdMap maxLength (currentLine++[tkn])

reduceLinesWithSeparation :: (Line, [Line]) -> Line -> Int -> HypMap -> (Line, [Line])
reduceLinesWithSeparation (current, lineSet) [] maxLength wrdMap= ([], lineSet++[current]) 
reduceLinesWithSeparation (current, lineSet) (tkn:tokens) maxLength wrdMap= 
 if lineLength (current++[tkn]) > maxLength
 then reduceLinesWithSeparation (sSep, lineSet++[fSep]) tokens maxLength wrdMap
 else reduceLinesWithSeparation (current++[tkn], lineSet) tokens maxLength wrdMap
 where (fSep, sSep) = findLargestSeparation current tkn maxLength wrdMap

separarYalinear :: Int -> SPFlag -> ALFlag -> String ->  HypMap -> [String]

separarYalinear maxLength NOSEPARAR NOAJUSTAR text wrdMap = map line2string lineSet
 where lineSet = divideTextByLines (breakLine maxLength (string2line text)) maxLength

separarYalinear maxLength NOSEPARAR AJUSTAR text wrdMap = (map adjustFunc (init lineSet)) ++ [line2string (last lineSet)]
 where
  adjustFunc line = line2string (insertBlanks (maxLength - (lineLength line)) line)
  lineSet = divideTextByLines (breakLine maxLength (string2line text)) maxLength

separarYalinear maxLength SEPARAR NOAJUSTAR text wrdMap =  map line2string lineSet
 where lineSet = snd (reduceLinesWithSeparation ([], []) (string2line text) maxLength wrdMap)

separarYalinear maxLength SEPARAR AJUSTAR text wrdMap = map adjustFunc (init lineSet) ++ [line2string (last lineSet)]
 where
  adjustFunc line = line2string (insertBlanks (maxLength - (lineLength line)) line)
  lineSet = snd (reduceLinesWithSeparation ([], []) (string2line text) maxLength wrdMap)