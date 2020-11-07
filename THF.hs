-- Text Handling Functions => TextAligner 
-- @author: JosueRV99      => @since: 1/11/2020 
------------------------------------------------
module THF where
import Data.Map (Map, member, (!), fromList)

type Line = [Token]
type HypMap = Data.Map.Map String [String]
data SPFlag = SEPARAR | NOSEPARAR -- Words separation
data ALFlag = AJUSTAR | NOAJUSTAR -- Line  adjustment
data Token = Word String | Blank | HypWord String
             deriving (Eq,Show)

enHyp :: HypMap -- for testing...
enHyp = Data.Map.fromList [ ("controla",["con","tro","la"]), ("futuro",["fu","tu","ro"]),("presente",["pre","sen","te"])]

texto = "Aquel que controla el pasado controla el futuro. Aquel que controla el presente controla el pasado."

{-- get string from token types --}
get :: Token -> String
get tkn = case tkn of
  (Word w)    -> w
  (Blank)     -> " "
  (HypWord w) -> w ++ "-"

{-- convert string to token list --}
string2line :: String -> Line
string2line text = map (\wrd-> Word wrd) $ words text

{-- convert token list to string --}
line2string :: Line -> String
line2string line = unwords $ map (\tkn->if tkn/=Blank then get tkn else "") (reverse (dropWhile (\x->x==Blank) (reverse (dropWhile (\x->x==Blank) line))))

{-- calc token length --}
tokenLength :: Token -> Int
tokenLength tkn = case tkn of 
  (Word w)    -> length w
  (Blank)     -> 1 
  (HypWord w) -> length w + 1

{-- calc the line length with spaces between tokens --}
lineLength :: Line -> Int
lineLength line = sum (map tokenLength line) + if (length line) <= 1 then 0 else (length line)-1

{-- break a line in two parts using the given len --}
breakLine :: Int -> Line -> (Line,Line)
breakLine _ [] = ([], [])
breakLine len line = (take breakIndex line, drop breakIndex line) 
 where
  calcBreakIdx _ []         = 0
  calcBreakIdx len (l:line) = if (tokenLength l) <= len && (l:line) /= [] then 1 + (calcBreakIdx (len - (tokenLength l)-1) line) else 0
  breakIndex                = calcBreakIdx len line

{-- generate all chances to concatenate the complete word --}
mergers :: [String] -> [(String, String)]
mergers [x] = []
mergers (str:strList) = [(str, concat strList)] ++ mergers (concat [[str++(head strList)], tail strList])

{-- separate a Word token from all possible ways that can be derived --}
hyphenate :: HypMap -> Token -> [(Token,Token)]
hyphenate wrdMap tkn = 
 if   ( member wrdWithoutPoints wrdMap ) 
 then map (\(x, y)->(HypWord x, Word (y ++ points))) (mergers wrdParts) else [] 
 where 
  wrdParts                   = wrdMap ! wrdWithoutPoints
  (wrdWithoutPoints, points) = (takeWhile (\ch->ch /= '.') (get tkn), dropWhile (\ch->ch /= '.') (get tkn))

{-- find the different ways a line can be separate with the different ways the last word can be divided --}
lineBreaks :: HypMap -> Int -> Line -> [(Line, Line)]
lineBreaks wrdMap len line = 
 if (lineLength line) <= len 
 then [breakLine len line] else map (\line->(breakLine len line)) lines
 where
  finalWrdSet = hyphenate wrdMap $ last line
  lines       = line:map (\(wrdStart, wrdEnd)->(init line)++([wrdStart,wrdEnd])) finalWrdSet

{-- distribute a specified number of blanks between words --}
insertBlanks :: Int -> [Token] -> [Token]
insertBlanks _ [] = []
insertBlanks _ [wrd] = [wrd]
insertBlanks spaces (w:wrds) = w:(replicate requiredSpaces Blank) ++ insertBlanks (spaces - requiredSpaces) wrds
 where requiredSpaces        = ceiling $ (fromIntegral spaces) / (fromIntegral (length wrds))

{-- break the line to convert to token line --}
divideTextByLines :: (Line, Line) -> Int -> [Line]
divideTextByLines (firstPart, secondPart) maxLength = 
 if   (lineLength secondPart) <= maxLength 
 then [firstPart, secondPart] else firstPart : divideTextByLines (breakLine maxLength secondPart) maxLength

{-- find the best way to separate and use less spaces --}
findLargestSeparation :: Line -> Token -> Int -> HypMap -> (Line, Line)
findLargestSeparation currentLine tkn maxLength wrdMap = 
 if (member wrdWithoutPoints wrdMap) 
 then findIt optionsSet else (currentLine, [tkn])
 where 
  findIt (s:separationSet) = foldr findLargest s separationSet
  findLargest x y          = if lineLength (fst x) <= maxLength && lineLength (fst x) > lineLength (fst y) then x else y 
  wrdWithoutPoints         = takeWhile (\ch->ch /= '.') (get tkn)
  optionsSet               = lineBreaks wrdMap maxLength (currentLine++[tkn])

{-- reduce lines using the different ways to separate the last word --}
reduceLinesWithSeparation :: (Line, [Line]) -> Line -> Int -> HypMap -> (Line, [Line])
reduceLinesWithSeparation (current, lineSet) [] maxLength wrdMap           = ([], lineSet++[current]) 
reduceLinesWithSeparation (current, lineSet) (tkn:tokens) maxLength wrdMap = 
 if    lineLength (current++[tkn]) > maxLength
 then  reduceLinesWithSeparation (sSep, lineSet++[fSep])   tokens maxLength wrdMap
 else  reduceLinesWithSeparation (current++[tkn], lineSet) tokens maxLength wrdMap
 where (fSep, sSep) = findLargestSeparation current tkn maxLength wrdMap

{-- convert text to lines with separation and adjustment modes --}
separarYalinear :: Int -> SPFlag -> ALFlag -> String -> HypMap -> [String]
separarYalinear maxLength sFlag aFlag text wrdMap = 
 case (sFlag, aFlag) of
  (NOSEPARAR, NOAJUSTAR) -> map line2string lineSet
   where lineSet         =  divideTextByLines (breakLine maxLength (string2line text)) maxLength
  (NOSEPARAR,   AJUSTAR) -> map adjustFunc (init lineSet) ++ [line2string (last lineSet)]
   where lineSet         =  divideTextByLines (breakLine maxLength (string2line text)) maxLength
  (SEPARAR,   NOAJUSTAR) -> map line2string lineSet
   where (_, lineSet)    =  reduceLinesWithSeparation ([], []) (string2line text) maxLength wrdMap
  (SEPARAR,     AJUSTAR) -> map adjustFunc (init lineSet) ++ [line2string (last lineSet)]
   where (_, lineSet)    =  reduceLinesWithSeparation ([], []) (string2line text) maxLength wrdMap
 where adjustFunc line   =  line2string (insertBlanks (maxLength - (lineLength line)) line)