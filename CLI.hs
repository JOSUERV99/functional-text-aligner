{--
	Command Line Interface
	 for Text Aligner Project
	  by: JOSUERV99
--}

import Prelude hiding (null, lookup, map, filter)
import Data.Map (fromList, member, Map, insert, toList, size)
import Data.Char (toLower)
import Data.List (sort, intercalate)
import System.IO

import THF -- Text Handling Functions Module from THF.hs

type State = Map String [String]

main :: IO ()
main = do 
 mainloop (fromList[])

mainloop :: State -> IO ()
mainloop state = do

  putStr ">> "
  inpStr <- getLine
  let tokens  = words inpStr
  let command = tokens!!0
  
  case command of

   "load" -> do 
     let fileName = tokens!!1
     inh <- openFile fileName ReadMode
     newState <- loadDict inh state
     hClose inh
     putStrLn $ "Dict was loaded ("++ show (size newState) ++" words)"
     mainloop newState

   "show" -> do
     putStrLn $ show state
     mainloop state    

   "ins" -> do
     let word = tokens!!1
     let separations = words [if c == '-' then ' ' else c|c <- tokens!!2]
     let newState = record state word separations
     putStrLn $ "Inserting "++word
     mainloop newState 

   "save" -> do 
     putStrLn ">>> Filename output: "
     let fileName = tokens!!1
     outh <- openFile fileName WriteMode
     saveDict outh $ sort (toList state)
     hClose outh
     mainloop state

   "split" -> do 
     putStrLn "Splitting..."
     let splittedText = processText tokens state
     putStrLn splittedText
     mainloop state 

   "splitf" -> do 
     let fileName = tokens!!1
     -- outh <- openFile fileName WriteMode
     -- processTextUsingFiles outh 
     mainloop state 

   "exit" -> do 
     putStrLn "Bye..."

   _ -> do 
     putStrLn $ "Unknown command ("++ command ++"): '" ++ inpStr ++ "'" 
     mainloop state 

record :: State -> String -> [String] -> State
record state word separations = if member word state then state else insert word separations state 

loadDict :: Handle -> State -> IO State
loadDict inh state = do
      ineof <- hIsEOF inh
      if ineof then return state
               else do 
                inpStr <- hGetLine inh
                let tkns = words inpStr
                let word = head tkns
                let separations = words [if c == '-' then ' ' else c|c <- last tkns]
                let nuevoestado = record state word separations
                loadDict inh nuevoestado

saveDict :: Handle -> [(String,[String])] -> IO ()
saveDict outh [] = return ()
saveDict outh ((word,separations):wordSet) = do 
 hPutStrLn outh $ word ++ " " ++ (intercalate "-" separations)
 saveDict outh wordSet

processText :: [String] -> State -> String
processText tokens state = do
 let maxLengthPerLine = read (tokens!!1)::Int
 let separationFlag   = if toLower ((tokens!!2)!!0) == 's' then SEPARAR else NOSEPARAR
 let adjustmentFlag   = if toLower ((tokens!!3)!!0) == 's' then AJUSTAR else NOAJUSTAR
 let textToAlign      = unwords $ drop 4 tokens
 let splittedText     = intercalate "\n" $ separarYalinear maxLengthPerLine separationFlag adjustmentFlag textToAlign state
 splittedText

processTextUsingFiles :: Handle -> [String] -> State -> IO ()
processTextUsingFiles outh tokens state = do
 let splittedText = processText tokens state
 hPutStrLn outh splittedText
 putStrLn splittedText
 return ()

