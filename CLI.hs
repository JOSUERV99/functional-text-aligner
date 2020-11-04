-- Command Line Interface  => TextAligner 
-- @author: JosueRV99      => @since: 3/11/2020 
------------------------------------------------
module CLI where

import THF -- Text Handling Functions Module from THF.hs
import Prelude hiding (null, lookup, filter)
import Data.Map (fromList, member, Map, insert, toList, size)
import Data.Char (toLower)
import Data.List (sort, intercalate)
import Data.Typeable
import System.IO

type State = Map String [String]

{-- main door --}
main :: IO ()
main = do 
 mainloop (fromList[])

{-- main loop to interact with THF using CLI --}
mainloop :: State -> IO ()
mainloop state = do

  putStr "[TextAligner] ~> "
  inpStr <- getLine
  let tokens  = words inpStr
      command = tokens!!0
  
  case command of

   "load" -> do 
   -- load filename 
     let filename = tokens!!1
     inh <- openFile filename ReadMode
     newState <- loadDict inh $ fromList[]
     hClose inh
     putStrLn $ "(Dict was loaded:"++ show (size newState) ++" words)"
     mainloop newState

   "show" -> do
     putStrLn $ show (toList state) ++ "\n" ++ (show (size state)) ++ " words"
     mainloop state    

   "ins" -> do
   -- ins word di-vi-ded-word
     let word = tokens!!1
     let separations = words [if c == '-' then ' ' else c|c <- tokens!!2]
     let newState = record state word separations
     putStrLn $ "("++ word ++" was inserted)"
     mainloop newState 

   "save" -> do 
   -- save filename 
     let outFilename = tokens!!1
     outh <- openFile outFilename WriteMode
     saveDict outh $ sort (toList state)
     hClose outh
     putStrLn $ "(File "++ outFilename ++" was writed)"
     mainloop state

   "split" -> do 
   -- split length separar? ajustar? text....
     putStrLn $ "\n"++(processText tokens state) ++ "\n"
     mainloop state 

   "splitf" -> do 
    -- splitf length separar? ajustar? inFilename outFilename
     processTextUsingFiles tokens state
     mainloop state 

   "exit" -> do 
     putStrLn "Bye..."

   _ -> do 
     putStrLn $ "Unknown command ("++ command ++"): '" ++ inpStr ++ "'" 
     mainloop state 

{-- record a new word-separation into the state --}
record :: State -> String -> [String] -> State
record state word separations = 
 if member word state then state else insert word separations state 

{-- load dict (word-separation) from file --}
loadDict :: Handle -> State -> IO State
loadDict inh state = do
      ineof <- hIsEOF inh
      if ineof then return state
               else do 
                inpStr <- hGetLine inh
                let tkns = words inpStr
                    word = head tkns
                    separations = words [if c == '-' then ' ' else c|c <- last tkns]
                    nuevoestado = record state word separations
                loadDict inh nuevoestado

{-- save dict of (word-separation) from the state --}
saveDict :: Handle -> [(String,[String])] -> IO ()
saveDict outh [] = return ()
saveDict outh ((word,separations):wordSet) = do 
 hPutStrLn outh $ word ++ " " ++ (intercalate "-" separations)
 saveDict outh wordSet

{-- process text using THF --}
processText :: [String] -> State -> String
processText tokens state = do
 let maxLengthPerLine = read (tokens!!1)::Int
     separationFlag   = if (map toLower (tokens!!2)) == "s" then SEPARAR else NOSEPARAR
     adjustmentFlag   = if (map toLower (tokens!!3)) == "s" then AJUSTAR else NOAJUSTAR
     textToAlign      = unwords $ drop 4 tokens
     alignedText      = intercalate "\n" $ separarYalinear maxLengthPerLine separationFlag adjustmentFlag textToAlign state
 alignedText

{-- save dict of (word-separation) from the state --}
saveTextToFile :: Handle -> [String] -> IO ()
saveTextToFile outh [] = return ()
saveTextToFile outh (l:lines) = do 
 hPutStrLn outh l
 saveTextToFile outh lines

{-- process text and save in a file --}
processTextUsingFiles :: [String] -> State -> IO ()
processTextUsingFiles tokens state = do
 outh <- openFile (tokens!!5) WriteMode -- out file
 content <- readFile (tokens!!4)        -- in  file

 let maxLengthPerLine = read (tokens!!1)::Int
     separationFlag   = if (tokens!!2) == "s" then SEPARAR else NOSEPARAR
     adjustmentFlag   = if (tokens!!3) == "s" then AJUSTAR else NOAJUSTAR
     textToAlign      = lines content
 
 -- using THF for adjust every line in the file content
 let alignedText      = separarYalinear maxLengthPerLine separationFlag adjustmentFlag (unwords textToAlign) state

 putStrLn $ "\n"++(intercalate "\n" alignedText)++"\n"

 if length tokens == 6 then do 
  saveTextToFile outh alignedText -- write to file
  hClose outh -- close the stream
  return ()
 else
  return ()