{--
	Command Line Interface
	 for Text Aligner Project
	 by: JOSUERV99
--}

import Prelude hiding (null, lookup, map, filter)
import Data.Map.Lazy hiding (sort,map,foldl)
import Data.Map (fromList)
import Data.Char
import Data.List (sort,map)
import System.IO

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
  let newState = fromList[]
  
  case command of
   "load" -> do
     --fileName <- tokens!!1
     --inh <- openFile fileName ReadMode
     --newState <- cargar inh state
     --hClose inh
     putStrLn $ "File " ++ " was loaded"
     mainloop newState
   "show" -> do
     putStrLn "Showing..."
     mainloop state     
   "ins" -> do
     putStrLn "Showing..."
     mainloop newState
   "save" -> do
     putStrLn ">>> Filename output: "
     --fileName <- getLine
     --outh <- openFile fileName WriteMode
     --descargar outh (sort (toList state))
     --hClose outh
     mainloop newState
   "split" -> do
     putStrLn "Splitting..."
     mainloop newState
   "splitf" -> do
     putStrLn "Splitting to File..."
     mainloop newState
   "exit" -> do
     putStrLn "Exitting..."
   _ -> do
     putStrLn $ "Unknown command ("++ command ++"): '" ++ inpStr ++ "'" 
     mainloop state