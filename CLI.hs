{--
	Command Line Interface
	 for Text Aligner Project
	  by: JOSUERV99
--}
import Prelude hiding (null, lookup, map, filter)
import Data.Map (fromList, member, Map, insert)
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
     let fileName = tokens!!1
     inh <- openFile fileName ReadMode
     newState <- load inh state
     hClose inh
     putStrLn $ "File " ++ " was loaded"
     mainloop newState

   "show" -> do
     putStrLn $ show state
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
     putStrLn "Bye..."

   _ -> do
     putStrLn $ "Unknown command ("++ command ++"): '" ++ inpStr ++ "'" 
     mainloop state

record :: State -> String -> State
record state str = 
 if member str state then state else insert key value state 
 where 
  key = head (words str)
  value = words [if c == '-' then ' ' else c|c <- (last (words str))]

load :: Handle -> State -> IO State
load inh state = do
      ineof <- hIsEOF inh
      if ineof then return state
               else do inpStr <- hGetLine inh
                       let nuevoestado = record state inpStr
                       load inh nuevoestado