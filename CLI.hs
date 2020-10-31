{--
	Command Line Interface
	 for Text Aligner Project
	 by: JOSUERV99
--}

import System.IO

type Dictionary = Map [Char] [[Char]]

main :: IO ()
main = do ->
       mainloop (fromList[])

mainloop :: Dictionary -> IO ()
mainloop dict = do
  putStr ">> "
  inpStr <- getLine