

import System.Console.ANSI
import Data.Char
import System.IO


main ::IO()
main = do
  hSetEcho stdin False
  setCursorPosition 25 40
  hPutStr stdout "----------------"
  skateMove 40


skateMove :: Int->IO ()
skateMove xPlataforma = do  
  c <- hGetChar stdin
  case (ord c) of
    97 -> moveLeft (xPlataforma -1)
    100 -> moveRight(xPlataforma +1)
    _  -> return ()


moveLeft ::Int->IO ()
moveLeft xPlataforma = do 
       setCursorPosition 25 xPlataforma
       clearFromCursorToLineEnd
       hPutStr stdout "----------------"
       skateMove xPlataforma

moveRight ::Int->IO ()
moveRight xPlataforma = do 
       setCursorPosition 25 xPlataforma
       clearFromCursorToLineBeginning
       hPutStr stdout "----------------"
       skateMove xPlataforma

