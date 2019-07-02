
import System.Console.ANSI
import Data.Char
import System.IO
import Control.Concurrent


data Diresao = Esq_cima| Esq_baixo | Dir_cima| Dir_baixo
    deriving(Eq,Show)

main :: IO()
main = do
  setCursorPosition 30 10
  leftUp
  clearScreen
  threadDelay 1000000

leftUp :: IO ()
leftUp = do 
   threadDelay 1000000
   clearFromCursorToScreenBeginning
   hCursorUp stdout 1
   hCursorBackward stdout 2
   hPutChar stdout '@'
   saveCursor
   --skateMove
   restoreCursor
   directEC


rigthUp :: IO ()
rigthUp = do 
   threadDelay 1000000
   clearFromCursorToScreenBeginning
   cursorUpLine 1 
   hPutChar stdout '@'
   saveCursor
   --skateMove
   restoreCursor
   directDC

rigthDown :: IO ()
rigthDown = do 
   threadDelay 1000000
   clearFromCursorToScreenBeginning
   cursorDownLine 1 
   hPutChar stdout '@'
   saveCursor
   --skateMove
   restoreCursor
   directDB
   
leftDown :: IO ()
leftDown = do 
   threadDelay 1000000
   clearFromCursorToScreenBeginning
   cursorDownLine 1
   cursorBackward 2 
   hPutChar stdout '@'
   saveCursor
   --skateMove
   restoreCursor
   directEB
   
directEC :: IO()
directEC = do
  bolinha <- getCursorPosition0
  terminal <-getTerminalSize
  case bolinha of 
    Just(x,y)-> case terminal of
                    Just(a,b)-> if x>=0 
                                then 
                                  if y>=b
                                  then
                                     leftDown
                                  else leftUp
                                else if y>=b
                                     then rigthDown
                                     else rigthUp

directEB :: IO()
directEB = return() 


directDB :: IO()
directDB = return()

directDC :: IO()
directDC = return()
