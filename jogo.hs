import System.Console.ANSI
import Data.Char
import System.IO
import Control.Concurrent

main                    :: IO ()
main =  do hSetEcho stdin False
           hideCursor
           clearScreen
           hPutChar stdout '*'
           move

move :: IO ()
move = do  c <- hGetChar stdin
           case (ord c) of
             114 -> moveUp
             99  -> moveDown
             100 -> moveLeft
             102 -> moveRight
             _  -> return ()


moveUp :: IO ()
moveUp = do 
       clearScreen
       hCursorBackward stdout 1
       hCursorUp stdout 1
       hPutChar stdout '*'
       test

moveDown :: IO ()
moveDown = do 
       clearScreen
       hCursorBackward stdout 1
       hCursorDown stdout 1
       hPutChar stdout '*'
       move
moveLeft :: IO ()
moveLeft = do 
       clearScreen
       hCursorBackward stdout 1
       hCursorBackward stdout 1
       hPutChar stdout '*'
       move

moveRight :: IO ()
moveRight = do 
       clearScreen
       hCursorForward stdout 1
       hPutChar stdout '*'
       test


test                    :: IO ()
test =  do hSetEcho stdin False
           hideCursor
           clearScreen
           hPutChar stdout '*'
           test1

test1 :: IO ()
test1 = do threadDelay 100000
           moveUp
           test1









-----------------------------------

data Diresao = Esq_cima| Esq_baixo | Dir_cima| Dir_baixo
        deriving(Eq,Show)


leftUp :: IO ()
leftUp = do 
   ClearFromCursorToBegining
   cursorUpLine 1 
   cursorBackward 2
   putChar '@'
   savecrusor
   --skateMove
   restoreCursor
   direct Esq_cima


rigthUp :: IO ()
rigthUp = do 
   ClearFromCursorToBegining
   cursorUpLine 1 
   putChar '@'
   savecrusor
   --skateMove
   restoreCursor
   direct Dir_cima

rigthDown :: IO ()
rigthDown = do 
   ClearFromCursorToBegining
   cursorDownLine 1 
   putChar '@'
   savecrusor
   --skateMove
   restoreCursor
   direct Dir_baixo
   
leftDown :: IO ()
leftDown = do 
   ClearFromCursorToBegining
   cursorDownLine 1
   cursorBackward 2 
   putChar '@'
   savecrusor
   --skateMove
   restoreCursor
   direct Esq_baixo
   
direct :: Diresao ->IO()
direct Esq_cima = do
   bolinha <- getCursorPosition0
   terminal <-getTerminalSize
   case bolinha of 
   	 Just(x,y)->
   	 case terminal of
       Just(a,b)->
       if x>=a 
       then 
         if y>=b
         then
           leftDown
         else do
           leftUp
       else do 
       	 if y>=b
       	 then
           rigthDown
         else do
           rigthUp

