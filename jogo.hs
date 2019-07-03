import System.Console.ANSI
import Data.Char
import System.IO
import Control.Concurrent


data Diresao = Esq_cima| Esq_baixo | Dir_cima| Dir_baixo
    deriving(Eq,Show)

main :: IO()
main = do
  setCursorPosition 10 3
  clearScreen
  saveCursor
  leftUp

leftUp :: IO ()
leftUp = do 
   threadDelay 1000000
   clearScreen
   restoreCursor
   hCursorUp stdout 1
   cursorBackward 2 
   hPutChar stdout '@'
   saveCursor
   --skateMove
   --restoreCursor
   directEC

rightUp :: IO ()
rightUp = do 
   threadDelay 1000000
   clearScreen
   restoreCursor
   hCursorUp stdout 1
   hPutChar stdout '@'
   saveCursor
   --skateMove
   restoreCursor
   directDC

rightDown :: IO ()
rightDown = do 
   threadDelay 1000000
   clearScreen
   restoreCursor
   hCursorDown stdout 1
   hPutChar stdout '@'
   saveCursor
   --skateMove
   restoreCursor
   directDB

leftDown :: IO ()
leftDown = do 
   threadDelay 1000000
   clearScreen
   restoreCursor
   hCursorDown stdout 1
   cursorBackward 2 
   hPutChar stdout '@'
   saveCursor
   --skateMove
   --restoreCursor
   directEB
            
   
directEC :: IO()
directEC = do
  bolinha <- getCursorPosition0
  terminal <-getTerminalSize
  print (show bolinha)
  case bolinha of 
    Just(x,y)-> case terminal of
                    Just(a,b)-> if y==1 --parede inferior
                                then rightUp
                                else if x==0 -- parede esquerda
                                then leftDown  
                                else leftUp -- nao toca em nenhuma parede 
                           
         
                
directEB :: IO()
directEB = do
  bolinha <- getCursorPosition0
  terminal <-getTerminalSize
  print (show bolinha)
  case bolinha of 
    Just(x,y)-> case terminal of
                    Just(a,b)-> if y==1 --parede inferior
                                then rightUp
                                else if x==a -- parede esquerda
                                then leftUp                 
                                else leftDown -- nao toca em nenhuma parede 


directDB :: IO()
directDB = do
  bolinha <- getCursorPosition0
  terminal <-getTerminalSize
  print (show bolinha)
  case bolinha of 
    Just(x,y)-> case terminal of
                    Just(a,b)-> if y==b --parede inferior
                                then leftDown
                                else if x==a -- parede direita
                                then leftUp
                                else rightDown -- nao toca em nenhuma parede 

directDC :: IO()
directDC = do
  bolinha <- getCursorPosition0
  terminal <-getTerminalSize
  print (show bolinha) 
  case bolinha of 
    Just(x,y)-> case terminal of
                    Just(a,b)-> if y==b -- parede direita
                                then leftUp
                                else if x==0 --parede superior
                                then rightDown
                                else rightUp -- nao toca em nenhuma parede
