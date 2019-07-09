import System.Console.ANSI
import Data.Char
import System.IO
import Control.Concurrent


data Diresao = Esq_cima| Esq_baixo | Dir_cima| Dir_baixo
    deriving(Eq,Show)

main :: IO()
main = do
  hSetEcho stdin False
  hideCursor
  setCursorPosition 25 40
  hPutStr stdout "----------------"
  setCursorPosition 20 (-1)
  clearFromCursorToScreenBeginning
  saveCursor
  leftUp 40

leftUp :: Int->IO ()
leftUp xPlataforma = do 
   threadDelay 100000
   clearScreen
   restoreCursor
   hCursorUp stdout 1
   cursorBackward 2 
   hPutChar stdout '@'
   saveCursor
   skateMove xPlataforma 3
  

rightUp :: Int->IO ()
rightUp xPlataforma = do 
   threadDelay 100000
   restoreCursor
   clearFromCursorToScreenBeginning
   hCursorUp stdout 1
   hPutChar stdout '@'
   saveCursor
   skateMove xPlataforma 1 
   

rightDown :: Int->IO ()
rightDown xPlataforma = do 
   threadDelay 100000
   restoreCursor
   clearFromCursorToScreenBeginning
   hCursorDown stdout 1
   hPutChar stdout '@'
   saveCursor
   skateMove xPlataforma 2

leftDown :: Int->IO ()
leftDown xPlataforma = do 
   threadDelay 100000
   restoreCursor
   clearFromCursorToScreenBeginning
   hCursorDown stdout 1
   cursorBackward 2 
   hPutChar stdout '@'
   saveCursor
   skateMove xPlataforma 4

            
   
directEC :: Int->IO()
directEC xPlataforma = do
  restoreCursor
  bolinha <- getCursorPosition0
  terminal <-getTerminalSize
  
  case bolinha of 
    Just(y,x)-> case terminal of
                    Just(y1,x1)-> if y==1 --parede superior
                                then leftDown xPlataforma
                                else if x== 1 -- parede esquerda
                                then rightUp xPlataforma  
                                else leftUp xPlataforma -- nao toca em nenhuma parede 
                           
         
                
directEB :: Int->IO()
directEB xPlataforma = do
  restoreCursor
  bolinha <- getCursorPosition0
  terminal <-getTerminalSize
  
  case bolinha of 
    Just(y,x)-> case terminal of
                    Just(y1,x1)-> if y==(y1-1) --parede inferior
                                then leftUp xPlataforma
                                else if x==1 -- parede esquerda
                                then rightDown xPlataforma                 
                                else leftDown xPlataforma -- nao toca em nenhuma parede 


directDB :: Int->IO()
directDB xPlataforma = do
  restoreCursor
  bolinha <- getCursorPosition0
  terminal <-getTerminalSize
  
  case bolinha of 
    Just(y,x)-> case terminal of
                    Just(y1,x1)-> if y == (y1-1)  --parede inferior
                                  then rightUp xPlataforma
                                  else if x==(x1-1) -- parede direita
                                  then leftDown xPlataforma
                                  else rightDown xPlataforma -- nao toca em nenhuma parede 

directDC :: Int->IO()
directDC xPlataforma = do
  restoreCursor
  bolinha <- getCursorPosition0
  terminal <-getTerminalSize
  
  case bolinha of 
    Just(y,x)-> case terminal of
                    Just(y1,x1)-> if x==(x1-1) -- parede direita
                                then leftUp xPlataforma
                                else if y==1 --parede superior
                                then rightDown xPlataforma
                                else rightUp xPlataforma -- nao toca em nenhuma parede



skateMove :: Int->Int->IO ()
skateMove xPlataforma direcao = do  
  c <- hGetChar stdin
  case (ord c) of
    97 -> moveLeft (xPlataforma -1) direcao
    100 -> moveRight(xPlataforma +1)direcao
    _  -> return ()
    

moveLeft ::Int->Int->IO ()
moveLeft xPlataforma direcao = do 
       setCursorPosition 25 xPlataforma
       clearFromCursorToLineEnd
       hPutStr stdout "----------------"
       if direcao == 1
       then directDC xPlataforma
       else if direcao == 2
       then directDB xPlataforma
       else if direcao == 3
       then directEC xPlataforma
       else directEB xPlataforma

moveRight ::Int->Int->IO ()
moveRight xPlataforma direcao = do 
       setCursorPosition 25 xPlataforma
       clearFromCursorToLineBeginning
       hPutStr stdout "----------------"
       if direcao == 1
       then directDC xPlataforma
       else if direcao == 2
       then directDB xPlataforma
       else if direcao == 3
       then directEC xPlataforma
       else directEB xPlataforma
