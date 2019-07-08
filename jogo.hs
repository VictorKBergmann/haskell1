import System.Console.ANSI
import Data.Char
import System.IO
import Control.Concurrent

skateinit :: IO()
skateinit = do
  terminal <-getTerminalSize
  case terminal of 
    Just(y,x)-> do setCursorPosition (y-3) 
                   saveCursor

main :: IO()
main = do
  clearScreen
  skateinit
  v<-getCursorPosition0
  print (show v)
  setCursorPosition 10 25
  hideCursor
  rightDown

leftUp :: IO ()
leftUp = do 
   threadDelay 100000
   clearScreen
   hCursorUp stdout 1
   cursorBackward 2 
   hPutChar stdout '@'
   directEC

rightUp :: IO ()
rightUp = do 
   threadDelay 100000
   clearScreen
   hCursorUp stdout 1
   hPutChar stdout '@'
   directDC

rightDown :: IO ()
rightDown = do 
   threadDelay 100000
   clearScreen
   hCursorDown stdout 1
   hPutChar stdout '@'
   directDB

leftDown :: IO ()
leftDown = do 
   threadDelay 100000
   clearScreen
   hCursorDown stdout 1
   cursorBackward 2 
   hPutChar stdout '@'
   directEB
            
directEC :: IO()
directEC = do
  bolinha <- getCursorPosition0
  terminal <-getTerminalSize
  
  case bolinha of 
    Just(y,x)-> case terminal of
                    Just(y1,x1)-> if y==0 --parede superior
                                then leftDown
                                else if x == 1 -- parede esquerda
                                then rightUp  
                                else leftUp -- nao toca em nenhuma parede 
          
directEB :: IO()
directEB = do
  bolinha <- getCursorPosition0
  terminal <-getTerminalSize
  
  case bolinha of 
    Just(y,x)-> case terminal of
                    Just(y1,x1)-> if y==(y1-4) --parede inferior
                                then leftUp
                                else if x==1 -- parede esquerda
                                then rightDown                 
                                else leftDown -- nao toca em nenhuma parede 

directDC :: IO()
directDC = do
  bolinha <- getCursorPosition0
  terminal <-getTerminalSize
  
  case bolinha of 
    Just(y,x)-> case terminal of
                    Just(y1,x1)-> if x==(x1-1) -- parede direita
                                then leftUp
                                else if y==1 --parede superior
                                then rightDown
                                else rightUp -- nao toca em nenhuma parede

directDB :: IO()
directDB = do
  bolinha <- getCursorPosition0
  terminal <-getTerminalSize
  
  case bolinha of 
    Just(y,x)-> case terminal of
                    Just(y1,x1)-> if y == (y1-4) --parede inferior
                                then do
                                  restoreCursor
                                  p<-getCursorPosition0
                                  print (show p)
                                  move  
                                  saveCursor
                                  setCursorPosition y x
                                  rightUp
                                else if x==(x1-1) -- parede direita
                                then do
                                  restoreCursor
                                  p<-getCursorPosition0
                                  print (show p)
                                  move  
                                  saveCursor
                                  setCursorPosition y x
                                  leftDown
                                else do
                                  restoreCursor
                                  p<-getCursorPosition0
                                  print (show p)
                                  move  
                                  saveCursor
                                  setCursorPosition y x
                                  rightDown -- nao toca em nenhuma parede 

move :: IO ()
move = do  c <- hGetChar stdin         
           case (ord c) of
             100 -> moveLeft
             102 -> moveRight
             _  -> return ()

moveLeft :: IO ()
moveLeft = do 
       clearScreen
       hCursorBackward stdout 1
       hCursorBackward stdout 1
       hPutStr stdout "___________"
       saveCursor
       return() 

moveRight :: IO ()
moveRight = do 
       clearScreen
       hCursorForward stdout 1
       hPutStr stdout "___________"
       saveCursor
       return() 


