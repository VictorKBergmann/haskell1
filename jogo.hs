
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
   directEC

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
  case bolinha of 
    Just(x,y)-> case terminal of
                    Just(a,b)-> if y==0 --parede inferior
                                then rightUp
                                else if x==0 -- parede esquerda
                                then rightDown  
                
                                else leftDown -- nao toca em nenhuma parede 


directDB :: IO()
directDB = do
  bolinha <- getCursorPosition0
  terminal <-getTerminalSize
  case bolinha of 
    Just(x,y)-> case terminal of
                    Just(a,b)-> if y==0 --parede inferior
                                then leftUp
          
                                else if x==a -- parede direita
                                then leftDown
                
                                else rightDown -- nao toca em nenhuma parede 

directDC :: IO()
directDC = do
  bolinha <- getCursorPosition0
  terminal <-getTerminalSize
  case bolinha of 
    Just(x,y)-> case terminal of
                    Just(a,b)-> if x==a -- parede direita
                                then leftUp
                                else if y==b --parede superior
                                then leftDown
                                else rightUp -- nao toca em nenhuma parede
