#!/usr/bin/env stack

import System.Console.ANSI
import Data.Char
import System.IO




main                    :: IO ()
main =  do hSetEcho stdin False
           hideCursor
           clearScreen
           hPutStr stdout "----------------"
           move

move :: IO ()
move = do  c <- hGetChar stdin
           case (ord c) of
             38 -> moveUp
             40  -> moveDown
             97 -> moveLeft
             100 -> moveRight
             _  -> return ()



moveUp :: IO ()
moveUp = do 
       clearScreen
       hCursorBackward stdout 1
       hCursorUp stdout 1
       hPutStr stdout "***"
       move

moveDown :: IO ()
moveDown = do 
       clearScreen
       hCursorBackward stdout 1
       hCursorDown stdout 1
       hPutStr stdout "***"
       move
moveLeft :: IO ()
moveLeft = do 
       clearScreen
       --hCursorBackward stdout 1
       hCursorBackward stdout 17
       hPutStr stdout "----------------"
       move

moveRight :: IO ()
moveRight = do 
       clearScreen
       hCursorBackward stdout 16
       hCursorForward stdout 1
       hPutStr stdout "----------------"
       move
