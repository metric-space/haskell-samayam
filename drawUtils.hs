module DrawUtils where

import UI.NCurses 
import Data.List

notFunction :: Int -> Int
notFunction 0 = 1
notFunction 1 = 0

vLengthAdditive  = 2  :: Int
hLengthAdditive  = 2  :: Int

stringBox :: String -> Update ()
stringBox stringToDraw = do
  let decons = lines stringToDraw
      len = length . head . sort $ decons  
  if len == 0 
    then return ()
    else do
      let hLength = toInteger  (2*hLengthAdditive + len) -- addition for padding
          vLength = toInteger  (2*vLengthAdditive + length decons)
          mappedIOFunction (index,word) =  do 
              -- got to clean this up soon
              moveCurrentCursor (toInteger $ vLengthAdditive*(1 - index) + index) (toInteger $ hLengthAdditive*(notFunction index))
              drawString_ word
      (_,_) <- drawBox_ hLength vLength
      mapM_ mappedIOFunction $ zip (0:[1,1..]) decons
      return ()
            
drawBox_ :: Integer -> Integer -> Update(Integer,Integer)
drawBox_ h_ v = do
  let h = h_
  pDrawLineH h 
  pDrawLineV v
  pDrawLineH (-h)
  pDrawLineV (-v)
  (currentX, currentY) <- cursorPosition 
  return (currentX, currentY)

-- raison d'etre : after drawing a line, 
-- program jumps back to the start of the line
-- AND the library cannot draw lines of negative arguments
pDrawLineH ::Integer -> Update ()
pDrawLineH x =  do
  (currentX, currentY) <- cursorPosition  
  if x >= 0 
    then do
      drawLineH Nothing x 
      moveCursor currentX (currentY + x)
    else do
      moveCursor currentX (currentY+x)
      drawLineH Nothing (abs x)

-- raison d'etre : after drawing a line, 
-- program jumps back to the start of the line
-- AND the library cannot draw lines of negative arguments
pDrawLineV ::Integer -> Update ()
pDrawLineV x =  do
  (currentX, currentY) <- cursorPosition  
  if x >= 0 
    then do
      drawLineV Nothing x 
      moveCursor (currentX + x) currentY
    else do
      moveCursor (currentX +x) currentY
      drawLineV Nothing (abs x)

-- raison d'etre : move cursor from current location to destination
moveCurrentCursor :: Integer -> Integer -> Update ()
moveCurrentCursor x y  = do 
  (currentX, currentY) <- cursorPosition 
  moveCursor (currentX + x) (currentY + y)

drawString_ :: String -> Update()
drawString_ stringToDraw = do
  drawString stringToDraw
  (currentX, currentY) <- cursorPosition 
  moveCursor currentX (currentY - (toInteger $ (length stringToDraw)))

