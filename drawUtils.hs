module DrawUtils where

import UI.NCurses

vLengthAdditive  = 1 + 4 
hLengthAdditive  = 2

stringBox :: String -> Update ()
stringBox stringToDraw = do
  let len = length stringToDraw 
  if len == 0 
    then return ()
    else do
      let hLength = toInteger $ 6*hLengthAdditive + len
          vLength = vLengthAdditive
      pDrawLineH hLength 
      pDrawLineV vLength
      pDrawLineH (-hLength)
      pDrawLineV (-vLength)

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

