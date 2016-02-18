import DrawUtils as M
import UI.NCurses

main :: IO ()
main = runCurses $ do
  setEcho False
  w <- defaultWindow
  updateWindow w $ do
    moveCursor 1 10 
    --drawString "Hello world!"  
    --moveCursor 3 10 
    --stringBox "Sevi Seviyorum"
    moveCursor 3 10 
    stringBox "chicken chicken \n on the who's the chicken of them all"
    moveCurrentCursor 40 40
    
  render
  waitFor w (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q')

waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor w p = loop where
   loop = do
     ev <- getEvent w Nothing
     case ev of
       Nothing -> loop
       Just ev' -> if p ev' then return () else loop
