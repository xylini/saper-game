import Control.Monad
import System.Random
import Data.List
import System.IO
import Data.Char

data State a = Mine | Unexplored | Clue a deriving(Show)

type Point    = (Int, Int)
type World    = [[State Int]]
type ClueMap  = [[Int]]
type Explored = [[State Int]]

size   = 1  -- wymiar komorki
width = 10  -- szerokość planszy
height = 10 -- wysokość planszy
minesIl = 5 -- ilosc bomb na planszy
lastBorderEl = intToDigit (width-1)   -- ustala ostatni nr el planszy w poziomie
lastHeightEl = intToDigit (height-1)  -- ustala ostani nr el planszy w pionie
borderWidth = ['0'..lastBorderEl]     -- tablica indexow szerokosci na planszy
borderHeight = ['0'..lastHeightEl]

   
{- Startujemy program i najpierw tworzymy
   RandomGen który jest używany jako nasiono do plantowania min.
   Potem tworzymy dwa World'y: "explored" i "world",
     - "explored" bedzie tym co bedzie wyswietlone
	 - "world" bedzie przechowywal lokalizacje min
	 Po tym startujemy gre.
  -}
   
main :: IO ()
main = do
  g        <- getStdGen -- globalny generator liczb losowych
  explored <- return $ makeMatrix width height Unexplored -- wszystko jest "Unexplored"
  world    <- return $ genGame width height minesIl g
  runGame explored world >>= showWorld

{- Tutaj mamy pętlę w której działa gra,
     - najpierw rysujemy tablice
     - nastepnie wczytujemy podana lokalizacje do sprawdzenia przez grasza
     - potem sprawdzamy "world" w miejscy podanym przez gracza.
   Gra się kończy gdy gracz popełnił błąd i pokazała się mina.
   Pętla działa na zasadzie rekurencji w której wywołujemy runGame
	ze sprawdzoną planszą. -}
runGame :: Explored -> World -> IO Explored
runGame e w = do
  showWorld e
  move <- getLine
  newE <- return $ explore w (parseInput move) e
  case isMineVisible newE of
      True  -> return newE
      False -> runGame newE w

{- Mamy dwa przypadki które nas interesują, kiedy sprawdzamy ruch gracza,
     - wyznaczone miejsce nie było jeszcze sprawdzone
     - wyznaczone miejsce juz było sprawdzane więc nic nie robimy -}
explore :: World -> Point -> Explored -> Explored
explore w p@(x, y) e = case e !! x !! y of
                         Unexplored -> updateExplored w p e
                         _          -> e

   {- By zaaktualizować przeszykaną mapę, kopiujemy stan
   z "world" do przeszukanej mapy (planszy).
   Jeżeli mamy stan Clue 0, przeszykujemy
   "komórki" w otoczeniu by oszczędzić graczowi
   niepotrzebną pracę związaną z odkrywaniem miejsc
   bez podpowiedzi   -}
   
updateExplored :: World -> Point -> Explored -> Explored
updateExplored w p@(x, y) e =
    case state of
      Clue 0 -> foldr (explore w) newExplored $ surrounding width height p
      _      -> newExplored
    where
      state       = (w !! x !! y)
      newExplored = replaceMatrixIndex p e state

{- Sprawdza czy  mina jest widoczna -}
isMineVisible :: Explored -> Bool
isMineVisible [] = False
isMineVisible (xs:xss)
              | containsMine xs = True
              | otherwise       = isMineVisible xss
              where containsMine xs = not $ all notMine xs
                    notMine Mine    = False
                    notMine _       = True

                                      
{- Parsuje podana przez uzytkownika pare punktow w punkt -}
parseInput :: String -> Point
parseInput line = listToPair $ map read $ words line
    where listToPair (x:y:_) = (x, y)


{- Generuje world o wzmiarach w h z n iloscia bomb
   rozrzuconych losowo oraz wylicza wszystkie wskazowki. -}
genGame :: RandomGen g => Int -> Int -> Int -> g -> World
genGame w h n g = [zipWith combine ms cs | (ms, cs) <- zip mineMap clueMap]
                   where
                     mines   = nub $ genPoints w h n g
                     clueMap = genClueMap w h mines
                     mineMap = genWorld w h n mines
                     combine :: (State a) -> a -> State a
                     combine Mine _       = Mine
                     combine Unexplored _ = Unexplored
                     combine (Clue _) x   = Clue x
                                            
{- Generuje losowe punkty w obszarze o wymiarze w na h -}
genPoints :: RandomGen g => Int -> Int -> Int -> g -> [Point]
genPoints w h n g = zip xs ys
    where
      xs = take n (randomRs (0, w-1) g)
      ys = drop n $ take (n*2) $ randomRs (0, h-1) g

{- Generuje world o wymiarach w na h i rozrzuca w nim losowo miny  -}
genWorld :: Int -> Int -> Int -> [Point] -> World
genWorld w h n mines = foldr placeMine world mines
    where
      world             = makeMatrix w h (Clue 0) -- Gdy tworze world to nie mam zadnych min
      placeMine p world = replaceMatrixIndex p world Mine

{- Generuje mape ze wskazowkami w kazdej komorce ktore sa
   liczba sasiadujacych z nia min -}
genClueMap :: Int -> Int -> [Point] -> ClueMap
genClueMap w h mines = foldr succPoint clueMap surroundingPoints
    where
      surroundingPoints          = concat $ map (surrounding w h) mines
      clueMap                    = replicate w $ replicate h 0
      succPoint p@(x, y) clueMap = replaceMatrixIndex p clueMap
                                   $ succ (clueMap !! x !! y)
                                     
{- Znajduje wszystie otaczajace punkty dla podanego punktu
   oraz odfiltrowuje punkty spoza mapy -}
surrounding :: Int -> Int -> Point -> [Point]
surrounding w h (x, y) =
    filter (inBounds w h) [(x-1, y+1), (x, y+1), (x+1, y+1),
                           (x-1, y),             (x+1, y),
                           (x-1, y-1), (x, y-1), (x+1, y-1)]

{- Pokazuje nam world -}
showWorld ::  World -> IO ()
showWorld w = putStrLn $ showMatrixWith showSquare w

showSquare :: State Int -> String
showSquare Mine       = showCentered size "*"
showSquare Unexplored = showCentered size "x"
showSquare (Clue 0)   = showCentered size " "
showSquare (Clue n)   = showCentered size (show n)

showCentered :: Int -> String -> String
showCentered w x = (replicate leftPad ' ') ++ x ++ (replicate rightPad ' ')
    where leftPad  =  w `div` 2
          rightPad =  w - leftPad - (length x)

{- Mapuje funkcje na liscie list -}
matrixMap :: (a -> b) -> [[a]] -> [[b]]
matrixMap f xss = map (map f) xss

showMatrixWith :: (a -> String) -> [[a]] -> String
showMatrixWith f = unlines . addBorder . map concat . matrixMap f . transpose

{- Dodaje granice/kontur na liscie list -}
addBorder :: [String] -> [String]
addBorder xs = [horizontalBorder w]
               ++ map verticalBorder xs
               ++ [(horizontalBorder w)]
    where w                  = length (xs !! 0)
          h                  = length xs
          horizontalBorder w =  "+" ++ borderWidth ++ "+"
          verticalBorder xs  = "|" ++ xs ++ "|"

replaceMatrixIndex :: Point -> [[a]] -> a -> [[a]]
replaceMatrixIndex (x, y) m e = replaceIndex x m $ replaceIndex y (m !! x) e

{- Zamienia element pod indexem w liście z innym elementem -}
replaceIndex :: Int -> [a] -> a -> [a]
replaceIndex index xs x = take index xs ++ ( x : (drop (index+1) xs))

{- Tworzy macierz wypelniona podanym elementem -}
makeMatrix :: Int -> Int -> a -> [[a]]
makeMatrix w h e = replicate w $ replicate h e

inBounds :: Int -> Int -> Point -> Bool
{- Sprawdza czy jestesmy w obszarze "planszy" -}
inBounds w h (x, y)
    | x < 0     = False
    | x >= w    = False
    | y < 0     = False
    | y >= h    = False
    | otherwise = True
