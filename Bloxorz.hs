{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}


module Bloxorz where

import ProblemState

import qualified Data.Matrix as M

{-
    Caracterele ce vor fi printate pentru fiecare tip de obiect din joc 
    Puteți înlocui aceste caractere cu orice, în afară de '\n'.
-}

hardTile :: Char
hardTile = '▒'

softTile :: Char
softTile = '='

block :: Char
block = '▓'

switch :: Char
switch = '±'

emptySpace :: Char
emptySpace = 'm'

winningTile :: Char
winningTile = '*'

{-
    Sinonim de tip de date pentru reprezetarea unei perechi (int, int)
    care va reține coordonatele de pe tabla jocului
-}

type Position = (Int, Int)
type Activate = [Position]

{-
    Direcțiile în care se poate mișcă blocul de pe tablă
-}

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    *** TODO ***

    Tip de date care va reprezenta plăcile care alcătuiesc harta și switch-urile
-}
data State = Won | Lost | Playing
	deriving (Show, Eq, Ord)
data Cell = Hard | Soft | Block | Switch | Empty | Winning
			deriving (Eq, Ord)

instance Show Cell where   
	show Hard = [hardTile]
	show Soft = [softTile]
	show Block = [block]
	show Switch = [switch]
	show Empty = [emptySpace]
	show Winning = [winningTile]
	

{-
    *** TODO ***

    Tip de date pentru reprezentarea nivelului curent
-}
data Level = Level {  matrix :: M.Matrix Cell
					, index :: Int
					, switches :: [(Position, [Position])]
					, up :: Bool
					, bl :: [(Cell, Position)]
					, matSize :: Position
					, win :: State
					, winPosition :: Position
					} deriving (Eq)

{-
    *** Opțional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară, 
    instantiati explicit clasele Eq și Ord pentru Level. 
    În cazul acesta, eliminați deriving (Eq, Ord) din Level. 
-}

-- instance Eq Level where
--     (==) = undefined

instance Ord Level where
	compare a b = compare (index a) (index b)

{-
    *** TODO ***

    Instantiati Level pe Show. 

    Atenție! String-ul returnat va fi urmat și precedat de un rând nou. 
    În cazul în care jocul este câștigat, la sfârșitul stringului se va mai
    concatena mesajul "Congrats! You won!\n". 
    În cazul în care jocul este pierdut, se va mai concatena "Game Over\n". 
-}

instance Show Level where
    show (Level m _ _ _ b _ w _)
		| w == Won = "\n" ++ filter (\x-> not (x `elem` "() ")) (show updatedMatrix) ++ "Congrats! You won!\n"
		| w == Lost = "\n" ++ filter (\x-> not (x `elem` "() ")) (show updatedMatrix) ++ "Game Over\n"
		| otherwise = "\n" ++ filter (\x-> not (x `elem` "() ")) (show updatedMatrix)
		where
			updatedMatrix
				| length b == 1 = M.setElem Block ((fst (snd (b!!0))), (snd (snd (b!!0)))) m
				| otherwise = M.setElem Block ((fst (snd (b!!1))), (snd (snd (b!!1)))) (M.setElem Block ((fst (snd (b!!0))), (snd (snd (b!!0)))) m)
{-
    *** TODO ***

    Primește coordonatele colțului din dreapta jos a hârtii și poziția inițială a blocului.
    Întoarce un obiect de tip Level gol.
    Implicit, colțul din stânga sus este (0, 0).
-}
updatePosition :: Position -> Position
updatePosition pos = ((fst pos) + 1, (snd pos) + 1)

emptyLevel :: Position -> Position -> Level
emptyLevel end b = Level (M.matrix ((fst end) + 1) ((snd end) + 1) (\(_, _) -> Empty)) 1 [] True [(Empty, (updatePosition b))] (((fst end) + 1), ((snd end) + 1)) Playing (0, 0)
{-
    *** TODO ***

    Adaugă o celulă de tip Tile în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        'H' pentru tile hard 
        'S' pentru tile soft 
        'W' pentru winning tile 
-}

addTile :: Char -> Position -> Level -> Level
addTile tile pos (Level m _ hash u b mS w wPosition)
	| tile == 'H' = Level (M.setElem Hard (updatePosition pos) m) 1 hash u (updateBlock Hard pos b) mS w wPosition
	| tile == 'S' = Level (M.setElem Soft (updatePosition pos) m) 1 hash u (updateBlock Soft pos b) mS w wPosition
	| otherwise = Level (M.setElem Winning (updatePosition pos) m) 1 hash u (updateBlock Winning pos b) mS w pos
	where
		updateBlock cell position blk = map (\(c, p) -> if (p == position) then (cell, p) else (c, p)) blk 

{-
    *** TODO ***

    Adaugă o celulă de tip Swtich în nivelul curent.
    Va primi poziția acestuia și o listă de Position
    ce vor desemna pozițiile în care vor apărea sau 
    dispărea Hard Cells în momentul activării/dezactivării
    switch-ului.
-}

addSwitch :: Position -> [Position] -> Level -> Level
addSwitch pos list (Level m _ s u b mS wgs wPosition) =  Level (updatedMatrix m pos) 1 ([((updatePosition pos), map updatePosition list)] ++ s) u (updateBlock Switch (updatePosition pos) b) mS wgs wPosition
	where
		updatedMatrix ma p = M.setElem Switch (updatePosition p) ma
		updateBlock cell position blk = map (\(c, p) -> if (p == position) then (cell, p) else (c, p)) blk 

{-
    === MOVEMENT ===
-}

{-
    *** TODO ***

    Activate va verifica dacă mutarea blocului va activa o mecanică specifică. 
    În funcție de mecanica activată, vor avea loc modificări pe hartă. 
-}

activate :: Cell -> Level -> Level
activate cell (Level m _ s u b mS w wPosition)
	| cell == Switch = activateSwitch
	| otherwise = (Level m 1 s u b mS w wPosition)
	where
		activateSwitch
			| M.getElem (fst (switchPositions!!0)) (snd (switchPositions!!0)) m == Hard = Level (foldl (\acc x -> M.setElem Empty x acc) m switchPositions) 1 s u b mS w wPosition
			| otherwise = Level (foldl (\acc x -> M.setElem Hard x acc) m switchPositions) 1 s u b mS w wPosition
		switchCell = snd ((filter (\(x,_)-> x == Switch) b)!!0)
		switchPositions = snd ((filter (\(x,_) -> x == switchCell) s)!!0)

{-
    *** TODO ***

    Mișcarea blocului în una din cele 4 direcții 
    Hint: Dacă jocul este deja câștigat sau pierdut, puteți lăsa nivelul neschimbat.
-}
checkCollumn :: Position -> Position -> Bool
checkCollumn p1 p2
	| (snd p1) == (snd p2) = True
	| otherwise = False

checkLine :: Position -> Position -> Bool
checkLine p1 p2 
	| (fst p1) == (fst p2) = True
	| otherwise = False
	

checkSoft :: [(Cell, Position)] -> [(Cell, Position)] -> Bool
checkSoft newPos b
	| length (filter (\(c, _)-> c == Soft) newPos) == 1 &&  (length b == 1) = True
	| otherwise = False

checkLimitOut :: [(Cell, Position)] -> Position -> Bool
checkLimitOut nPos mS 
	| length (filter (\(c,_)-> c == Empty) nPos) == 0 && length (filter (\(_, p)-> (fst p) < 0 || (fst p) > (fst mS) || (snd p) < 0 || (snd p) > (snd mS)) nPos) == 0 = False
	| otherwise = True

move :: Directions -> Level -> Level
move d (Level m _ s u b mS wg wPosition)
	| d == North && u == True = if (checkLimitOut (updateBlockNS (-2) (-1) b m) mS) 
								then Level m 1 s u (updateBlockNS (-2) (-1) b m) mS Lost wPosition
								else foldl (\acc c -> activate (fst c) acc) (Level m 1 s False (updateBlockNS (-2) (-1) b m) mS wg wPosition) (updateBlockNS (-2) (-1) b m)
	
	| d == South && u == True = if (checkLimitOut (updateBlockNS 1 2 b m) mS)
								then Level m 1 s u (updateBlockNS 1 2 b m) mS Lost wPosition
								else foldl (\acc c -> activate (fst c) acc) (Level m 1 s False (updateBlockNS 1 2 b m) mS wg wPosition) (updateBlockNS 1 2 b m)
	
	| d == West && u == True = if (checkLimitOut [(getELWE (-2) b m 0), (getELWE (-1) b m 0)] mS)
							   then Level m 1 s u [(getELWE (-2) b m 0), (getELWE (-1) b m 0)] mS Lost wPosition
							   else foldl (\acc c -> activate (fst c) acc) (Level m 1 s False [(getELWE (-2) b m 0), (getELWE (-1) b m 0)] mS wg wPosition) [(getELWE (-2) b m 0), (getELWE (-1) b m 0)]
	
	| d == East && u == True = if (checkLimitOut (updateBlockWE 1 2 b m) mS) 
							   then Level m 1 s u (updateBlockWE 1 2 b m) mS Lost wPosition
							   else foldl (\acc c -> activate (fst c) acc) (Level m 1 s False (updateBlockWE 1 2 b m) mS wg wPosition) (updateBlockWE 1 2 b m)
	
	| d == North && u == False && (checkCollumn (snd (b!!0)) (snd (b!!1))) == True = if (checkSoft [(head (updateBlockNS (-1) 0 b m))] [(head (updateBlockNS (-1) 0 b m))]) || (checkLimitOut [(head (updateBlockNS (-1) 0 b m))] mS)
																					 then Level m 1 s u [(head (updateBlockNS (-1) 0 b m))] mS Lost wPosition
																					 else if checkWin [(head (updateBlockNS (-1) 0 b m))]
																						   then Level m 1 s True [(head (updateBlockNS (-1) 0 b m))] mS Won wPosition
																						   else foldl (\acc c -> activate (fst c) acc) (Level m 1 s True [(head (updateBlockNS (-1) 0 b m))] mS wg wPosition) [(head (updateBlockNS (-1) 0 b m))]
	
	| d == North && u == False && (checkCollumn (snd (b!!0)) (snd (b!!1))) == False = if (checkLimitOut (updateBlockNS2 (-1) (-1) b m) mS)
																					  then Level m 1 s u (updateBlockNS2 (-1) (-1) b m) mS Lost wPosition
																					  else foldl (\acc c -> activate (fst c) acc) (Level m 1 s False (updateBlockNS2 (-1) (-1) b m) mS wg wPosition) (updateBlockNS2 (-1) (-1) b m)
	
	| d == South && u == False && (checkCollumn (snd (b!!0)) (snd (b!!1))) == True = if (checkSoft [(head (updateBlockNS 2 0 b m))] [(head (updateBlockNS 2 0 b m))]) || (checkLimitOut [(head (updateBlockNS 2 0 b m))] mS)
																					 then Level m 1 s u [(head (updateBlockNS 2 0 b m))] mS Lost wPosition
																					 else if checkWin [(head (updateBlockNS 2 0 b m))]
																						  then Level m 1 s True [(head (updateBlockNS 2 0 b m))] mS Won wPosition
																						  else foldl (\acc c -> activate (fst c) acc) (Level m 1 s True [(head (updateBlockNS 2 0 b m))] mS wg wPosition) [(head (updateBlockNS 2 0 b m))]
	
	| d == South && u == False && (checkCollumn (snd (b!!0)) (snd (b!!1))) == False = if (checkLimitOut (updateBlockNS2 1 1 b m) mS)
																					  then Level m 1 s u (updateBlockNS2 1 1 b m) mS Lost wPosition
																					  else foldl (\acc c -> activate (fst c) acc) (Level m 1 s False (updateBlockNS2 1 1 b m) mS wg wPosition) (updateBlockNS2 1 1 b m)
	
	| d == West && u == False && (checkCollumn (snd (b!!0)) (snd (b!!1))) == True = if (checkLimitOut (updateBlockWE2 (-1) (-1) b m) mS)
																					then Level m 1 s u (updateBlockWE2 (-1) (-1) b m) mS Lost wPosition
																					else foldl (\acc c -> activate (fst c) acc) (Level m 1 s False (updateBlockWE2 (-1) (-1) b m) mS wg wPosition) (updateBlockWE2 (-1) (-1) b m)

	| d == West && u == False && (checkCollumn (snd (b!!0)) (snd (b!!1))) == False = if (checkSoft [(head (updateBlockWE (-1) 0 b m))] [(head (updateBlockWE (-1) 0 b m))]) || (checkLimitOut [(head (updateBlockWE (-1) 0 b m))] mS)
																					 then Level m 1 s u [(head (updateBlockWE (-1) 0 b m))] mS Lost wPosition
																					 else if checkWin [(head (updateBlockWE (-1) 0 b m))] 
																						  then Level m 1 s True [(head (updateBlockWE (-1) 0 b m))] mS Won wPosition
																						  else foldl (\acc c -> activate (fst c) acc) (Level m 1 s True [(head (updateBlockWE (-1) 0 b m))] mS wg wPosition) [(head (updateBlockWE (-1) 0 b m))]

	| d == East && u == False && (checkCollumn (snd (b!!0)) (snd (b!!1))) == True = if (checkLimitOut (updateBlockWE2 1 1 b m) mS)
																					then Level m 1 s u (updateBlockWE2 1 1 b m) mS Lost wPosition
																					else foldl (\acc c -> activate (fst c) acc) (Level m 1 s False (updateBlockWE2 1 1 b m) mS wg wPosition) (updateBlockWE2 1 1 b m)

	| d == East && u == False && (checkCollumn (snd (b!!0)) (snd (b!!1))) == False = if (checkSoft [(head (updateBlockWE (2) 0 b m))] [(head (updateBlockWE (2) 0 b m))]) || (checkLimitOut [(head (updateBlockWE (2) 0 b m))] mS)
																					 then Level m 1 s u [(head (updateBlockWE (2) 0 b m))] mS Lost wPosition
																					 else if checkWin [(head (updateBlockWE (2) 0 b m))]
																						  then Level m 1 s False [(head (updateBlockWE (2) 0 b m))] mS Won wPosition
																						  else foldl (\acc c -> activate (fst c) acc) ( Level m 1 s True [(head (updateBlockWE (2) 0 b m))] mS wg wPosition) [(head (updateBlockWE (2) 0 b m))]
	
	where
		updateBlockNS pos1 pos2 bl1 matr = [(getELNS pos1 bl1 matr 0), (getELNS pos2 bl1 matr 0)]
		
		updateBlockNS2 pos1 pos2 bl2 matr = [(getELNS pos1 bl2 matr 0), (getELNS pos2 bl2 matr 1)]
		
		updateBlockWE pos1 pos2 bl3 matr = [(getELWE pos1 bl3 matr 0), (getELWE pos2 bl3 matr 0)]
		
		updateBlockWE2 pos1 pos2 bl4 matr = [(getELWE pos1 bl4 matr 0), (getELWE pos2 bl4 matr 1)]
		
		getELNS pos1 bl5 matr x = ((M.getElem (fst (snd (bl5!!x)) + pos1) (snd (snd (bl5!!x))) matr), ((fst (snd (bl5!!x)) + pos1), (snd (snd (bl5!!x)))))
			
		getELWE pos1 bl6 matr x = ((M.getElem (fst (snd (bl6!!x))) (snd (snd (bl6!!x)) + pos1) matr),((fst (snd (bl6!!x))), (snd (snd (bl6!!x)) + pos1))) 
		
		checkWin bl7 
			| (length bl7) == 1 && (fst (head bl7)) == Winning = True
			| otherwise = False
{-	
    *** TODO ***

    Va returna True dacă jocul nu este nici câștigat, nici pierdut.
    Este folosită în cadrul Interactive.
-}

continueGame :: Level -> Bool
continueGame (Level _ _ _ _ _ _ wg _)
	| wg == Playing = True
	| otherwise = False

{-
    *** TODO ***

    Instanțiați clasa `ProblemState` pentru jocul nostru. 
  
    Hint: Un level câștigat nu are succesori! 
    De asemenea, puteți ignora succesorii care 
    duc la pierderea unui level.
-}

instance ProblemState Level Directions where
    successors lvl
    	| isGoal lvl = []
    	| otherwise = filter (\(d,l)-> (win l) /= Lost) [(North, move North lvl),(South, move South lvl), (East, move East lvl),(West, move West lvl)]
	
    isGoal (Level _ _ _ _ _ _ wg _)
		| wg == Won = True
		| otherwise = False

    -- Doar petru BONUS
    heuristic lvl = (abs (fst (winPosition lvl) - (fst(snd (last (bl lvl)))) + 1)) + (abs (snd (winPosition lvl) - (snd ( snd  (last (bl lvl)))) + 1))

myLevel :: Level
myLevel = addSwitch (3,4) [(0,6), (1,6), (2,6)] $ addTile 'H' (3,2) $ addTile 'H' (4,2) $ addTile 'H' (7,2) $ addTile 'H' (6,2) $ addTile 'H' (5,2) $ addTile 'H' (3,3) $ addTile 'H' (4,3) $ addTile 'H' (7,3) $ addTile 'H' (6,3) $ addTile 'H' (5,3) $ addTile 'H' (3,5) $ addTile 'H' (4,5) $ addTile 'H' (7,5) $ addTile 'H' (6,5) $ addTile 'H' (5,5) $ addTile 'H' (4,4) $ addTile 'H' (5,4) $ addTile 'H' (6,4) $ addTile 'H' (7,4) $ addTile 'H' (1,2) $ addTile 'S' (1, 3) $ addTile 'W' (1, 4) $ emptyLevel (9,9) (8, 4)