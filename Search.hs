{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Maybe as I
import qualified Data.Function as Fct

{-
    *** DONE ***

    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:

    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime
    * copiii, ce vor desemna stările învecinate
-}

data Node s a = Node { state :: s
                     , action :: Maybe a
                     , parent :: Maybe (Node s a)
                     , depth :: Int
                     , kids :: [Node s a]
                     } deriving (Show)

{-
    *** DONE ***

    Întoarce starea stocată într-un nod.
-}

nodeState :: Node s a -> s
nodeState node = state node

{-
    *** DONE ***

    Generarea întregului spațiu al stărilor 
    Primește starea inițială și creează nodul corespunzător acestei stări, 
    având drept copii nodurile succesorilor stării curente.
-}

createStateSpace :: (ProblemState s a) => s -> Node s a

createStateSpace state = root
	where
		root = Node state Nothing Nothing 0 (map (\(a, s) -> (createStateSpaceHelper root 0 (a, s))) (successors state))

createStateSpaceHelper :: (ProblemState s a) => Node s a -> Int -> (a, s) -> Node s a
createStateSpaceHelper parentNode depth (a, s) = node
	where
		node = Node s (Just a) (Just parentNode) (depth + 1) (map children (successors s))
		children (newAct, newState) = createStateSpaceHelper node (depth + 1) (newAct, newState)


{-
    *** TODO PENTRU BONUS ***

    Ordonează întreg spațiul stărilor după euristica din ProblemState. 
    Puteți folosi `sortBy` din Data.List.
-}

orderStateSpace :: (ProblemState s a) => Node s a -> Node s a
orderStateSpace (Node stateNode _ _ _ _) = root
    where
        root = Node stateNode Nothing Nothing 0 (compareHelper (map (\(a, s) -> (orderStateSpaceHelper root 0 (a, s))) ((successors stateNode))))

orderStateSpaceHelper :: (ProblemState s a) => Node s a -> Int -> (a, s) -> Node s a
orderStateSpaceHelper parentNode depth (a, s) = node
    where
        node = Node s (Just a) (Just parentNode) (depth + 1) (compareHelper (map children (successors s)))
        children (newAct, newState) = orderStateSpaceHelper node (depth + 1) (newAct, newState)

compareHelper :: (ProblemState s a) => [Node s a] -> [Node s a] 
compareHelper list = L.sortBy (compare `Fct.on` (heuristic . state)) list


{-
    *** DONE ***

    Întoarce lista nodurilor rezultate prin parcurgerea limitată în adâncime
    a spațiului stărilor, pornind de la nodul dat ca parametru.

    Pentru reținerea stărilor vizitate, recomandăm Data.Set. Constrângerea
    `Ord s` permite utilizarea tipului `Set`.
-}

isIn :: Eq s => Node s a -> [Node s a] -> Bool
isIn node [] = False
isIn node list = (state node) == (state (head list)) || isIn node (tail list)

   
limitedDfs :: (ProblemState s a, Ord s)
           => Node s a    -- Nodul stării inițiale
           -> Int         -- Adâncimea maximă de explorare
           -> [Node s a]  -- Lista de noduri
limitedDfs current depthM = helperDfs [current] depthM []

helperDfs :: Eq s => [Node s a] -> Int -> [Node s a] -> [Node s a]
helperDfs [] d visited = reverse visited
helperDfs graph d visited
	| isIn (head graph) visited = helperDfs (tail graph) d visited 
	| depth (head graph) > d = helperDfs (tail graph) d visited 
	| depth (head graph) == d = helperDfs (tail graph) d ([head graph] ++ visited)
	| otherwise = helperDfs ((kids (head graph)) ++ (tail graph)) d ([head graph] ++ visited)




{-
    *** DONE ***

    Explorează în adâncime spațiul stărilor, utilizând adâncire iterativă,
    pentru determinarea primei stări finale întâlnite.

    Întoarce o perche între nodul cu prima stare finală întâlnită și numărul
    de stări nefinale vizitate până în acel moment.
-}

iterativeDeepening :: (ProblemState s a, Ord s)
    => Node s a         -- Nodul stării inițiale
    -> (Node s a, Int)  -- (Nod cu prima stare finală,
                        --  număr de stări nefinale vizitate)
                        
iterativeDeepening node = let  dep = length lists
                               restOfList = limitedDfs node dep
                               beforeWin = takeWhile (\x-> isGoal (nodeState x) == False) restOfList
                               nr = length beforeWin + (foldl (\acc x-> acc + (length x)) 0 lists)
                               newNode = restOfList !! (length beforeWin)
                           in (newNode, nr)
                               where lists = takeWhile ((== Nothing) . (L.findIndex (\x-> isGoal (nodeState x) == True))) [limitedDfs node x| x <- [0,1..]]
{-
    *** DONE ***
    
    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care se încheie în starea
    finală, dar care EXCLUDE starea inițială.
-}

extractPath ::(Eq a) => Node s a -> [(a, s)]
extractPath node = foldl (\acc (Node s a _ _ _) ->  [(I.fromJust a, s)] ++ acc) [] (takeWhile (\x-> (depth x) /= 0) (iterate (\x-> I.fromJust (parent x)) node))

{-
    *** DONE ***

    Pornind de la o stare inițială, se folosește de iterativeDeepening pentru 
    a găsi prima stare finală și reface calea către nodul inițial folosind 
    extractPath. 
  
    Întoarce o listă de perechi (acțiune, stare), care se încheie în starea
    finală, dar care EXCLUDE starea inițială.
-}

solve :: (ProblemState s a, Ord s, Eq a)
      => s          -- Starea inițială de la care se pornește 
      -> Bool       -- Dacă să folosească sau nu euristica dată 
      -> [(a, s)]   -- Lista perechilor
solve s b 
    | b = extractPath (fst (iterativeDeepening (orderStateSpace (createStateSpace s))))
    | otherwise = extractPath (fst (iterativeDeepening (createStateSpace s)))

{-
    Poate fi utilizată pentru afișarea fiecărui element al unei liste
    pe o linie separată.
-}

printSpacedList :: Show a => [a] -> IO ()
printSpacedList = mapM_ (\a -> print a >> putStrLn (replicate 20 '*'))