{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}


module Bloxorz where

import ProblemState

import qualified Data.Array as A

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
emptySpace = ' '

winningTile :: Char
winningTile = '*'

{-
    Sinonim de tip de date pentru reprezetarea unei perechi (int, int)
    care va reține coordonatele de pe tabla jocului
-}

type Position = (Int, Int)
{-
    Direcțiile în care se poate mișcă blocul de pe tablă
-}

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    Tip de date care va reprezenta plăcile care alcătuiesc harta și switch-urile
-}

data Cell = MyCell [Char]
    deriving (Eq, Ord)

instance Show Cell where
    show (MyCell str) = str

{-
    Tip de date care reprezinta pozitia Blocului pe harta (ocupa doua celule),
     daca pos1 == pos2, blocul este vertical, altfel este orizontal.
-}
type MyBlock = (Position, Position)

{-
    Tip de date pentru reprezentarea nivelului curent
-}


data Level = MyLevel MyBlock Position (A.Array Position Cell) [Position]
    deriving (Eq, Ord)

{-
    *** Opțional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară, 
    instantiati explicit clasele Eq și Ord pentru Level. 
    În cazul acesta, eliminați deriving (Eq, Ord) din Level. 
-}

-- instance Eq Level where
--     (==) = undefined

-- instance Ord Level where
--     compare = undefined

{-
    Instantiati Level pe Show. 

    Atenție! String-ul returnat va fi urmat și precedat de un rând nou. 
    În cazul în care jocul este câștigat, la sfârșitul stringului se va mai
    concatena mesajul "Congrats! You won!\n". 
    În cazul în care jocul este pierdut, se va mai concatena "Game Over\n". 
-}
{-
    assocs = se aplica unui array si intoarce
            lista elementelor in ordinea indexului dat.
    bounds = returneaza limitele cu care a fost 
             construit array
    foldl  = (fold left): returnează rezultatul aplicării 
            funcției f pe rând asupra unui element din 
            listă și a unui acumulator. (foldl f init L)
    A.assocs  => [(indice, valoare)]
    A.elems   => [valoare]
    A.indices => [indice] 
    @ => face pattern matching 
-}

instance Show Level where
    show lvl = "\n" ++ (foldl printMessage "" (A.assocs a)) ++ endMessage
        where
            MyLevel (pos1, pos2) wonPosition a _ = lvl
            (_, (_, c)) = A.bounds a
            endMessage
                -- blocul se afla in aer, nu este pe nicio celula
                | a A.!(pos1) == MyCell [emptySpace] && a A.!(pos2) == MyCell [emptySpace] = ""
                -- blocul a ajuns pe celula winningTile
                | pos1 == wonPosition && pos2 == wonPosition                               = "Congrats! You won!\n"
                -- daca s-a terminat jocul, adica a castigat sau a pierdut
                | continueGame lvl == False                                                = "Game Over\n"
                | otherwise                                                                = ""
            printMessage acumulator (wonPosition@(_, current_element), element) = acumulator ++ my_str1 ++ my_str2
                where
                    my_str1
                        | wonPosition == pos1|| wonPosition == pos2  = [block]
                        | otherwise                                  = show element
                    my_str2
                        | c == current_element                       = "\n"
                        | otherwise                                  = ""
{-
    Primește coordonatele colțului din dreapta jos a hârtii și poziția inițială a blocului.
    Întoarce un obiect de tip Level gol.
    Implicit, colțul din stânga sus este (0, 0).
-}
emptyLevel :: Position -> Position -> Level
emptyLevel (x, y) p = MyLevel (p, p) (-1, -1) (A.array ((0, 0), (x, y)) [((i, j), MyCell [emptySpace]) | i <- [0 .. x], j <- [0 .. y]]) []

{-
    Adaugă o celulă de tip Tile în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        'H' pentru tile hard 
        'S' pentru tile soft 
        'W' pentru winning tile 
-}
{-
    numeVariabila A.//[(pos, WinningTile)] = pune WinningTile la pos in numeVariabila,
                                             primeste ca parametru o lista, intoarce un nou array
    numeVariabila A.!pos                   = intoarce ce se afla la pozitia curenta, 
                                             adica celula asociata pozitie in coordonate
-}
{-
        Level are obiectele: MyBlock Position (A.Array Position Cell) [Switch]
-}
addTile :: Char -> Position -> Level -> Level
addTile tileType pos (MyLevel blockPosition wonPosition array switch_now)
        | tileType == 'H' = MyLevel blockPosition wonPosition (array A.//[(pos, MyCell [hardTile])]) switch_now
        | tileType == 'S' = MyLevel blockPosition wonPosition (array A.//[(pos, MyCell [softTile])]) switch_now
        -- retinem pozitia pe harta unde se afla winningTile
        | otherwise       = MyLevel blockPosition pos (array A.//[(pos, MyCell [winningTile])]) switch_now

{-
    Adaugă o celulă de tip Swtich în nivelul curent.
    Va primi poziția acestuia și o listă de Position
    ce vor desemna pozițiile în care vor apărea sau 
    dispărea Hard Cells în momentul activării/dezactivării
    switch-ului.
-}
{- 
    (:) -- operatorul de adăugare la începutul listei - cons
    switch_now = este o lista de pozitii pentru celulele care
                 vor aparea sau disparea de tip Hard Cells 
-}
addSwitch :: Position -> [Position] -> Level -> Level
addSwitch pos list_pos (MyLevel blockPosition wonPosition array switch_now) 
    = MyLevel blockPosition wonPosition (array A.//[(pos, MyCell [switch])]) (switch_now ++ list_pos)

{-
    === MOVEMENT ===
-}

{-

    Activate va verifica dacă mutarea blocului va activa o mecanică specifică. 
    În funcție de mecanica activată, vor avea loc modificări pe hartă. 
-}
{-
    Level are obiectele: MyBlock Position (A.Array Position Cell) [Switch]
-}
activate :: Cell -> Level -> Level
activate (MyCell str) (MyLevel blockPosition wonPosition array switch_now)
        -- dacă jocul este deja câștigat sau pierdut, lasam nivelul nemodificat
        | continueGame (MyLevel blockPosition wonPosition array switch_now) == False
                                        = MyLevel blockPosition wonPosition array switch_now
        -- daca blocul este pe o celula switch atunci schimbam celulele 
        | array A.!(fst blockPosition) == MyCell [switch] || array A.!(snd blockPosition) == MyCell [switch]
                                        = MyLevel blockPosition wonPosition array_new1 switch_now
        -- se afla pe celule hardTile, il lasam nemodificat
        | otherwise                     = MyLevel blockPosition wonPosition array switch_now
            where
                array_new1
                    -- schimbam cu hardTile, in functie de ce se afla pe prima pozitie din lista switch_now
                    | array A.!(head switch_now) == MyCell [emptySpace] = (array A.//[(pos1, MyCell [hardTile]) | pos1 <- switch_now])
                    -- schimbam cu softTile
                    | otherwise                                         = (array A.//[(pos1, MyCell [emptySpace]) | pos1 <- switch_now])

{-
    Mișcarea blocului în una din cele 4 direcții 
    Hint: Dacă jocul este deja câștigat sau pierdut, puteți lăsa nivelul neschimbat.
-}

move :: Directions -> Level -> Level
move str (MyLevel ((i, j), (k, l)) (won1, won2) array switch_now)
    -- daca blocul este orizontal, nord - sud
    | i == k && j /= l = activate (array A.!(k, l)) (MyLevel ((x, y), (z, w)) (won1, won2) array switch_now)
    -- daca blocul este orizontal, est - vest
    | i /= k && j == l = activate (array A.!(k, l)) (MyLevel ((a, b), (c, d)) (won1, won2) array switch_now)
    -- daca blocul este pe verticala, simbam pozitiile conform directiei
    | i == k && j == l = activate (array A.!(k, l)) (MyLevel ((e, f), (g, h)) (won1, won2) array switch_now)
    -- dacă jocul este deja câștigat sau pierdut, lasam nivelul nemodificat
    | otherwise        = activate (array A.!(k, l)) (MyLevel ((i, j), (k, l)) (won1, won2) array switch_now)
            where
                -- blocul este pe orizontal, nord - sud
                ((x, y), (z, w))
                        | str == East  = if j < l then ((i, j + 2), (k, l + 1)) else ((i, j + 1), (k, l + 2))
                        | str == South = ((i + 1, j), (k + 1, l))
                        | str == West  = if j < l then ((i, j - 1), (k, l - 2)) else  ((i, j - 2), (k, l - 1))
                        | otherwise    = ((i - 1, j), (k - 1, l))
                -- blocul este pe orizontal, est - vest
                ((a, b), (c, d))
                        | str == East  = ((i, j + 1), (k, l + 1))
                        | str == South = if j < l then ((i + 2, j), (k + 1, l)) else ((i + 1, j), (k + 2, l))
                        | str == West  = ((i, j - 1), (k, l - 1))
                        | otherwise    = if j < l then ((i - 1, j), (k - 2, l)) else ((i - 2, j), (k - 1, l))
                -- blocul este pe verticala
                ((e, f), (g, h))
                        | str == East  = ((i, j + 2), (k, l + 1))
                        | str == South = ((i + 2, j), (k + 1, l))
                        | str == West  = ((i, j - 2), (k, l - 1))
                        | otherwise    = ((i - 2, j), (k - 1, l))
{-
    Va returna True dacă jocul nu este nici câștigat, nici pierdut.
    Este folosită în cadrul Interactive.
-}

continueGame :: Level -> Bool
continueGame (MyLevel blockPosition wonPosition array switch_now) 
            -- daca blocul este vertical si este pe winningTile
            | fst blockPosition == snd blockPosition && fst blockPosition == wonPosition                     = False
            -- daca blocul este vertical si se afla pe softTile
            | fst blockPosition == snd blockPosition && array A.!(fst blockPosition) == MyCell [softTile]    = False
            -- daca blocul se afla vertical si este pe emptySpace
            | fst blockPosition == snd blockPosition &&  array A.!(fst blockPosition) == MyCell [emptySpace] = False
            -- daca blocul se afla orizontal si oricare pozitie se afla pe emptySpace
            | fst blockPosition /= snd blockPosition && (array A.!(fst blockPosition) 
                == MyCell [emptySpace] || array A.!(snd blockPosition) == MyCell [emptySpace])               = False
            | otherwise                                                                                      = True
{- 
     Functie care verifica daca dupa un move, adica un succesor este valid
     (daca succesorul este castigat il bagam in lista, altfel daca este pierdut, nu il consideram in lista)
-}

verifyWon :: Directions -> Level -> Bool
verifyWon d (MyLevel blockPosition wonPosition array switch_now) 
            -- daca blocul este vertical si este pe winningTile
            | fst blockPosition == snd blockPosition && fst blockPosition == wonPosition                     = True
            -- daca blocul este vertical si se afla pe softTile
            | fst blockPosition == snd blockPosition && array A.!(fst blockPosition) == MyCell [softTile]    = False
            -- daca blocul se afla vertical si este pe emptySpace
            | fst blockPosition == snd blockPosition &&  array A.!(fst blockPosition) == MyCell [emptySpace] = False
            -- daca blocul se afla orizontal si oricare pozitie se afla pe emptySpace
            | fst blockPosition /= snd blockPosition && (array A.!(fst blockPosition) 
                == MyCell [emptySpace] || array A.!(snd blockPosition) == MyCell [emptySpace])               = False
            | otherwise                                                                                      = True
{- 
     Functie care primeste o directie si un level si face move pe ele
-}          
f :: Directions -> Level -> Level
f directions (MyLevel blockPosition wonPosition array switch_now ) =
        (move directions (MyLevel blockPosition wonPosition array switch_now))
{-
    Instanțiați clasa `ProblemState` pentru jocul nostru. 
  
    Hint: Un level câștigat nu are succesori! 
    De asemenea, puteți ignora succesorii care 
    duc la pierderea unui level.
-}

instance ProblemState Level Directions where
    successors (MyLevel blockPosition wonPosition array switch_now) =
        -- alegem din lista doar succesorii valizi. Care sunt pierduti, ii scoatem din lista
        filter (\(dir, lvl) -> verifyWon dir lvl) list_map
           where 
              -- facem o lista de perechi (directie, level), cu toate cele 4 posibilitati de move
              list_map = map (\dir -> (dir, f dir (MyLevel blockPosition wonPosition array switch_now))) [North, South, East, West]

    isGoal (MyLevel blockPosition wonPosition array switch_now)
        -- daca level-ul este castigat, intoarcem True
        | fst blockPosition == snd blockPosition && fst blockPosition == wonPosition = True
        | otherwise                                                                  = False


    -- Doar petru BONUS
    -- heuristic = undefined
    