{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}

module Basics where
{-
    Expune funcțiile necesare reprezentării jocului.
-}

import ProblemState
import Data.List

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc.
    Colțul stânga-sus este (0, 0).
-}
type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea Target-urilor.
    Acestea conțin informații atât despre poziția curentă a
    Target-ului cât și despre comportamentul acestuia.
    Tipul Behavior este definit mai jos.
-}
data Target = Target {
    position :: Position,
    behavior :: Behavior
}

data EmptyCell = EmptyCell {
    position_empty :: Position
}
data Gateway = Gateway {
    init_poz :: Position,
    to_poz :: Position
}
data Obstacle = Obstacle {
    position_obstacle :: Position
}
data Hunter = Hunter {
    position_hunter :: Position
}
instance Eq EmptyCell where
    EmptyCell p1 == EmptyCell p2 = p1 == p2

instance Ord EmptyCell where
    EmptyCell p1 <= EmptyCell p2 = p1 <= p2

instance Eq Gateway where
    Gateway p1 p3 == Gateway p2 p4 = (p1 == p2 && p3 == p4)

instance Ord Gateway where
    Gateway p1 _ <= Gateway p2 _ = (p1 <= p2)  

instance Eq Obstacle where
    Obstacle p1 == Obstacle p2 = p1 == p2

instance Ord Obstacle where
    Obstacle p1 <= Obstacle p2 = p1 <= p2


instance Eq Hunter where
    Hunter p1 == Hunter p2 = p1 == p2

instance Ord Hunter where
    Hunter p1 <= Hunter p2 = p1 <= p2    




instance Eq Target where
    Target p1 _ == Target p2 _ = p1 == p2

instance Ord Target where
    Target p1 _ <= Target p2 _ = p1 <= p2

{-
    Tip de date pentru reprezentarea comportamentului unui Target.
    Tipul Behavior este utilizat pentru a modela tranziția Target-urilor
    din starea curentă în starea următoare. Primul parametru este poziția
    actuală a target-ului, iar al doilea, starea curentă a jocului.
    Tipul Game este definit mai jos.
    
    Observați că, din moment ce un Behavior produce un Target nou,
    acesta din urmă ar putea fi caracterizat de un alt Behavior
    decât cel anterior.
-}
type Behavior = Position -> Game -> Target

{-
    Direcțiile de deplasare pe tablă
-}
data Direction = North | South | West | East
    deriving (Eq, Show)

{-
    *** TODO ***
    
    Tip de date pentru reprezentarea stării jocului, la un anumit
    moment. Completați-l cu orice informație aveți nevoie pentru
    stocarea stării jocului (hunter, target, obstacole, gateways).
-}
data Game = Game Int Int Position [EmptyCell] [Obstacle] [Gateway] [Target] deriving (Eq, Ord)
{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Game.
    În cazul acesta, eliminați deriving (Eq, Ord) din Game.
-}



{-
    *** TODO ***

    Reprezentați starea jocului ca șir de caractere, pentru afișarea
    la consolă.
    
    Atenție! Fiecare linie, mai puțin ultima, este urmată de \n.
    Celule goale vor fi reprezentate ca ' '.
    Hunter-ul va fi reprezentat ca '!'.
    Target-urile vor fi reprezentate ca '*'
    Gateways-urile vor fi reprezentate ca '#'.
    Obstacolele vor fi reprezentate de '@'.

    Hint: S-ar putea să vă fie utile list comprehensions,
    precum și funcțiile elem, any și intercalate din Data.List.
-}

gameAsStringHelper :: Int -> Int -> Game -> String
gameAsStringHelper intx inty (Game linii coloane (x,y) listcell listobstacle listgateway listtarget )
    | (intx == (linii - 1) && inty == coloane) = "" 
    | (inty == coloane) = "\n" ++ (gameAsStringHelper (intx + 1) 0 (Game linii coloane (x,y) listcell listobstacle listgateway listtarget ))
    | (x,y) == (intx, inty) = "!" ++ (gameAsStringHelper intx (inty + 1) (Game linii coloane (x,y) listcell listobstacle listgateway listtarget ))
    | (filter (\x -> (intx,inty) == (position_obstacle x)) listobstacle) /= [] =  "@" ++ (gameAsStringHelper intx (inty + 1) (Game linii coloane (x,y) listcell listobstacle listgateway listtarget ))
    | (filter (\x -> (intx,inty) == (position x)) listtarget) /= [] =  "*" ++ (gameAsStringHelper intx (inty + 1) (Game linii coloane (x,y) listcell listobstacle listgateway listtarget ))
    | (filter (\x -> (intx,inty) == (init_poz x)) listgateway) /= [] =  "#" ++ (gameAsStringHelper intx (inty + 1) (Game linii coloane (x,y) listcell listobstacle listgateway listtarget ))
    | otherwise =  " " ++ (gameAsStringHelper intx (inty + 1) (Game linii coloane (x,y) listcell listobstacle listgateway listtarget ))

    



gameAsString :: Game -> String
gameAsString (Game linii coloane (x,y) listcell listobstacle listgateway listtarget ) = (gameAsStringHelper 0 0 (Game linii coloane (x,y) listcell listobstacle listgateway listtarget ))

instance Show Game where
    show = gameAsString

{-
    *** TODO ***
    
    Primește numărul de linii și numărul de coloane ale tablei de joc.
    Intoarce un obiect de tip Game în care tabla conține spații goale în interior, fiind
    împrejmuită de obstacole pe toate laturile. Implicit, colțul din stânga sus este (0,0),
    iar Hunterul se găsește pe poziția (1, 1).
-}
emptyGameHelper :: Int -> Int -> Game -> Game
emptyGameHelper intx inty (Game linii coloane (x,y) listcell listobstacle listgateway listtarget )
    | (intx == linii) =  (Game linii coloane (x,y) listcell listobstacle listgateway listtarget )
    | (inty == coloane) = emptyGameHelper (intx + 1) 0 (Game linii coloane (x,y) listcell listobstacle listgateway listtarget )
    | (intx == 0 || intx == (linii - 1)) = emptyGameHelper intx (inty + 1) (Game linii coloane (x,y) listcell (listobstacle ++ [Obstacle (intx, inty)]) listgateway listtarget )
    | (inty == 0 || inty == (coloane - 1)) = emptyGameHelper intx (inty + 1) (Game linii coloane (x,y) listcell (listobstacle ++ [Obstacle (intx, inty)]) listgateway listtarget )
    | otherwise = emptyGameHelper intx (inty + 1) (Game linii coloane (x,y) (listcell ++ [EmptyCell (intx,inty)]) listobstacle listgateway listtarget )




emptyGame :: Int -> Int -> Game
emptyGame linii coloane = (emptyGameHelper 0 0 (Game linii coloane (1,1) [] [] [] [] )) 
    
{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, cu Hunter-ul pus
    pe poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugat Hunter-ul
    Daca poziția este invalidă (ocupată sau în afara tablei de joc) se va întoarce
    același joc.
-}
addHunter :: Position -> Game -> Game
addHunter (z,y) (Game linii coloane (k,l) listcell listobstacle listgateway listtarget ) 
    | z < 0 || z > linii || y < 0 || y > coloane =  (Game linii coloane (k,l) listcell listobstacle listgateway listtarget )
    | (filter (\x -> (z,y) == (init_poz x)) listgateway) /= [] = (Game linii coloane (k,l) listcell listobstacle listgateway listtarget )
    | (filter (\x -> (z,y) == (position_obstacle x)) listobstacle) /= [] = (Game linii coloane (k,l) listcell listobstacle listgateway listtarget )
    | (filter (\x -> (z,y) == (position x)) listtarget) /= [] = (Game linii coloane (k,l) listcell listobstacle listgateway listtarget )
    | otherwise = (Game linii coloane (z,y) listcell listobstacle listgateway listtarget ) 
{-
    *** TODO ***

    Primește un comportament, o poziție și un joc și întoarce un nou joc, în care a fost
    adăugat Target-ul descris de comportament și poziție.
    Parametrul Behavior reprezintă comportamentul Hunter-ului care va fi adăugat.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat Target-ul.
-}
addTarget :: Behavior -> Position -> Game -> Game
addTarget behavior (x,y) (Game linii coloane (k,l) listcell listobstacle listgateway listtarget )
  = (Game linii coloane (k,l) (delete (EmptyCell (x,y)) listcell) listobstacle listgateway (listtarget ++ [Target (x,y) behavior]) )

{-
    *** TODO ***

    Primește o pereche de poziții și un joc și întoarce un nou joc, în care au fost adăugate
    cele două gateway-uri interconectate.
    Parametrul (Position, Position) reprezintă pozițiile de pe hartă la care vor fi adăugate 
    cele două gateway-uri interconectate printr-un canal bidirecțional.
-}
addGateway :: (Position, Position) -> Game -> Game
addGateway ((x1,y1),(x2,y2)) (Game linii coloane (k,l) listcell listobstacle listgateway listtarget ) 
    = (Game linii coloane (k,l) newlistcell listobstacle newlistgateway listtarget ) where
        newlistcell = (delete (EmptyCell (x2,y2)) (delete (EmptyCell (x1,y1)) listcell))
        newlistgateway = listgateway ++ [Gateway (x1,y1) (x2,y2)] ++ [Gateway (x2,y2) (x1,y1)]
{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, în care a fost adăugat un obstacol
    la poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat obstacolul.
-}
addObstacle :: Position -> Game -> Game
addObstacle (x,y) (Game linii coloane (k,l) listcell listobstacle listgateway listtarget )
    = (Game linii coloane (k,l) (delete (EmptyCell (x,y)) listcell) newlistobstacle listgateway listtarget ) where
        newlistobstacle = listobstacle ++ [Obstacle (x,y)]

{-
    *** TODO ***
    
    Primește o poziție destinație înspre care vrea să se deplaseze o entitate (Hunter sau Target)
    și verifică daca deplasarea este posibilă, întorcând noua poziție, luând în considerare
    și Gateway-urile.
    Avem următoarele cazuri:
    - dacă poziția corespunde unui spațiu gol, se întoarce acea poziție;
    - dacă poziția corespunde unui gateway, se întoarce poziția gateway-ului pereche;
    - dacă poziția corespunde unui obstacol, se întoarce Nothing.
    Parametrul Position reprezintă poziția destinație.
-}
attemptMove :: Position -> Game -> Maybe Position
attemptMove (x,y) (Game linii coloane (k,l) listcell listobstacle listgateway listtarget )
    | (filter (\t -> (x,y) == (init_poz t)) listgateway) /= [] =  Just $ to_poz $ head $ filter (\t -> (x,y) == (init_poz t)) listgateway
    | (filter (\t -> (x,y) == (position_obstacle t)) listobstacle) /= [] = Nothing
    | otherwise = Just (x,y)

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre est. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
    
    Conform definiției, tipul Behavior corespunde tipului funcție
    Position -> Game -> Target.
    
    Având în vedere că cele patru funcții definite în continuare (goEast, goWest,
    goNorth, goSouth) sunt foarte similare, încercați să implementați o funcție
    mai generală, pe baza căreia să le definiți apoi pe acestea patru.
-}
secondHelper :: Maybe Position -> Position
secondHelper (Just x) = x
secondHelper c = (-1,-1)


goDirection :: Position -> Behavior -> Position -> Game -> Target
goDirection (k,l) behavior (x,y) (Game linii coloane (t,q) listcell listobstacle listgateway listtarget )
    | y + l >= coloane  || y + l < 0 =  (Target (x,y) behavior)
    | x + k >= linii || x + k < 0 = (Target (x,y) behavior)
    | (attemptMove (x + k, y + l)  (Game linii coloane (t,q) listcell listobstacle listgateway listtarget )) == Nothing 
    = if (filter (\t -> (x,y) == (init_poz t)) listgateway /= []) then (Target (to_poz $ head $ filter (\t -> (x,y) == (init_poz t)) listgateway) behavior) else (Target (x,y) behavior)
    | otherwise = (Target (secondHelper (attemptMove (x + k, y + l) (Game linii coloane (t,q) listcell listobstacle listgateway listtarget ))) behavior)

goEast :: Behavior
goEast (x,y) (Game linii coloane (k,l) listcell listobstacle listgateway listtarget )
     = goDirection (0,1) goEast (x,y) (Game linii coloane (k,l) listcell listobstacle listgateway listtarget )

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre vest. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goWest :: Behavior
goWest (x,y) (Game linii coloane (k,l) listcell listobstacle listgateway listtarget ) 
    =  goDirection (0,-1) goWest (x,y) (Game linii coloane (k,l) listcell listobstacle listgateway listtarget )

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre nord. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goNorth :: Behavior
goNorth (x,y) (Game linii coloane (k,l) listcell listobstacle listgateway listtarget ) 
    =  goDirection (-1,0) goNorth (x,y) (Game linii coloane (k,l) listcell listobstacle listgateway listtarget )


{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre sud. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goSouth :: Behavior
goSouth (x,y) (Game linii coloane (k,l) listcell listobstacle listgateway listtarget ) 
    =  goDirection (1,0) goSouth (x,y) (Game linii coloane (k,l) listcell listobstacle listgateway listtarget )


{-
    *** TODO ***

    Comportamentul unui Target de a-și oscila mișcarea, când înspre nord, când înspre sud. 
    Mișcarea se poate face doar dacă poziția este validă (se află pe tablă de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul iși va schimba
    direcția de mers astfel:
    - daca mergea inspre nord, își va modifica direcția miscării înspre sud;
    - daca mergea inspre sud, își va continua mișcarea înspre nord.
    Daca Target-ul întâlneste un Gateway pe traseul său, va trece prin acesta,
    către Gateway-ul pereche conectat și își va continua mișcarea în același sens la ieșire
    din acesta.
    Puteți folosit parametrul Int pentru a surprinde deplasamentul Target-ului (de exemplu,
    1 pentru sud, -1 pentru nord).
-}
bounce :: Int -> Behavior
bounce intx (x,y) (Game linii coloane (k,l) listcell listobstacle listgateway listtarget ) 
    | (filter (\t -> (x + intx,y) == (init_poz t)) listgateway) /= [] = (Target  (to_poz $ head $ filter (\t -> (x + intx,y) == (init_poz t)) listgateway) (bounce intx))
    |  (attemptMove (x + intx, y) (Game linii coloane (k,l) listcell listobstacle listgateway listtarget )) /= Nothing = (Target (x + intx,y) (bounce intx))
    |   otherwise = (Target (x - intx,y) (bounce (-intx)))
{-
    *** TODO ***
    Funcție care mută toate Target-urile din Game-ul dat o poziție, în functie
    de behavior-ul fiecăreia și întoarce noul Game în care pozițiile Target-urilor
    sunt actualizate.
    
-}
changeTargets :: Game -> [Target] -> [Target]
changeTargets game list = foldl (\acc x -> acc ++ [((behavior x) (position x) game)]) [] list 

emptyTargets :: [Target] -> [EmptyCell]
emptyTargets list = foldl (\acc x -> acc ++ [EmptyCell (position x)]) [] list

moveTargets :: Game -> Game
moveTargets (Game linii coloane (k,l) listcell listobstacle listgateway listtarget )  = (Game linii coloane (k,l) (listcell ++ (emptyTargets listtarget)) listobstacle listgateway (changeTargets game listtarget) ) where
    game = (Game linii coloane (k,l) listcell listobstacle listgateway listtarget ) 

{-
    *** TODO ***

    Verifică dacă Targetul va fi eliminat de Hunter.
    Un Target este eliminat de Hunter daca se află pe o poziție adiacentă
    cu acesta.
    Parametrul Position reprezintă poziția Hunterului pe tabla
    de joc.
    Parametrul Target reprezintă Targetul pentru care se face verificarea.
-}
isTargetKilled :: Position -> Target -> Bool
isTargetKilled (x,y) (Target (k,l) behavior)
    | (x + 1, y) == (k, l) = True
    | (x - 1, y) == (k, l) = True
    | (x, y - 1) == (k, l) = True
    | (x, y + 1) == (k, l) = True
    | otherwise = False


{-
    *** TODO ***

    Avansează starea jocului curent, rezultând starea următoare a jocului.
    Parametrul Direction reprezintă direcția în care se va deplasa Hunter-ul.
    Parametrul Bool specifică dacă, după mutarea Hunter-ului, vor fi
    mutate și Target-urile sau nu, și dacă vor fi eliminate din joc sau nu.
    Este folosit pentru a distinge între desfășurarea reală a jocului (True)
    și planificarea „imaginată” de hunter (False) în partea a doua a temei.

    Avansarea stării jocului respectă următoarea ordine:
    1. Se deplasează Hunter-ul.
    2. În funcție de parametrul Bool, se elimină Target-urile omorâte de către Hunter.
    3. In funcție de parametrul Bool, se deplasează Target-urile rămase pe tablă.
    4. Se elimină Targeturile omorâte de către Hunter și după deplasarea acestora.
    
    Dubla verificare a anihilării Target-urilor, în pașii 2 și 4, îi oferă Hunter-ului
    un avantaj în prinderea lor.
-}
clearTargets :: Game -> Game
clearTargets  (Game linii coloane (k,l) listcell listobstacle listgateway listtarget ) 
    = (Game linii coloane (k,l) listcell listobstacle listgateway  (filter (\t -> (isTargetKilled (k,l) t) == False) listtarget) )



advanceGameStateHelper :: Bool -> Game -> Game
advanceGameStateHelper flag root@(Game linii coloane (k,l) listcell listobstacle listgateway listtarget ) = case flag of
    True ->  clearTargets $ moveTargets $ clearTargets root
    False -> root


advanceGameState :: Direction -> Bool -> Game -> Game
advanceGameState direction flag root@(Game linii coloane (k,l) listcell listobstacle listgateway listtarget ) 
    | direction == North = if((attemptMove (k - 1, l) root) /= Nothing) 
        then advanceGameStateHelper flag (Game linii coloane (secondHelper (attemptMove (k - 1, l) root)) listcell listobstacle listgateway listtarget ) 
        else advanceGameStateHelper flag root
    | direction == South = if((attemptMove (k + 1, l) root) /= Nothing) 
        then advanceGameStateHelper flag (Game linii coloane (secondHelper (attemptMove (k + 1, l) root)) listcell listobstacle listgateway listtarget ) 
        else advanceGameStateHelper flag root
    | direction == East = if((attemptMove (k, l + 1) root) /= Nothing) 
        then advanceGameStateHelper flag (Game linii coloane (secondHelper (attemptMove (k, l + 1) root)) listcell listobstacle listgateway listtarget ) 
        else advanceGameStateHelper flag root
    | direction == West = if((attemptMove (k, l - 1) root) /= Nothing) 
        then advanceGameStateHelper flag (Game linii coloane (secondHelper (attemptMove (k, l - 1) root)) listcell listobstacle listgateway listtarget ) 
        else advanceGameStateHelper flag root
    | otherwise = root   
 


   
{-
    ***  TODO ***

    Verifică dacă mai există Target-uri pe table de joc.
-}
areTargetsLeft :: Game -> Bool
areTargetsLeft (Game linii coloane (k,l) listcell listobstacle listgateway listtarget ) = if (listtarget /= []) then True else False

{-
    *** BONUS TODO ***

    Comportamentul unui Target de a se deplasa în cerc, în jurul unui Position, având
    o rază fixată.
    Primul parametru, Position, reprezintă centrul cercului.
    Parametrul Int reprezintă raza cercului.
    Puteți testa utilizând terenul circle.txt din directorul terrains, în conjuncție
    cu funcția interactive.
-}
circle :: Position -> Int -> Behavior
circle = undefined


instance ProblemState Game Direction where
    {-
        *** TODO ***
        
        Generează succesorii stării curente a jocului.
        Utilizați advanceGameState, cu parametrul Bool ales corespunzător.
    -}
    successors game = [(North, advanceGameState North False game), (South, advanceGameState South False game), (East, advanceGameState East False game), (West, advanceGameState West False game)]

    {-
        *** TODO ***
        
        Verifică dacă starea curentă este un în care Hunter-ul poate anihila
        un Target. Puteți alege Target-ul cum doriți, în prezența mai multora.
    -}
    isGoal  (Game linii coloane (k,l) listcell listobstacle listgateway listtarget ) = if  (filter (\t -> (isTargetKilled (k,l) t) == True) listtarget) /= [] then True else False

    {-
        *** TODO ***
        
        Euristica euclidiană (vezi hEuclidian mai jos) până la Target-ul ales
        de isGoal.
    -}
    h (Game linii coloane (k,l) listcell listobstacle listgateway listtarget ) 
        = if filter (\t -> (isTargetKilled (k,l) t) == True) listtarget /= [] 
    then hEuclidean  (position $ head  $ filter (\t -> (isTargetKilled (k,l) t) == True) listtarget) (k,l)
    else hEuclidean (k,l) (position $ head listtarget)

{-
     ** NU MODIFICATI **
-}
hEuclidean :: Position -> Position -> Float
hEuclidean (x1, y1) (x2, y2) = fromIntegral $ ((x1 - x2) ^ pow) + ((y1 - y2) ^ pow)
  where
    pow = 2 :: Int

{-
    *** BONUS ***

    Acesta reprezintă un artificiu necesar pentru testarea bonusului,
    deoarece nu pot exista două instanțe diferite ale aceleiași clase
    pentru același tip.

    OBSERVAȚIE: Testarea bonusului pentru Seach este făcută separat.
-}

newtype BonusGame = BonusGame Game
    deriving (Eq, Ord, Show)

{-
    *** BONUS TODO ***

    Folosind wrapper-ul peste tipul Game de mai sus instanțiați
    ProblemState astfel încât să fie folosită noua euristică. 
-}
instance ProblemState BonusGame Direction where
    {-
        *** BONUS TODO ***

        Pentru a ne asigura că toțî succesorii unei stări sunt de tipul
        BonusGame și folosesc noua euristică trebuie să aplicăm wrapper-ul
        definit mai sus peste toți succesorii unei stări.

        Hint: Puteți să folosiți funcția fmap pe perechi pentru acest lucru.
        https://wiki.haskell.org/Functor
    -}
    successors = undefined

    {-
        *** BONUS TODO ***

        Definiți funcția isGoal pentru BonusGame.

        Hint: Folosiți funcția isGoal deja implementată pentru tipul Game.
    -}
    isGoal = undefined

    {-
        *** BONUS TODO ***

        Definiți o funcție euristică care este capabilă să găsească un drum mai scurt
        comparativ cu cel găsit de euristica implementată pentru Game.

        ATENȚIE: Noua euristică NU trebuie să fie una trivială.

        OBSERVAȚIE: Pentru testare se va folosi fișierul terrains/game-6.txt.
    -}
    h = undefined
