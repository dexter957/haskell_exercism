------- Exercism in haskell. Github ----------
---There are several versions for each function, to try as much as possible

--------------- ** Imports ** ---------------
import qualified Data.List as List
import qualified Data.Char as Character
import qualified Data.Map as Map
import qualified Data.List.Split as Split
import qualified Data.Set as Set
import qualified System.Random as Random
import qualified Control.Monad as CtlMon
import qualified Control.Concurrent.MVar as CMV
---------------------------------------------------

--------------------------- Defs ----------------------------
punctuation = ['.',' ',':','!','?','(' , ')',',']
-------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------------------------------------------------

----------------------- Accumulate  -------------------------
----- 1st solution without help from standard ----
accumulate :: (a -> b) -> [a] -> [b]
accumulate f [] = []
accumulate f (x:xs) = [f x] ++ accumulate f xs

----
accumulate2 :: (a -> b) -> [a] -> [b]
accumulate2 f (x:[]) = [f x]
accumulate2 f (x:xs) = f x : accumulate2 f xs

----
----- 2nd solution with the help of standards ----
accumulate1 :: (a -> b) -> [a] -> [b]
accumulate1 f xs = map f xs


----
----- 3rd solution after a while playing with haskell
myMap1 :: [a] -> (a->b) -> [b]
myMap1 l f = [f x | x<-l]

---------------------------------------------------------------

-------------------------  Acronym  ----------------------------
---- Gets a string made up of random words, and returns the first letter of each. They must be separated by a whitespace -----
acronym :: String -> String
acronym toAcronymize = map head (words toAcronymize)
----
acronym' :: String -> String
acronym' toAcronymize = map head $words toAcronymize

----------------------------------------------------------------

---------------------- Difference Of Squares -----------------------

---- Square of the sum

sqSum :: Int -> String
sqSum 0 = "Not defined"
sqSum n = show $ (^2) (sum(take n [1..]))

---- Sum of the Squares
sumSq :: Int -> String
sumSq 0 = "Not defined"
sumSq n = show.sum.map (^2) $take n [1..]

--- Difference
dsumsq :: Int -> String
dsumsq n
          |(n<=0) = "Not defined"
          |otherwise = show (read (sqSum n) - read (sumSq n) + 0 )
---------------------------------------------------------------------

---------------------  Dominoes  ----------------------------------
----  [2|3] [3|1] [1|2]  <-- Correct

data Domino = Domino Int Int deriving (Show)

--- Domino functionalities: equality and reversion

eqDoms :: Domino -> Domino -> Bool
eqDoms (Domino x y) (Domino f l) = ((x==f)&&(y==l))

revDom :: Domino -> Domino
revDom (Domino f l) = Domino l f


twoDoms :: [Domino] -> [[Domino]]
twoDoms []=[[]]
twoDoms l = [f |f <- List.subsequences l, length f ==2]

-----------------------------------------------------------------------

---------- crypto-square ---------------------------

cryptoSquare :: String -> String
cryptoSquare str = map Character.toLower [l|l<-str,not (l `elem` punctuation)]


frmMsg :: [String] -> [String]
frmMsg [] = []
frmMsg l
          |(length (head l) > 1 ) = [map head l] ++ frmMsg (map tail l)
          |otherwise = [map head l]


---- What is left from this exercise is how to calculate c and r, that I really don't get
-------------------------------------------------------------------------------------------------

-------------------------------------------- atbash cipher  -------------------------------------------------------

--- Map.lookup returns Maybe, not Integer
getNum :: Maybe Integer -> Integer
getNum Nothing = 0
getNum (Just n) = n

getFTuple :: (a,b) -> a
getFTuple (f,s) = f

atbash = Map.fromList([('a',1),('b',2),('c',3),('d',4),('e',5),('f',6),('g',7),('h',8),('i',9),('j',10),('k',11),('l',12),('m',13),('n',14),('o',15),('p',16),('q',17),('r',18),('s',19),('t',20),('u',21),('v',22),('w',23),('x',24),('y',25),('z',26)])

encodeAtbash :: String -> String
encodeAtbash [] = []
encodeAtbash (c:cs) = [getFTuple(head (Map.toList( Map.filterWithKey (\k v -> v+(getNum (Map.lookup c atbash))==27) atbash)))] ++ encodeAtbash cs

encode :: String -> [String]
encode [] = []
encode str = ciphertext $encodeAtbash (List.map Character.toLower plaintext)
            where plaintext = [pln|pln<-str, not (pln `elem` punctuation)]
                  ciphertext = Split.chunksOf 5

----------------------------------------------------------------------------------------------------------------------------

----------------------------------------------------  isbn verifier ---------------------------------------------------------

parseISBN :: String -> [String]
parseISBN isbn = let splitted = Split.splitOn "-" isbn
                  in if length splitted > 1 then dashParser splitted
                    else [spl|spl<-List.subsequences (splitted !! 0),length spl == 1]

dashParser :: [String] -> [String]
dashParser [] = []
dashParser (d:ds) = [x|x<-List.subsequences d,length x == 1] ++ dashParser ds

verifyISBN :: String -> Bool
verifyISBN isbn = (calcISBN (parseISBN isbn) 10 0 ) `mod` 11 == 0


calcISBN :: [String] -> Int -> Int -> Int
calcISBN [] a s = s
calcISBN ("X":[]) a s = calcISBN [] (a-1) (s+(10*a))
calcISBN (x:xs) a s = calcISBN xs (a-1) (s+((read x)*a))

------------------------------------------------------------------------------------------------------------------------------
------RNA  Transcription---------------------------------
---classes and types are a bit too much for this; they are used purely for practise
dtor = Map.fromList([('A','U'),('T','A'),('G','C'),('C','G')])

type Cleotide = Char
data Dna = Nuc [Cleotide]   deriving Show
data Rna = Ribo [Cleotide] deriving Show

nucs = ['A','T','G','C']
ribos = ['A','T','G','U']

getCleotide :: Maybe Cleotide -> Cleotide
getCleotide Nothing = ' '
getCleotide (Just e) = e

ll :: Cleotide -> Cleotide
ll c = getCleotide (Map.lookup c dtor)

ll1 :: Dna -> [Cleotide]
ll1 (Nuc [])= []
ll1 (Nuc (c:cs)) = [ll c] ++ ll1 (Nuc cs)

transcription :: Dna -> Rna
transcription (Nuc cl) = Ribo (ll1 (dna cl))

dna :: [Cleotide] -> Dna
dna cl
        |allNucs cl = Nuc cl
        |otherwise = error "Error not nucleotides!"

rna :: [Cleotide] -> Rna
rna rb
        |allRibos rb = Ribo rb
        |otherwise = error "Error not ribocleotides!"


allNucs :: [Cleotide] -> Bool
allNucs [] = False
allNucs (c:[]) = isNuc c
allNucs (c:cs)
                | isNuc c == True = allNucs cs
                |otherwise = False

allRibos :: [Cleotide] -> Bool
allRibos [] = False
allRibos (r:[]) = isRibo r
allRibos (r:rs)
                | isRibo r == True = allRibos rs
                |otherwise = False

isNuc :: Cleotide -> Bool
isNuc c = c `elem` nucs

isRibo :: Cleotide -> Bool
isRibo r = r `elem` ribos

-----------------------------------------------------------------------------------------------------------------------------
------- Double Linked List  ----------------------------------------

---A double linked list is either empty, or it has node that points to its previous node (maybe empty), and the next one (also maybe empty)

data DbList a = EmptyDbl | Node a (DbList a) (DbList a) deriving Show

singleton :: a -> DbList a -> DbList a
singleton x previous = Node x (previous) (EmptyDbl)

isEmpty :: DbList a -> Bool
isEmpty EmptyDbl = True
isEmpty _ = False

insertInDbl :: DbList a -> a -> DbList a
insertInDbl EmptyDbl x = singleton x EmptyDbl
insertInDbl (Node v previous next) x
                                      |isEmpty next = Node v previous (singleton x (Node v previous next) )
                                      |otherwise = Node v previous (insertInDbl next x)

-----------------------------------------------------------------------------------------------------------------------------
---- A binary search tree

data Tree a = EmptyTree | TreeNode a (Tree a) (Tree a) deriving Show

singletonTree :: a -> Tree a
singletonTree x  = TreeNode x EmptyTree EmptyTree

insertInTree ::(Ord a)=> a -> Tree a -> Tree a
insertInTree x EmptyTree = singletonTree x
insertInTree x (TreeNode v left right)
                                  |(x==v) = TreeNode v left right
                                  |(x<v) = TreeNode v (insertInTree x left) right
                                  |(x>v) = TreeNode v left (insertInTree x right)

treeElem :: (Ord a)=> a -> Tree a -> Bool
treeElem x (TreeNode v EmptyTree EmptyTree) = x==v
treeElem x (TreeNode v left right)
                                  |(x>v) = treeElem x right
                                  |(x<v) = treeElem x left
                                  |otherwise = True

--- Let's implement a toList function as well

toList :: Tree a -> [a]
toList EmptyTree = []
toList (TreeNode v left right) = [v] ++ toList left ++ toList right


------------------------------------------------------------------------------------------------------------------------------

--------------------------------------------------------  Bracket Push --------------------------------------------------------

bracketPush :: String -> Bool
bracketPush expr = if opnclsMatches allBrackets then propOrd allBrackets else False
              where allBrackets = gatherBrackets expr

bracketPush' :: String -> Bool
bracketPush' expr = if opnclsMatches allBrackets then propOrd allBrackets else False
              where allBrackets = gatherBrackets' expr

gatherBrackets :: String -> String
gatherBrackets [] = []
gatherBrackets ('(':cr) = ['('] ++ gatherBrackets cr
gatherBrackets (')':cr) = [')'] ++ gatherBrackets cr
gatherBrackets ('[':cr) = ['['] ++ gatherBrackets cr
gatherBrackets (']':cr) = [']'] ++ gatherBrackets cr
gatherBrackets ('{':cr) = ['{'] ++ gatherBrackets cr
gatherBrackets ('}':cr) = ['}'] ++ gatherBrackets cr
gatherBrackets (c:cr) = gatherBrackets cr

gatherBrackets' :: String -> String
gatherBrackets' s  = [c|c<-s,c `elem` "[](){}"]


---- Check that every opening '[', '{', or '(' has a matching closing ']','}', or ')' respectively

opnclsMatches :: String -> Bool
opnclsMatches str =
                        let twos = filter (\l-> length l == 2) (List.subsequences str)
                            br = "[]"
                            cbr = "{}"
                            par = "()"
                        in  ((br `elem` twos) || (br `bothNotIn` str)) && ((cbr `elem` twos) || (cbr `bothNotIn` str)) && ((par `elem` twos) || (par `bothNotIn` str))

bothNotIn :: String -> String -> Bool
bothNotIn [] brackets = True
bothNotIn (c:cs) brackets = if ((c `elem` brackets) == True) then False else bothNotIn cs brackets

----- Check proper order of brackets; first (), then [] and {} afterwards

propOrd :: String -> Bool
propOrd [] = True
propOrd ('}':[]) = propOrd []
propOrd (')':[]) = propOrd []
propOrd (']':[]) = propOrd []
propOrd (')':']':cs) = propOrd (']':cs)
propOrd (']':'}':cs) = propOrd ('}':cs)
propOrd ('}':'}':cs) = propOrd ('}':cs)
propOrd ('(':cs) = propOrd cs
propOrd ('[':'(':cs) = propOrd cs
propOrd ('{':'[':cs) = propOrd ('[':cs)
propOrd ('{':'{':cs) = propOrd ('{':cs)
propOrd (c:cs) = False


---------------------------------------------------------------------------------------------------------------------------------
-------   Clock  -------

type Seconds = Int
type Minutes = Int
type Hours = Int

data Clock = Clock { hour :: Hours, minutes :: Minutes, seconds :: Seconds }  deriving (Show, Eq)

--- Smart constructor, to impose constraints on hours', minutes' and seconds' values
clockConstruct :: (Hours, Minutes,Seconds) -> Clock
clockConstruct (h,m,s)
                        |((h<=24)&&(h>=0))&&((m<=60)&&(m>=0))&&((s<=60)&&(s>=0)) = Clock {hour = h, minutes = m, seconds = s}
                        |otherwise = error "Invalid time values"

clockPrint :: Clock -> String
clockPrint (Clock {hour = h, minutes = m, seconds = s }) = show h ++ ":" ++ show m ++ ":" ++ show s

clockSadd :: Clock -> Clock -> Clock
clockSadd (Clock {hour = h1, minutes = m1, seconds = s1 }) (Clock {hour = h2, minutes = m2, seconds = s2 }) =
              let (m3, s3) = addS [s1, s2]
                  (h3, m4) = addM [m1, m2, m3]
                  (_, h4) = addH [h1, h2, h3]
                    in clockConstruct (h4,m4,s3)

--- Since Clock is instance of the Eq class, we do not need this function
clockSequal :: Clock -> Clock -> Bool
clockSequal (Clock {hour = h1, minutes = m1, seconds = s1 }) (Clock {hour = h2, minutes = m2, seconds = s2 }) = (h1==h2)&&(m1==m2)&&(s1==s2)

toString :: Clock -> String
toString = clockPrint

--- Add duration
addDelta :: Clock -> (Hours, Minutes) -> Clock
addDelta (Clock {hour = h1, minutes = m1, seconds = s1 }) (h2,m2) =
        let (h3,m3) = addM [m1, m2]
            (_, h4) = addH [h1, h2, h3]
              in clockConstruct (h4,m3,s1)



addS :: [Seconds] -> (Minutes, Seconds)
addS secs =
            let su = sum secs
                      in (div su 60, rem su 60)

addM :: [Minutes] -> (Hours, Minutes)
addM mins =
            let mu = sum mins
                      in (div mu 60, rem mu 60)

addH :: [Hours] -> (Hours, Hours)
addH hous =
            let hu = sum hous
                in (0, rem hu 24)

-----------------------------------------------------------------------------------------------------------------------------------------
---- Leap -----

isLeapYear :: Int -> Bool
isLeapYear y
              | (y `mod` 4 == 0) && (y `mod` 100 == 0) = (y `mod` 400 == 0)
              | (y `mod` 4 == 0) && (y `mod` 100 /= 0) =  True
              | otherwise = False

-------------------------------------------------------------------------------------------------------------------------------------------
-----   Run Length Encoding   -----

                        ----  Encoder  -----

encodeRL :: String -> String
encodeRL rl = encodeRL' 0 rl

encodeRL' :: Int -> String -> String
encodeRL' acc (c:x:[])
                      | (c==x) = show (acc+2) ++ [c]
                      | otherwise = show (acc+1) ++ [c] ++ "1" ++ [x]
encodeRL' acc (c:cs)
                      | (c == head cs ) = encodeRL' (acc+1) cs
                      | otherwise = show (acc+1) ++ [c] ++ (encodeRL' 0 cs)


                        ----  Decoder ----

decodeRL :: String -> String
decodeRL cipher =
                  let letters = decode1 cipher
                      numbers = decode3 (decode2 cipher) []
                  in decode4 letters numbers


decode1 :: String -> String
decode1 c = [ x | x <- c, x `elem` ['a'..'z']++['A'..'Z'] ]

decode2 :: String -> String
decode2  = map (\x -> if x `elem`['a'..'z']++['A'..'Z'] then ';' else x)

decode3 :: String -> String -> [Int]
decode3 [] acc = if acc == [] then [] else [read acc +0]
decode3 (n:';':ns) acc = [(read (acc ++ [n]))+0] ++ decode3 ns []
decode3 (n:k:ns) acc =  decode3 ([k]++ns) (acc ++ [n])

decode4 :: String -> [Int] -> String
decode4 [] [] = []
decode4 (c:cs) (n:ns) = replicate n c ++ decode4 cs ns

-------------------------------------------------------------------------------------------------------------------------------------------
            -------   Change -------
              ---- ****** SUPER SLOW *****    ----------

change :: Int -> [Int] -> [Int]
change amount diath =snd $ head $ Map.toAscList $ Map.fromList $ useCoins amount diath


useCoins :: Int -> [Int] -> [(Int,[Int])]
useCoins s chan = [(length fs ,fs) | fs <- fn ]
                  where coins = countCoins chan s
                        z = List.subsequences coins
                        fn = remDups [f |f <- z, f/=[], sum f == s]

countCoins :: [Int] -> Int -> [Int]
countCoins [] _ = []
countCoins (n:ns) s = (replicate k n) ++ (countCoins ns s)
                    where k = div s n

remDups :: Eq a => [a] -> [a]
remDups [] = []
remDups (x:xs)
                | x `elem` xs = remDups xs
                | otherwise = x : remDups xs

--------------------------------------------------------------------------------------------------------------------------------------------
--------------  Armstrong  Numbers ----------------

armstrongNum :: Int -> Bool
armstrongNum n = (verArmstrong $ reverse $ digits n) == n


verArmstrong :: [Int] -> Int
verArmstrong ns = sum $ map (^p) ns
                where p = length ns


digits :: Int -> [Int]
digits 0 = []
digits num = l : digits k
        where k = div num 10
              l = mod num 10


---------------------------------------------------------------------------------------------------------------------------------------------
-----------  Beer Song ----------

beerSong :: IO ()
beerSong = putStrLn (reciteBeerSong 100)


reciteBeerSong :: Int -> String
reciteBeerSong n
                  | (n>=0) = beerOnTheWall n ++ "\n" ++ beerDownAround n ++ "\n" ++ "\n" ++ reciteBeerSong (n-1)
                  | otherwise = ""


beerOnTheWall :: Int -> String
beerOnTheWall 0 = " No more bottles of beer on the wall, no more bottles of beer."
beerOnTheWall 1 = " 1 bottle of beer on the wall, 1 bottle of beer."
beerOnTheWall n = " " ++ show n ++ " bottles of beer on the wall, " ++ show n ++ " bottles of beer."


beerDownAround :: Int -> String
beerDownAround 0 = " Go to the store and buy some more, 99 bottles of beer on the wall."
beerDownAround 1 = " Take it down and pass it around, no more bottles of beer on the wall."
beerDownAround 2 = " Take one down and pass it around, " ++ " 1 bottle of beer on the wall."
beerDownAround n = " Take one down and pass it around, " ++ show (n-1) ++ " bottles of beer on the wall."

------------------------------------------------------------------------------------------------------------------------------------------------
--------  Bob --------

bobResponse :: String -> String
bobResponse "" = "Fine!Be that way!"
bobResponse saying
                    | (('?' `elem` saying) && ('!' `elem` saying)) = "Calm down, I know what I'm doing!"
                    | ('?' `elem` saying) = "Sure"
                    | ('!' `elem` saying) = "Whoa, chill out!"
                    | otherwise = "Whatever."

---------------------------------------------------------------------------------------------------------------------------------------------------
-------  Binary -----------

binary :: String -> Int
binary oz =
            let bins = char2num oz
                tens = toBase10 bins
                pow2 = convert tens
            in  sum $ multLists pow2 bins


toBase10 :: [Int] -> [Int]
toBase10 [] = [0]
toBase10 bins = reverse [0..n-1]
              where n = length bins


convert :: [Int] -> [Int]
convert tens = (2^) <$> tens


multLists :: [Int] -> [Int] -> [Int]
multLists [] [] = []
multLists (n:ns) (b:bs) = [n*b] ++ multLists ns bs


char2num :: String -> [Int]
char2num = map (\x -> if x == '1'then 1 else 0)

-----------------------------------------------------------------------------------------------------------------------------------------------------
-----  etl  -----

type Letter = Char
type Old = (Int, [Letter])
type New = [(Int, Letter)]


fromOldToNew :: Old -> New
fromOldToNew (sc,[]) = []
fromOldToNew (sc,(l:ls)) = [(sc,l)] ++ fromOldToNew (sc,ls)

converse :: [Old] -> New
converse = concat . converse'

converse' :: [Old] -> [New]
converse' [] = []
converse' (o:os) = fromOldToNew o : ( converse' os )

-------------------------------------------------------------------------------------------------------------------------------------------------------
-------- scrabble  -------

scrabble_score = [(1,"AEIOULNRST"),(2,"DG"),(3,"BCMP"),(4,"FHVWY"),(5,"K"),(8,"JX"),(10,"QZ")]

getScore :: String -> Int
getScore [] = 0
getScore (l:ls) = (scrabbleScore l scrabble_score) + getScore ls


scrabbleScore :: Char -> [(Int , String)] -> Int
scrabbleScore c [] = 0
scrabbleScore c ((v,k):ss)
                            | (c `elem` k ) = v
                            | otherwise = scrabbleScore c ss

---------------------------------------------------------------------------------------------------------------------------------------------------------
---------  sublist   -----------

sublist :: (Eq k) => [k] -> [k] -> ([k],[k],String)
sublist a b = ((applySublist (a,b, "") aINb )  `applySublist` bINa) `applySublist` aEQb


aINb :: (Eq k) => [k] -> [k] -> ([k],[k], String)
aINb a b = if (a `elem` c) then (a,b," First is sublist of second. ") else (a,b," First is not sublist of second. ")
            where  c = [x | x<- List.subsequences b, length x == (length a)]

bINa :: (Eq k) => [k] -> [k] -> ([k],[k], String)
bINa a b = if (b `elem` c) then (a,b," First is superlist of second. ") else (a,b," First is not superlist of second. ")
            where  c = [x | x<- List.subsequences a, length x == (length b)]

aEQb :: (Eq k) => [k] -> [k] -> ([k],[k], String)
aEQb a b
          |(a == b) = (a,b, " First and second are equal. ")
          | otherwise = (a,b, "First and second are not equal. ")

applySublist :: (Monoid m) => (a,a,m) -> (a -> a -> (b,b,m)) -> (b,b,m)
applySublist (x,y, log) f = let (w,v, newLog) = f x y in (w,v, log `mappend` newLog)


-----------------------------------------------------------------------------------------------------------------------------------------------------------
--------- Roman Numerals ----------


romanNumerals :: Int -> Maybe String
romanNumerals tenNum
                      | tenNum > 3000 = Nothing
                      | otherwise = Just (stringify $ breakInTens tenNum)


stringify :: [(Int,Int)] -> String
stringify [] = ""
stringify ((k,1):ls) = let rmons = [(0,""),(1,"I"),(2,"II"),(3,"III"),(4,"IV"),(5,"V"),(6,"VI"),(7,"VII"),(8,"VIII"),(9,"IX")]
                           in (snd $ head $ filter (\(k1,v1) -> k1 == k ) rmons) ++ stringify ls
stringify ((k,10):ls)
                        | (k <= 5) = if k == 5 then "L" ++ stringify ls else (concat $ take k $ repeat "X") ++ stringify ls
                        | ((k > 5) && (k <= 9)) = if k == 9  then "XC" ++ stringify ls else  ("L" ++ (concat $ take k $ repeat "X")) ++ stringify ls
stringify ((k,100):ls)
                        | (k <= 5) = if k == 5 then "D" ++ stringify ls else (concat $ take k $ repeat "C") ++ stringify ls
                        | ((k > 5) && (k <= 9)) = if k == 9  then "CM" ++ stringify ls else  ("D" ++ (concat $ take k $ repeat "C")) ++ stringify ls
stringify ((k,1000):ls) = (concat $ take k $ repeat "M") ++ stringify ls


breakInTens :: Int -> [(Int,Int)]
breakInTens nbr = let maxTen = 1000
                  in breakInTens'  nbr maxTen


breakInTens' :: Int -> Int -> [(Int,Int)]
breakInTens' nbr ten
                      | (ten > 0 )  =  let  p = nbr `div` ten
                                            r = nbr `mod` ten
                                            tten = ten `div` 10
                                        in  [(p,ten)] ++  breakInTens' r tten
                      | otherwise = []
------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------  Luhn -------------------------------------------

luhn :: String -> Bool
luhn luhnee = let stripped = stripInput luhnee
              in if ((luhn1Step luhnee) == True) then (luhn2Step stripped) else False


luhn1Step :: String -> Bool
luhn1Step input = let allowed = ['0'..'9']
                      stripped = [x | x <- input , x `elem` allowed]
                      numOfBlanks = foldl (\acc x -> if x == ' ' then acc+1 else acc) 0 input
                in ((length stripped) + numOfBlanks) == (length input)


luhn2Step :: String -> Bool
luhn2Step input = sum summable `mod` 10 == 0
                  where onlyNums = map Character.digitToInt input
                        doubler = (\x -> if ((x*2)<9) then (x*2) else (x*2-9))
                        undoubled = nths onlyNums 2
                        summable = (map doubler (reverse $ nths (reverse onlyNums) 2)) ++ undoubled


stripInput :: String -> String
stripInput input = [x | x <- input , x /= ' ']


nths :: [a] -> Int -> [a]
nths _ 0 = []
nths l n = if (len < n) then [] else map fst nthTups
          where
                len = length l
                p = [1..len]
                func = (\(e,p) -> p `mod` n == 0)
                np = zip l p
                nthTups = filter func np

----------------------------------------------------------------------------------------------------------------------------------------------------------------
----------  Word Count -----------

tokenize :: String -> [String]
tokenize inputText = let unpuced = [x | x <- inputText, (not (x `elem` punctuation) || x == ' ')]
                      in [[x] | x<- inputText, (x `elem` punctuation && x /= ' ')] ++ (words unpuced)


wordCount :: [String] -> [(String,Int)]
wordCount werds = wordCount' werds []


wordCount' :: [String] -> [String] -> [(String,Int)]
wordCount' [] acc = []
wordCount' (w:ws) acc  = if not (w `elem` acc ) then  [(w,(length $ filter (==w) ws) +1 )] ++ (wordCount' ws (w : acc )) else ([] ++ wordCount' ws acc)

------------------------------------------------------------------------------------------------------------------------------------------------------------------
----- Phone Number -------

---type NANP = String

---phoneNumber :: String -> Maybe NANP
---phoneNumber pnum = let cpnum = [n | n <- pnum, n `elem` "0123456789"]
  ---                  in removeCCode cpnum >>= checkIfNANP
                ----       ncpnum = removeCCode cpnum
            -----        in if (checkIfNANP ncpnum) == True then Just ncpnum else Nothing

--- (nxx)-nxx-xxxx
---checkIfNANP :: Maybe String -> Maybe NANP
---checkIfNANP pnum
  ---                | (length pnum /= 10)  = Nothing
    ---              | otherwise = if (((head pnum) `elem` bigNs) && ((pnum !! 3) `elem` bigNs)) then pnum else Nothing
      ---            where bigNs = "23456789"


---removeCCode :: String -> Maybe String
---removeCCode pnum
  ---                | (length pnum == 11) && (head pnum == '1') = Just (tail pnum)
    ---              | (length pnum == 10) = Just (tail pnum)
      ---            | otherwise = Nothing

------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------   food-chain ------


foodChain :: IO ()
foodChain = putStrLn foodChain''
---foodChain = putStrLn foodChain'


foodChain' :: String
foodChain' = concat $ map oldLadyFunc animals
              where oldLadyFunc = ("I know an old lady who swallowed a " ++)
                    animals = ["fly.\n" ++ flyFunc, "spider.\n" ++ spiderFunc, "bird.\n" ++ birdFunc, "cat.\n" ++ catFunc,
                                "dog.\n" ++ dogFunc , "goat.\n" ++ goatFunc, "cow.\n" ++ cowFunc]

foodChain'' :: String
foodChain'' = concat $ map oldLadyFunc animals
              where oldLadyFunc = ("I know an old lady who swallowed a " ++)
                    animals =  map animalFunc ["fly","spider","bird","cat","dog","goat","cow"]


animalFunc :: String -> String
animalFunc all@('f':"ly") = all ++ ".\n" ++ flyFunc
animalFunc all@('s':"pider") = all ++ ".\n" ++ spiderFunc
animalFunc all@('b':"ird") = all ++ ".\n" ++ birdFunc
animalFunc all@('c':"at") = all ++ ".\n" ++ catFunc
animalFunc all@('d':"og") = all ++ ".\n" ++ dogFunc
animalFunc all@('g':"oat") = all ++ ".\n" ++ goatFunc
animalFunc all@('c':"ow") = all ++ ".\n" ++ cowFunc
animalFunc _ = ""


flyFunc :: String
flyFunc = "I don't know why she swallowed the fly. Perhaps she'll die.\n\n"

spiderFunc :: String
spiderFunc = "It wriggled and jiggled and tickled inside her.\nShe swallowed the spider to catch the fly.\n"
                ++ flyFunc

birdFunc :: String
birdFunc = "How absurd to swallow a bird!\nShe swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.\n"
                  ++ spiderFunc

catFunc :: String
catFunc = "Imagine that, to swallow a cat!\nShe swallowed the cat to catch the bird.\n" ++ birdFunc

dogFunc :: String
dogFunc = "What a hog, to swallow a dog!\nShe swallowed the dog to catch the cat.\n" ++ catFunc

goatFunc :: String
goatFunc = "Just opened her throat and swallowed a goat!\nShe swallowed the goat to catch the dog.\n" ++ dogFunc

cowFunc :: String
cowFunc = "I don't know how she swallowed a cow!\nShe swallowed the cow to catch the goat.\n" ++ goatFunc

-------------------------------------------------------------------------------------------------------------------------------------------------------------------
------------   Raindrops   --------------

rainDropSpeak :: Int -> String
rainDropSpeak num = if rnds == "" then show num else rnds
                      where rnds = plingPlangPlong $ factors num


factors :: Int -> [Int]
factors num = let ll = [1..num]
                in filter (\x -> (mod num x == 0)) ll


plingPlangPlong :: [Int] -> String
plingPlangPlong [] = ""
plingPlangPlong (3:ll) = "Pling" ++ plingPlangPlong ll
plingPlangPlong (5:ll) = "Plang" ++ plingPlangPlong ll
plingPlangPlong (7:ll) = "Pling" ++ plingPlangPlong ll
plingPlangPlong (_:ll) = "" ++ plingPlangPlong ll

-------------------------------------------------------------------------------------------------------------------------------------------------------------------
------  Panagram  -------

panagram :: String -> Bool
panagram phrase = isPanagram alphabet ils
                  where ils = getToSet phrase
                        alphabet = Set.fromList ['a'..'z']


getToSet :: String -> Set.Set Char
getToSet = setLetters . lower . stripSpaces


lower :: String -> String
lower s = [ Character.toLower x | x <- s ]


stripSpaces :: String -> String
stripSpaces s = [ x | x <- s , x /= ' ' ]


setLetters :: String -> Set.Set Char
setLetters = Set.fromList


isPanagram :: Set.Set Char -> Set.Set Char -> Bool
isPanagram a b = (Set.difference a b) == Set.empty

--------------------------------------------------------------------------------------------------------------------------------------------------------------------
--------   Custom-Set   ---------

newtype CSet a = CSet {getCSet :: [a]} deriving (Show, Eq)


emptyCSet :: CSet a
emptyCSet = CSet []


insertInCSet :: (Eq a) => CSet a -> a -> CSet a
insertInCSet (CSet []) e = CSet [e]
insertInCSet (CSet s) e = if not (e `elem` s) then CSet (s ++ [e]) else CSet s


fromListCSet :: (Eq a) => [a] -> CSet a
fromListCSet l = fromListCSet' l (emptyCSet)


fromListCSet' :: (Eq a) => [a] -> CSet a -> CSet a
fromListCSet' [] (CSet s) = CSet s
fromListCSet' (x:xs) (CSet s) = fromListCSet' xs (CSet k)
                                where k = getCSet $ insertInCSet (CSet s) x


toListCSet :: CSet a -> [a]
toListCSet (CSet k) = getCSet (CSet k)


memberOfCSet :: (Eq a) => CSet a -> a -> Bool
memberOfCSet (CSet s) x = elem x s


sizeOfCSet :: CSet a -> Int
sizeOfCSet (CSet s) = length s


removeFromCSet :: (Eq a) => CSet a -> a -> CSet a
removeFromCSet (CSet s) e = if (memberOfCSet (CSet s) e) then (removeFromCSet' (getCSet (CSet s)) e []) else (CSet s)


removeFromCSet' :: (Eq a) => [a] ->  a -> [a] -> CSet a
removeFromCSet' [] e s = CSet s
removeFromCSet' (x:xs) e s
                            | (x == e) = removeFromCSet' xs e s
                            | otherwise = removeFromCSet' xs e k
                                                  where k = s ++ [x]


isSubSetOfCSet :: (Eq a) => CSet a -> CSet a -> Bool
isSubSetOfCSet (CSet a) (CSet b) = if k > l then (isSubSetOfCSet' a b) else False
                                    where k = sizeOfCSet (CSet a)
                                          l = sizeOfCSet (CSet b)


isSubSetOfCSet' :: (Eq a) => [a] -> [a] -> Bool
isSubSetOfCSet' _ [] = True
isSubSetOfCSet' a (b:bs) = if (b `elem` a) then (isSubSetOfCSet' a bs) else False


unionCSets :: (Eq a) => CSet a -> CSet a -> CSet a
unionCSets (CSet []) (CSet a) = CSet a
unionCSets (CSet a) (CSet []) = CSet a
unionCSets (CSet a) (CSet b) = CSet c
                                where c = outDups (a ++ b)
                                      outDups = foldl (\acc x -> if not (x `elem` acc) then acc ++ [x] else acc) []


intersectionCSet :: (Eq a) => CSet a -> CSet a -> CSet a
intersectionCSet (CSet []) _ = CSet []
intersectionCSet _ (CSet []) = CSet []
intersectionCSet (CSet a) (CSet b) = CSet ([x | x <- a , x `elem` b])


isDisjointFromCSet :: (Eq a) => CSet a -> CSet a -> Bool
isDisjointFromCSet (CSet a) (CSet b) = if ((intersectionCSet (CSet a) (CSet b)) == emptyCSet) then True else False
--------------------------------------------------------------------------------------------------------------------------------------------------------------------

------ Proverb -------


proverb :: [String] -> IO ()
proverb wordList = putStrLn (concat ((allPhrases wordList) ++ (lastPhrase $ head wordList)))

allPhrases :: [String] -> [String]
allPhrases (w:[]) = []
allPhrases (w1:w2:ws) = ["For want of a " ++ w1 ++ " the " ++ w2 ++ " was lost\n"] ++ (allPhrases (w2:ws))

lastPhrase :: String -> [String]
lastPhrase fw = ["And all that for the want of a " ++ fw]

------------------------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-------- Hamming --------

hamming :: String -> String -> Int
hamming dna1 dna2 = if ((eqLens dna1 dna2) && (dnaAcc dna1) && (dnaAcc dna2)) then (hammingDistance dna1 dna2) else -1


eqLens :: String -> String -> Bool
eqLens str1 str2 = (length str1) == (length str2)


dnaAcc :: String -> Bool
dnaAcc str =
              let dis = [x | x <- str, not (x `elem` "AGTC")]
              in (length dis == 0)


hammingDistance :: String -> String -> Int
hammingDistance [] [] = 0
hammingDistance (d:ds) (r:rs)
                              | d == r = hammingDistance ds rs
                              | otherwise = 1 + hammingDistance ds rs

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
