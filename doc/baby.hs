import Prelude

doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmall x = if x > 100
    then x
    else doubleMe x

doubleSmall' x = (if x > 49 then x else doubleMe x) + 1

conanO'Brien = "It's a-me, Conan O'Brien!" 


boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]   

nouns = ["hobo","frog","pope"]  
adjs = ["lazy","grouchy","scheming"]

nounAndAdj a n = [adjective ++ " " ++ noun | adjective <- a, noun <- n]

length' xs = sum [1 | _ <- xs] 

removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]




numbers = zip [1 .. 5] ["one", "two", "three", "four", "five"] 
fruits = zip [1..] ["apple", "orange", "cherry", "mango"] 


triangles = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10] ]  
rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]
rightTriangles' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]

addThree :: Int -> Int -> Int -> Int 
addThree x y z = x + y + z 

addThree' x y z = x + y + z 


factorial' :: Integer -> Integer  
factorial' n = product [1..n]  

lucky :: (Integral a) => a -> String  
lucky 7 = "LUCKY NUMBER SEVEN!"  
lucky x = "Sorry, you're out of luck, pal!" 


sayMe :: (Integral a) => a -> String  
sayMe 1 = "One!"  
sayMe 2 = "Two!"  
sayMe 3 = "Three!"  
sayMe 4 = "Four!"  
sayMe 5 = "Five!"  
sayMe x = "Not between 1 and 5" 

factorial :: (Integral a) => a -> a  
factorial 0 = 1  
factorial n = n * factorial (n - 1) 

tell :: (Show a) => [a] -> String  
tell [] = "The list is empty"  
tell (x:[]) = "The list has one element: " ++ show x  
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y 


length'' :: (Num b) => [a] -> b  
length'' [] = 0  
length'' (_:xs) = 1 + length'' xs  

capital :: String -> String  
capital "" = "Empty string, whoops!"  
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x] 


densityTell' :: (RealFloat a) => a -> a -> String  
densityTell' mass volume  
    | mass / volume < 1.2 = "Wow! You're going for a ride in the sky!"  
    | mass / volume <= 1000.0 = "Have fun swimming, but watch out for sharks!"  
    | otherwise   = "If it's sink or swim, you're going to sink."  


max' :: (Ord a) => a -> a -> a  
max' a b  
    | a > b     = a  
    | otherwise = b 


myCompare :: (Ord a) => a -> a -> Ordering  
a `myCompare` b  
    | a > b     = GT  
    | a == b    = EQ  
    | otherwise = LT 


densityTell :: (RealFloat a) => a -> a -> String  
densityTell mass volume  
    | density < air = "Wow! You're going for a ride in the sky!"  
    | density <= water = "Have fun swimming, but watch out for sharks!"  
    | otherwise   = "If it's sink or swim, you're going to sink."  
    where density = mass / volume  
          air = 1.2  
          water = 1000.0  



initials :: String -> String -> String  
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname 

initials' :: String -> String -> String  
initials' (f:_) (l:_) = [f] ++ ". " ++ [l] ++ "."  

calcDensities :: (RealFloat a) => [(a, a)] -> [a]  
calcDensities xs = [density m v | (m, v) <- xs]  
    where density mass volume = mass / volume  

cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea 



divideByTen :: (Floating a) => a -> a  
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool  
isUpperAlphanum = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a  
applyTwice f x = f (f x)


zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys


flip' :: (a -> b -> c) -> (b -> a -> c)  
flip' f y x = f x y 


map' :: (a -> b) -> [a] -> [b]  
map' _ [] = []  
map' f (x:xs) = f x : map' f xs  
-- can be done wit hlist comprehension
-- map' (+3) [1,5,3,1,6] same as [x+3 | x <- [1,5,3,1,6]]


filter' :: (a -> Bool) -> [a] -> [a]  
filter' _ [] = []  
filter' p (x:xs)  
    | p x       = x : filter' p xs  
    | otherwise = filter' p xs  
-- let notNull x = not (null x) in filter notNull [[1,2,3],[],[3,4,5],[2,2],[],[],[]] 



quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) = 
    let smallerSorted = quicksort (filter (<=x) xs)  
        biggerSorted = quicksort (filter (>x) xs)  
    in  smallerSorted ++ [x] ++ biggerSorted  


largestDivisible :: (Integral a) => a  
largestDivisible = head (filter p [100000,99999..])  
    where p x = x `mod` 3829 == 0 

-- sum of odd square < 10000
-- sum (takeWhile (<10000) (filter odd (map (^2) [1..]))) 
-- with list comprehension
-- sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)])


-- Collatz sequence
chain :: (Integral a) => a -> [a]  
chain 1 = [1]  
chain n  
    | even n =  n:chain (n `div` 2)  
    | odd n  =  n:chain (n*3 + 1) 

-- for all starting numbers between 1 and 100, how many chains have a length greater than 15?
numLongChains :: Int  
numLongChains = length (filter isLong (map chain [1..100]))  
    where isLong xs = length xs > 15


--let listOfFuncs = map (*) [0..]

-- lambda 
-- syntax : \params -> body
-- better to surround with ()

numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))


-- flip with lambda
-- flip' :: (a -> b -> c) -> b -> a -> c  
-- flip' f = \x y -> f y x 


-- foldl take f acc and list
-- f: \acc x -> ...
-- acc: starting value
sum' :: (Num a) => [a] -> a  
sum' xs = foldl (\acc x -> acc + x) 0 xs

-- with how functions are curried in mind
sum2 :: (Num a) => [a] -> a  
sum2 = foldl (+) 0 

-- use of fodl to implement elem
elem' :: (Eq a) => a -> [a] -> Bool  
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

-- right fold (foldr) works the same except the acc is passed in second 
-- \x acc -> ...



-- $: function application with lowest precedance
-- ($) :: (a -> b) -> a -> b  
-- f $ x = f x 

-- simplify expression
-- sum (map sqrt [1..130])
-- sum $ map sqrt [1..130]

-- sqrt (3 + 4 + 9)
-- sqrt $ 3 + 4 + 9

-- sum (filter (> 10) (map (*2) [2..10]))
-- sum $ filter (> 10) $ map (*2) [2..10]



-- composing two functions produces a new function that, 
-- when called with a parameter x, is the equivalent of calling g with x 
-- and then calling the f with that result

-- function composition with .
-- (.) :: (b -> c) -> (a -> b) -> a -> c  
--    f . g = \x -> f (g x)  


-- can be more concise than lambda

-- map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24]
-- map (negate . abs) [5,-3,-6,7,-3,2,-19,24]

-- map (\xs -> negate (sum (tail xs))) [[1..5],[3..6],[1..7]]
-- map (negate . sum . tail) [[1..5],[3..6],[1..7]]


-- with function taking multipel parameter 

-- sum (replicate 5 (max 6.7 8.9))
-- sum . replicate 5 . max 6.7 $ 8.9

-- replicate 100 (product (map (*3) (zipWith max [1,2,3,4,5] [4,5,6,7,8])))
-- replicate 100 . product . map (*3) . zipWith max [1,2,3,4,5] $ [4,5,6,7,8]

fn x = ceiling (negate (tan (cos (max 50 x))))  
fn' = ceiling . negate . tan . cos . max 50 


-- rewrite example of function
oddSquareSum :: Integer  
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))  

-- with composition
oddSquareSum1 :: Integer  
oddSquareSum1 = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]  

-- more friendly?
oddSquareSum2 :: Integer  
oddSquareSum2 = 
    let oddSquares = filter odd $ map (^2) [1..]  
        belowLimit = takeWhile (<10000) oddSquares  
    in  sum belowLimit  


-- fixity : associative and order
-- data cosntructor recursif 

-- List implementation
infixr 5 :-:  
data ListB a = Empty | a :-: (ListB a) deriving (Show, Read, Eq, Ord) 


-- ++ operator for List (ListB) (custom name here :'.++')
infixr 5  .++  
(.++) :: ListB a -> ListB a -> ListB a  
Empty .++ ys = ys  
(x :-: xs) .++ ys = x :-: (xs .++ ys)

