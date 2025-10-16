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



