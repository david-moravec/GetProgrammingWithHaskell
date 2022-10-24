subseq start stop list = if start > len
                          then []
                          else if stop > len
                          then listWithoutStart
                          else take diff listWithoutStart
  where len = length list
        diff = stop - start
        listWithoutStart = drop start list

inFirstHalf list element = element `elem` halfList
  where halfList = drop halfLen list
        halfLen = length list `div` 2

say 1 = "one"
say 2 = "two"
say n = "a bucnh"


myTail (_:xs) = xs
myTail [] = []

myGCD a b = if remainder == 0
            then b
            else myGCD b remainder
  where remainder = a `mod` b

myGCDpattern a 0 = a
myGCDpattern a b = myGCD b remainder
  where remainder = a `mod` b

myTake 0 (x:xs) = []
myTake n (x:xs) = [x] ++ myTake (n-1) (xs)

myDrop n (x:xs) = if lenList == requiredLen
                  then (x:xs)
                  else myDrop (n-1) (xs)
  where requiredLen = lenList - n
        lenList = length (x:xs)

myLength [] = 0
myLength xs = 1 + myLength (tail xs)

myCycle list = list ++ myCycle list

myReverse [] = []
myReverse list =  (myReverse myTail) ++ [head list]
  where myTail = tail list

genFuncByAll _ [] = []
genFuncByAll func (x:xs) = (func x):(genFuncByAll func xs)

myFold _ arg [] = arg
myFold binaryFunc arg list = binaryFunc (myFold binaryFunc arg (tail list)) list 

myElem el list = lenFiltered > 0
  where lenFiltered = length filtered
        filtered = filter (\x -> x == el) list

isPalindrome word = word == reverse word

mapOutSpaces sentence = map deleteSpace sentence
  where deleteSpace x = if x == ""
                          then []
                        else [x]

harmonic n = foldl (+) 0 transformed
  where transformed = map (\x -> 1/x) [1 .. n]
        