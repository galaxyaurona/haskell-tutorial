-- variable declaration
x = 2
sampleStr = "abcdef"
sampleList = [1,2,3,4,5]
-- sample functions
printStr :: String -> IO()
printStr = print
-- math function
double :: Num a => a -> a
double x = x + x
-- function composition
quadruple :: Num a => a -> a
quadruple = double . double

-- infinite list , lazily evalued
-- also sample of type anotation
infiniteList = [0..] :: [Float]

-- take from infinite list aka list generator
from0To :: Int -> [Float]
from0To 0 = [0]
from0To n = take n infiniteList

-- function operate in list
average ::  [Float] -> Float
average [] = 0.0
-- need to run fromIntegral to convert Int -> float
-- don't use `div` here, must use (/)
average (x:xs) = sum(x:xs) / (fromIntegral(length(x:xs)))

-- exercise 2
-- N needs to be lower case
n = a `div` length xs
    where 
        a    = 10
        xs   = [1,2,3,4,5]
-- exercise 3
myLast :: [a] -> a
myLast (x:xs) = (x:xs) !! (length((x:xs))-1)

-- exercise 4
-- last using recursion
myOtherLast :: [a] -> a
myOtherLast [a] = a
myOtherLast (x:xs) = myOtherLast xs

-- exercise 5

-- first init
myInit :: [a] -> [a]
myInit [] = []
myInit (x:xs) = (x:(take (length(xs)-1) xs ))
-- recursion init
myOtherInit :: [a] -> [a]
myOtherInit [] = []
myOtherInit [a] = []
myOtherInit (x:xs) = (x: myOtherInit xs)


-- main function
main :: IO()
main = do
    print("Hello")
    printStr "world"
    -- indexing
    print( sampleStr !! 2)
    print( 2 `take` sampleList  )
    print(quadruple 4)
    print(average(from0To 4))
    print(last(from0To 10))
    print(myOtherLast (from0To 0))
    print( myInit [1,2,3])
    print( myInit [1])
    print( myInit [] :: [Int] )
    print( myOtherInit [1,2,3])
    print( myOtherInit [1])
    print( myOtherInit [] :: [Char])