module third
import StdEnv

/*
You are given a tuple of lower and upper bounds and a list of tuples, where the first element is course
code and the second one is course score. Filter the list and return the list of course codes, which have
scores greater or equal to the lower bound and less or equal to the upper bound.
*/

//aux :: (Int,Int)Int -> Bool
//aux (a,b) n

//Start = aux (10, 15) 10
rangeFilter :: (Int, Int) [(String,Int)] -> [String]
rangeFilter (x,y) ls = [fst c \\ c <- ls | snd c >= x && snd c <= y]

//Start = rangeFilter (10, 15) [("A",12),("B",3),("C",5),("E",14),("F",16)] // ["A","E"]
//Start = rangeFilter (3, 13) [("A",12),("B",3),("C",5),("E",14),("F",16)] // ["A", "B", "C"]
//Start = rangeFilter (5, 7) [] // []
//Start = rangeFilter (15, 3) [("A",12),("B",3),("C",5),("E",14),("F",16)] // []
//Start= rangeFilter (0, 2) [("A",12),("B",3),("C",5),("E",14),("F",16)] // []


/*
2. Given a list of lists of Int.
For each element greater than 5 of each sublist create a triple like (number, sum, product)
where
number -> the number itself
sum -> the sum of all the integers in the [1,number) interval
product -> product of all the integers in the [1,number)

Example:
[[1,2,4],[6,8,2,1]] -> [[],[(6,15,120),(8,28,5040)]]
because 1,2,4,2,1 are not greater than 5,
15 = 1+2+3+4+5, 120 = 1*2*3*4*5
28 = 1+2+3+4+5+6+7. 5040 = 1*2*3*4*5*6*7
*/
//check:: [Int] -> [[Int]]
//check ls = map (takeWhile ((<)5)) ls
//Start = check [1..6]
//sum :: Int -> Int
//sum n = sum [1..n]

//[(n,sum [1..n], prod [1..n])]

numAux :: [Int] -> [(Int, Int,Int)]
numAux ls = [(x, sum[1..x],prod[1..x]) \\ x<-ls| x>5]


numSumProd::[[Int]]->[[(Int,Int,Int)]]
numSumProd ls = [numAux a\\ a<-ls] 
//Start = numSumProd [[1,2,4],[6,8,2,1]] //[[],[(6,21,720),(8,36,40320)]]
//Start = numSumProd [[1..6],[1..5]]//[[(6,15,120)],[]]
//Start = numSumProd [[1,2,2],[]]//[[],[]]
Start = numSumProd []//[]