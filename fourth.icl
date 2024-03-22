module fourth 
import StdEnv

//bta5bp

/*
Find the Maximum Even Element in a List of Lists:
Write a function that takes a list of lists, and your task is to find and return the
largest even number present in any of the sublists. If there are no even numbers in
the sublists, return -1.
Examples:
For the input [[1, 2, 3], [4, 5], [6, 7, 8]], the function should return 8, as it's the
largest even number in any of the sublists.
*/


/* 
max :: [Int] -> Int
max [x] = x
max [x,y:xs] 
|  x > y = max [x:xs]
= max [y:xs]
*/
//Start = max [1, 2, 4]

maxEvenElement :: [[Int]] -> Int 
maxEvenElement [] = -1
maxEvenElement ls = maxList(filter isEven(flatten ls))

/*
maxEvenElement [] = -1
maxEvenElement ls = maxList[x\\x<-flist|isEven x]
where flist = flatten ls
*/
//Start = maxEvenElement [[1, 2, 3], [4, 5], [6, 7, 8]] // 8
Start = maxEvenElement [[1, 1, 3], [1, 1], [1, 7, 1], [1]] // -1
// Start = maxEvenElement [[1,1,1], []] // -1