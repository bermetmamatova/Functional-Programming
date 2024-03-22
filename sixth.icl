module sixth
import StdEnv


/*Given the list of tuples. Each tuple has 3 element: L, R and Step.
For each tuple generate a list of numbers from L to R increasing with Step (L,L+Step,L+2*Step...).
For example, if L is 1, R is 10 and step is 4 list would be [1,5,9]. Your function should return
a list of lists.
*/

//tplAux :: (Int,Int,Int) -> [Int]

expandList :: [(Int,Int,Int)] -> [[Int]]
expandList ls = [[fst3 x, (fst3 x + thd3 x)..snd3 x ]\\ x<- ls]

//Start = expandList [(1,10,4), (3,5,4), (5,4,1), (1,10,3)] // [[1,5,9],[3],[],[1,4,7,10]]
//Start = expandList [] // []
//Start = expandList [(5,3,-1),(2,13,3),(1,8,1)] // [[5,4,3],[2,5,8,11],[1,2,3,4,5,6,7,8]]
//Start = expandList [(1,12,100), (2,5,10), (4,-1,-10)] // [[1],[2],[4]]


