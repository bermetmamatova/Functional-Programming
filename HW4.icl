module HW4
import StdEnv


//bta5bp

//Task 1
/*
	You are given two lists (one contains numbers, the other one contains indexes) and a number n.
	Your task is to extract the numbers from the given numbers lists of the coresponding indexes,
	and to concatinate it with the rest of the list. Repeat this process n times, and return all 
	the encodings.
	
	Example:
	encode [1, 2, 3, 4, 5, 6] [2, 4] 3 => [[3,5,1,2,4,6],[1,4,3,5,2,6],[3,2,1,4,5,6]]
	
	Explanation:
	we take the numbers under indexes [2, 4] from the list: [1, 2, 3, 4, 5, 6] => [3, 5]
	and to that, we add the rest of the numbers in the same order => [3, 5] ++ [1, 2, 4, 6]
	and we repeat this the same process, but now on the new list: [3, 5, 1, 2, 4, 6]
	[1, 4] ++ [3, 5, 2, 6] => [1, 4, 3, 5, 2, 6]
	[3, 2] ++ [1, 4, 5, 6] => [3, 2, 1, 4, 5, 6]
	
	You can expect the indexes to always be existent.
*/

//*couldn't make "rem" to work with lists, which contain more than 2 elems(

rem :: [Int] [Int] -> [Int]
rem list [] = list
rem list [x:xs] = rem(removeAt x list)( map (dec) xs)


//encode (afterDeletion = rem list1 (sort list2))
encode :: [Int] [Int] Int -> [[Int]]
encode list1 list2 0 = []
encode list1 list2 n = [deletedElems ++ afterDeletion : encode new list2 (n-1)]



where deletedElems = [list1!! x\\x<-list2]
	  afterDeletion = rem list1 list2
	  new = deletedElems ++ afterDeletion


//Start = encode [1, 2, 3, 4, 5, 6] [2, 4] 1 // [[3,5,1,2,4,6]]
//Start = encode [1, 2, 3, 4, 5, 6] [2, 4] 3 // [[3,5,1,2,4,6],[1,4,3,5,2,6],[3,2,1,4,5,6]]
//Start = encode [1, 2, 3, 4] [1, 3] 5 // [[2,4,1,3],[4,3,2,1],[3,1,4,2],[1,2,3,4],[2,4,1,3]]
//Start = encode [1, 2, 3, 4, 5, 6] [0, 5] 3 // [[1,6,2,3,4,5],[1,5,6,2,3,4],[1,4,5,6,2,3]]
//Start = encode [1, 2, 3, 4, 5, 6] [0, 1] 3 // [[1,2,3,4,5,6],[1,2,3,4,5,6],[1,2,3,4,5,6]]
//Start = encode [1, 2, 3, 4] [2, 0, 1] 3 // [[3,1,2,4],[2,3,1,4],[1,2,3,4]] 
//Start = encode [1, 2, 3] [1] 3 // [[2,1,3],[1,2,3],[2,1,3]]
//Start = encode [1, 2, 3] [2, 1, 0] 3 // [[3,2,1],[1,2,3],[3,2,1]] 
 


//Task 2 
/*
	Garfield is a very lazy cat, but he loves to eat lasagnia. Somehow, he is on a table
	that has a lot of plates with lasagnia. Help Garfield find the shortest way to a plate.
	
	You are given the coordinate of Garfield (a tuple) and a list with the coordinates 
	of the plates (list of tuples). Return the Real value of the shortest distance between
	the Garfield's coordinate and a plate coordinate.
	
	Use the distance formula: distance = sqrt ((x_2 - x_1)^2 + (y_2 - y_1)^2)
	You can expect the distances to be less than 100.
*/

min_dist :: (Int, Int) [(Int, Int)] -> Real
min_dist garfield list = minList[sqrt(toReal((fst points - fst garfield)^2 + (snd points - snd garfield)^2))\\points<-list]



//Start = min_dist (0, 0) [(0, 1), (0, 2)] // 1
//Start = min_dist (0, 0) [(0, 10), (0, 20), (3, 0), (5, -1)] // 3
//Start = min_dist (1, 3) [(1, 9), (6, 3), (1, -1)] // 4
//Start = min_dist (1, 3) [(1, 90), (11, 3), (1, -90)] // 10
//Start = min_dist (5, 7) [(2, 6), (3, 8), (1, 9), (12, 23)] // 2.23606797749979
//Start = min_dist (5, 7) [(14, 26), (-33, 5), (-12, 22), (1, -50), (-10, -12)] // 21.0237960416286