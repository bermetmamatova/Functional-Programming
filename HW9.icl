module HW9
import StdEnv

/*
Please write your NEPTUN code here: bta5bp

Your submission should not have any errors when running the code.
You'll receive a total of 100 points when you successfully solve both problems,
with 50 points awarded for each.

You should not delete anything from the given code, given test cases should stay
the same, but you can add your tests as well. Don't change the given function signatures. 
You can add your own functions, of course.

Make sure that you comment all 'Start'-s before submitting the code.
*/




//Task 1
/*
You are given a tree and your task will be to represent it in a string format. To read it easier
we will use different types of parantheses, in the next order "()" "<>" "{}" "[]". So, for
each level of the tree, different parantheses are used. Once we run out, the parantheses cycle
repeats. Instead of the leaves, insert "|".

Example: tree_to_paranth = Node (Node (Node (Node (Node Leaf Leaf) Leaf) Leaf) Leaf) Leaf -> "(<{[(||)|]|}|>|)"
		                     ^     ^     ^     ^     ^ 
						     |     |     |     |     |
		                    ()    <>    {}    []    ()
*/

:: Tree = Node Tree Tree | Leaf
treePar :: Tree Int -> String
treePar Leaf n = "|"
treePar (Node le ri) n 
| (n rem 4) == 1 = "(" +++ treePar le (n+1) +++ treePar ri (n+1) +++ ")"
| (n rem 4) == 2 = "<" +++ treePar le (n+1) +++ treePar ri (n+1) +++ ">"
| (n rem 4) == 3 = "{" +++ treePar le (n+1) +++ treePar ri (n+1) +++ "}"
| (n rem 4) == 0 = "[" +++ treePar le (n+1) +++ treePar ri (n+1) +++ "]"

//Start = treePar (Node (Node Leaf Leaf) Leaf) 1
tree_to_paranth :: Tree -> String
tree_to_paranth tree = treePar tree 1


//Start = tree_to_paranth (Node (Node Leaf Leaf) Leaf) // "(<||>|)"
                            // (     <     |   | >   |)
//Start = tree_to_paranth (Node (Node (Node (Node (Node Leaf Leaf) Leaf) Leaf) Leaf) Leaf) // "(<{[(||)|]|}|>|)"
//Start = tree_to_paranth (Node (Node (Node (Node Leaf Leaf) Leaf) Leaf) (Node (Node (Node Leaf Leaf) (Node Leaf Leaf)) Leaf)) // "(<{[||]|}|><{[||][||]}|>)"





//Task 2
/*
Define the (==) operator for trees. Two trees are equal when they have the same branch configuration, 
and their values an the same positions are equal.

Example: (BNode 1 (BNode 2 BLeaf BLeaf) BLeaf) == (BNode 1 (BNode 2 BLeaf BLeaf) BLeaf)
							
						  1 										1
						 / \									   / \
						2   L                  ==                 2   L   
					   / \										 / \
					  L   L									   	L   L
					  
		 (BNode 3 (BNode 2 BLeaf BLeaf) BLeaf) != (BNode 1 (BNode 2 BLeaf BLeaf) BLeaf)
						   
						  3 										1
						 / \									   / \
						2   L                  !=                 2   L   
					   / \										 / \
					  L   L									   	L   L
*/

:: BTree a = BNode a (BTree a) (BTree a) | BLeaf

instance == (BTree a) | Eq a
where
	(==) BLeaf BLeaf = True
	(==) (BNode a le ri) (BNode b le1 ri1) = (a==b) && (le == le1) && (ri == ri1) 
	(==) _ _ = False

//Start = (BNode 1 (BNode 2 BLeaf BLeaf) BLeaf) == (BNode 1 (BNode 2 BLeaf BLeaf) BLeaf) // True
//Start = (BNode 3 (BNode 2 BLeaf BLeaf) BLeaf) == (BNode 1 (BNode 2 BLeaf BLeaf) BLeaf) // False
//Start = (BNode 1 (BNode 1 BLeaf BLeaf) (BNode 1 BLeaf BLeaf)) == (BNode 1 (BNode 1 BLeaf BLeaf) BLeaf) // False
//Start = (BNode 1 (BNode 1 BLeaf (BNode 1 BLeaf BLeaf)) (BNode 1 (BNode 1 BLeaf BLeaf) BLeaf)) == (BNode 1 BLeaf (BNode 1 (BNode 1 BLeaf BLeaf) (BNode 1 (BNode 1 BLeaf BLeaf) BLeaf))) // False
//Start = (BNode 1 (BNode 2 BLeaf (BNode 3 (BNode 4 BLeaf BLeaf) BLeaf)) (BNode 5 (BNode 6 BLeaf BLeaf) BLeaf)) == (BNode 1 (BNode 2 BLeaf (BNode 3 (BNode 4 BLeaf BLeaf) BLeaf)) (BNode 5 (BNode 6 BLeaf BLeaf) BLeaf)) // True
