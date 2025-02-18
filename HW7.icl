module HW7
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




/*
	Hint! In clean, strings are arrays of chars, so you can index them the same
	way you do with the arrays:
		"asd".[1] == 's'
		[x \\ x <-: "asd"] == ['a','s','d']
		{x \\ x <-: "asd"} == "asd"
*/

//Task 1

/*
You are given a string and a letter and you need to return the index of the
last occurrence of that letter in the string. If there is no such letter
in the string, then the function should return -1.
*/

last_occ :: String Char -> Int
last_occ st ch 
| isMember ch ls == False = -1
= last[p\\p<-[0..] &  letter<- ls|(letter == ch)]
where ls = [c\\c<-:st]


//Start = last_occ "Hello world!!!" 'l' // 9
//Start = last_occ "Hello world!!!" 'o' // 7
//Start = last_occ "aasdddasssda" 'd' // 10
//Start = last_occ "xaasdddasssda" 'x' // 0
//Start = last_occ "Hello world!!!" 'A' // -1
//Start = last_occ "" 'q' // -1
//Start = last_occ "Hello" '!' // -1


//Task 2

/*
Help the lazy programmer find out if every paranthese is closed.

You are given a string of parentheses. Determine if the order
of the parentheses is valid. The function should return true if the string is
valid, and false if it's invalid.
*/
pAux :: Int [Char] -> Bool
pAux 0 [] = True
pAux _ [] = False
pAux n [x:xs]
| (x == '[') = pAux (n+1) xs
| (x == ']') && (n>0)= pAux (n-1) xs
| hd [x:xs]== ']' = False
= pAux n xs

para :: String -> Bool
para st  =  pAux 0 [x\\x<-:st]


	  
//Start = para "[]" // True
//Start = para "[[[]]]" // True
//Start = para "[][][]" // True
//Start = para "[[][]][]" // True
//Start = para "[][[]][[[]]][[]][]" // True
//Start = para "][" // False
//Start = para "[][][" // False
//Start = para "[[[]]" // False
//Start = para "[]][[]" // False
//Start = para "[][[]" // False
//Start = para "]" //False