module HW2
import StdEnv

/*
Please write your NEPTUN code here: BTA5BP

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
Find the sum of the numbers from a range (beginning to end) with step s.
The numbers are not guaranteed to be in the right order (the first number
may be greater than the second).
You can assume that the numbers are positive.
*/
inc :: Int Int Int-> Int
inc first last range
| first > last = 0
= first + inc (first + range) last range

dec :: Int Int Int -> Int
dec last first range
| last < 0 = 0
= last + dec (last - range) first range


seqSum :: Int Int Int -> Int
seqSum x y z
| (x < y) = inc x y z
|otherwise = dec x y z



//Start = seqSum 1 5 1 // 1 + 2 + 3 + 4 + 5 = 15    
//Start = seqSum 1 5 2 // 1 + 3 + 5 = 9
//Start = seqSum 5 1 2
//Start = seqSum 1 5 3 // 1 + 4 = 5
//Start = seqSum 5 1 1 // 15




//Task 2
/*
Find the n-th index of the Padovan number sequence.
P(n) = P(n - 2) + P(n - 3) for n >= 3, with P(0) = P(1) = P(2) = 1.
Read more here: https://en.wikipedia.org/wiki/Padovan_sequence
P0 = P1 = P2 = 1 ,
P(7) = P(5) + P(4)
     = P(3) + P(2) + P(2) + P(1)
     = P(2) + P(1) + 1 + 1 + 1
     = 1 + 1 + 1 + 1 + 1 
     = 5
*/


padovan :: Int -> Int
padovan x
| x < 0 = abort "invalid argument"
| x == 0 = 1 
| x == 1 = 1
| x == 2 = 1
| otherwise = padovan (x - 2) + padovan(x - 3)


//Start = padovan 4 // 2
//Start = padovan 6 // 4
//Start = padovan 9 // 9
//Start = padovan 12 // 21
//Start = padovan 0 // 1
//Start = padovan 1 // 1
//Start = padovan 2 // 1
//Start = padovan -4 // "invalid argument"