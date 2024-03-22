module HW1

import StdEnv

/*
Please write your NEPTUN code here: BTA5BP Bermet Mamatova

Your submission should not have any errors when running the code.
You'll receive a total of 100 points when you successfully solve both problems,
with 50 points awarded for each.

You should not delete anything from the given code, given test cases should stay
the same, but you can add your tests as well. Don't change the given function signatures. 

Make sure that you comment all 'Start'-s before submitting the code.
*/

/*
Task 1:

Write a function that will check if two given characters have different cases.

    If either of the characters is not a letter, return -1
    If the letters have different cases, return 1
    If both characters are letters, but the same case, return 0
    
It's important that you use the ASCII table!

Examples:

'a' and 'G' returns 1

'A' and 'c' returns 1

'b' and 'g' returns 0

'B' and 'G' returns 0

'0' and '?' returns -1

Hint: Use fromChar() function to get the ASCII value of a character.
*/

f1 :: Char -> Int
f1 x = fromChar x 


compare_letters :: Char Char -> Int
compare_letters y z 

| ((isUpper y) &&(isLower z)) || ((isUpper z)&&(isLower y)) = 1 //diff cases
| ((isUpper y) && (isUpper z)) || ( (isLower y) && (isLower z)) = 0 // same cases
| ((isMember(f1 y)[65..90] || isMember(f1 y)[97..122]) || (isMember(f1 z)[65..90] || isMember(f1 z)[97..122])) = -1 //either not char

//Start = compare_letters 'A' '*' // -1
//Start = compare_letters '0' 'Z' // -1 ?
//Start = compare_letters 'A' 'Z' // 0
//Start = compare_letters 'a' 'z' // 0
//Start = compare_letters 'a' 'Z' // 1
//Start = compare_letters 'z' 'A' // 1



/*
Task 2:

Columns are a definitive attribute of the classical architectural style.

A building has a certain number of columns of the same diameter and the
distances between the columns are the same.

You are given the number of the columns, their diameter and the distance 
between them. Calculate the length of the building.

You need to determine if the building is long or not. Any building
longer than 50 will be considered long. Return the boolean value.
(True if the building is considered long and False if it is considered short)

You don't need to check the input, consider it to be correct.
*/

is_building_long :: Int Int Int -> Bool
is_building_long x y z
= ((x*y)+(z*(x-1)))>50

//Start = is_building_long 2 10 20 // False
//Start = is_building_long 5 15 25 // True
//Start = is_building_long 3 13 24 // True
//Start = is_building_long 2 10 30 // False