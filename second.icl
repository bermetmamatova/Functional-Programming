module second
import StdEnv

//noptune: BTA5BP

/* Write a function that sums the odd digits of a given positive integer number*/

sumOddDigits :: Int -> Int
sumOddDigits x 
| x == 0 = 0
| isOdd(y) =  y + sumOddDigits(x/10)
| otherwise = sumOddDigits(x/10)

where y = x rem 10
 
isOdd :: Int -> Bool 
isOdd y = (y rem 2) <> 0






//Start = sumOddDigits 123 // 4
//Start = sumOddDigits 223456 // 8
//Start = sumOddDigits 5555 // 20
//Start = sumOddDigits 1 // 1
//Start = sumOddDigits 2 // 0
