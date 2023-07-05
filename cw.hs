--functional programming coursework corben sayer
---- Part 1 ----


{-
a) List and explain (in your own words) 3 benefits that Functional Programming brings to programmers
		
	1) A key benefit of functional programming is lazy evaluation. Lazy evaluation is an evaluation strategy based on the notion that an
		expression whose value is never used need never be evaluated [1]. What this means is that functional programming languages only calculate
		values when they are needed. This could result in a performance boost. For example, imagine there was a large resource heavy argument
		in your code but it never got used: in a programming language with lazy evaluation that argument would never have to be actually calculated
		unless it was needed, saving time and resources that would otherwise have been used in a programming language without lazy evaluation.
		Furthermore, only calculating values when they are needed means we can do more things such as work with infinite lists. As we are only
		calculating the elements we need, we don't have to try and use up all our memory finding every element in the list. 
	2) Another benefit of functional programming is that since the functions are pure functions, [2] it means that when they take an argument,
		that output will be unchangeable. This means the output will never be something you're not expecting or can't figure out, 
		therefore allowing you to be much more effective with testing and debugging as you are able to spot easily where and why things
		are or aren't doing what you expect. This allows you to quickly take the neccessary steps to make sure your code works as you actually
		intended it to. Furthermore, just by looking at a function signature, such as f :: Int -> Int, you can have a good idea of how the
		function behaves, as you are able to see exactly what goes in and what comes out, thus also helping you with any testing or debugging. 
	3) Another benefit of functional programming is to do with static variables. In functional programming, once you have set the variable,
		you are unable to modify it. This means that the program is much more stable as there are fewer opportunities for the code to change
		and thus an error to occur. This along with function signatures only allowing valid inputs for functions means that code can be much 
		more secure from possible errors.
	
	
b) Explain in your own words what a (mathematical) function is and discuss to what extent Haskell functions resemble mathematical functions
	
	A mathematical function is an expression which defines a relationship between a set of values (e.g. "inputs") to another set of values
	(e.g. "outputs") where each individual input applied on the function results in a single output. In haskell, a function operates on some
	input parameters and returns a result. This mirrors very closely to that of a mathematical function: in both cases we have some kind of 
	input / initial value which when is performed on the function we get a single result returned. Furthermore, all haskell programs will result
	in the same answer when given the same inputs, regardless of the order in which the calculations were performed [3] which is the same case
	when you compare it to that of a mathematical function. For example, a function f defined as a * b * c could be calculated as a * (b * c)
	where we calculate the multiplication of b and c first then multiply by a, or as (a * b) * c where we do the multiplication of a and b first,
	then multiply by c. Either way we decide to calculate the answer, we will get the same result as long as our constants, a, b and c, stay the
	same. This highlights how haskell functions resemble mathematical functions very heavily due to their common properties of: computing an input
	to an output, always getting the same result from the same inputs, and not worrying about the order in which calculations are performed. 
	
	
c) Explain what a higher-order function is (use examples to support your answer)
	
	A higher-order function is a function that can either take a function as an argument or produce a function as a result, or both.
	An example of a higher-order function would be [4]
	twice :: (a -> a) -> a -> a
	twice f x = f (f x)
	This function is a higher order function as what it does is it takes a function as an argument f and an argument x and applies the function f
	on the function f on x. For example, twice (+5) 8 would result in 18 because (+5) 8 applied onto the function twice would become (+5) (+5) 8
	which is equal 5+5+8 = 18. Higher-order functions like this are useful as they allow for more to be done with a function as you can let your 
	functions interact and work with one another. For example, the function filter takes a predicate as an argument and a list so filter odd [1,2,3,4,5]
	would return [1,3,5] as the list filters out all the elements that did not meet the predicate. Using this function in conjunction with another
	can enable you to be able to do a lot of different things, for example if you needed to make a list out of only the even elements of the list 
	multiplied by 2, you could do that:
	mult2even :: [Int] -> [Int]
	mult2even ns = (map (*2) (filter even ns))


References: [1] - Programming Language Design Concepts, David A. Watt P: Chapter 14.1
			[2] - Programming in Haskell, Graham Hutton: Chapter 1.3
			[3] - The Haskell School of Expression, Paul Hudak: Chapter 1.1
			[4] - Programming in Haskell, Graham Hutton: Chapter 7.1
-}




---- Part 2 ----
--- 2a ---


{-Description: takes an integer, n, and returns a string that can be displayed as asterisks of width n
  How it works: if n is less than or equal to one, an asterisk is returned, otherwise a an asterisk concatenated with the function width (n-1) is returned-}
width :: Int -> String
width n
	|n <= 1 = ("*")
	|otherwise = ("*") ++ (width (n-1))

{-Description: takes two integers, m and n, and returns a string that can be displayed as asterisks of width n and height m
 How it works: if m is less than or equal to one, a string of the function width n concatenated with a newline is returned. otherwise, a string of
the function width n concatenated a new line and the function height (m-1) n is returned.-}
height :: Int -> Int -> String
height m n
	|m <= 1 = (width n) ++ ("\n")
	|otherwise = (width n) ++ ("\n") ++ (height (m-1) n)

{-Description: takes an integer, a, and returns a list of integers from 1 to a backdown to 1 from a e.g. [1,2,..,a-1,a,a,a-1,..,2,1]
 How it works: returns the list [1..a] concatenated with the list [a,a-1..1]-}
makeList :: Int -> [Int]
makeList a = [1..a] ++ [a,a-1..1]

{-Description: takes two integers, m and n, and a list of integers, q, and returns a string of length n*c + a newline m times for every element c in the list q
 How it works: if the length of the list q is less than or equal to 1 the string of the functon height m (n*q) is returned, otherwise the string of the function
height m (n*q) concatenated with the function steps2 m n qs is returned-}
steps2 :: Int -> Int -> [Int] -> String
steps2 m n (q:qs)
	|length (q:qs) <= 1 = height m (n * q)
	|otherwise = height m (n * q) ++ (steps2 m n qs)

{-Description: takes three integers, m, n and p, and returns a string that can be displayed as asterisks of width n and height m repeated in steps of p and then
repeated back in the opposite way
 How it works: returns the string of the function steps2 m n (makeList p)-}
steps :: Int -> Int -> Int -> String	
steps m n p = steps2 m n (makeList p)




--- 2b ---


{-takes an integer y and list xs where y is the length of a line and xs is a list of where  asterisks should be put in a line of the pattern and outputs
a string. the function loops down from y to 1 where if y matches an element in the list, an asterisk is added to the string, otherwise a space is added
to the string. when the last asterisk is added so is a newline-}
design :: Int -> [Int] -> String
design y xs
	|length (xs) == 1 && y == 1 = "*\n"
	|y == last xs = ("*") ++ (design (y-1) (init xs))
	|otherwise = (" ") ++ (design (y-1) xs)
	
{-takes an integer a that determines the size of a pattern and an integer b that determines what line of the pattern string we're up to. It then returns
a list of integers with each integer representing the nth position in a line of a string where asterisks belong-}
makeList2 :: Int -> Int -> [Int]
makeList2 a b
	|b == 1 || b == a = [1..a]
	|(a `mod` 2 == 1) && (b-1 == a-b) = [1] ++ [b] ++ [a]
	|b+b <= a = [1] ++ [b] ++ [1+a-b] ++ [a]
	|otherwise = [1] ++ [1+a-b] ++ [b] ++ [a]

{-takes an integer n that determines the size of a pattern and an integer m to determine what line of the string of the pattern we're up to. It then
loops through from m to 1 adding each line of the pattern size n then finally returning it once m gets to 0-}
printList :: Int -> Int -> String
printList n m
	|n == 1 = design m (makeList2 m 1)
	|otherwise = (design m (makeList2 m n)) ++ (printList (n-1) m)

{-takes two integers m and n and returns a string equal to that of the function printList m m repeated n times-}
flagpattern :: Int -> Int -> String
flagpattern m n 
	|n <= 1 = printList m m
	|otherwise = (printList m m) ++ (flagpattern m (n-1))




---- Part 3 ----

{-Description: takes a char x and string ys and returns a string of either an asterisk or a string of the char x determined by if the char x is found
in the string ys or not. 
 How it works: if the string ys is empty a string of the char x is returned. if char x is not equal to the first element of the string ys then it returns
a string of the function itself of char x and the tail of the string ys. if char x is equal to the first element of ys it returns a string of an asterisk-} 
value :: Char -> String -> String
value x ys
	|ys == [] = [x]
	|x /= head (ys) = (value (x) (tail ys))
	|x == head (ys) = "*"

{-Description: takes a char x and string ys and returns a string equivalent to ys but without an element equal to that of the char x if there is one in the
string ys.
 How it works: takes a char x and a string ys and returns an empty string if ys is empty. if char x is not equal to the first element of the string ys it
returns a string of the first element of ys plus the result of the same functon char x and the tail of ys. if char x is equal to the first element of ys
then the function returns a string of the tail of ys-}
removedY :: Char -> String -> String
removedY x ys
	|ys == [] = []
	|x /= head (ys) = [head ys] ++ (removedY (x) (tail ys))
	|x == head (ys) = (tail ys)

{-Description: takes a string xs and another string ys and returns a string equivalent to xs but with asterisks replacing any common characters with the
string ys.
 How it works: returns an empty string if xs is empty. returns xs if ys is empty. otherwise returns a string of the function value of head xs and ys plus the
result of the the function newString of the tail of xs and the function removedY of the head of xs and ys-}
newString :: String -> String -> String
newString xs ys
	|xs == [] = []
	|ys == [] = xs
	|otherwise = (value (head xs) (ys)) ++ (newString (tail xs) (removedY (head xs) (ys)))

{-takes a string xs and returns a string identical but without any asterisks-}
removeAsterisks :: String -> String
removeAsterisks xs = [ x | x <- xs, not (x `elem` "*")]
	
{-takes a string xs and returns a string identical but without any spaces-}
removeSpaces :: String -> String
removeSpaces xs = [ x | x <- xs, not (x `elem` " ")]

{-given a string returns an integer 1,2,3 or 4 by adding 1 to the modulus 4 of the length of the string.-}
lastLPHI :: String -> Int
lastLPHI (x:xs) 
	|length xs == 0 = 1
	|otherwise = ((length xs) `mod` 4) + 1
	
{-takes an integer, a, of value 1 to 4 and returns a statement representing compatibility-}
compatValue :: Int -> String
compatValue a
	|a == 1 = " loves "
	|a == 2 = " is physically attracted to "
	|a == 3 = " hates "
	|a == 4 = " is indifferent to "
	
{-takes two strings representing people's names and returns a string representing their compatibility where if the strings are identical their compatibility
is indifferent to one another. otherwise a formula is applied where common characters are eliminated from the string and a pattern going from lphi is applied 
to each remaining character of the string. whatever the last letter is determines the compatibility of the the pair.-}
compatibility :: String -> String -> String
compatibility xs ys
	|xs == ys = xs ++ " is indifferent to " ++ ys ++ " and " ++ ys ++ " is indifferent to " ++ xs
	|otherwise = xs ++ (compatValue(lastLPHI(removeAsterisks(newString (removeSpaces xs) (removeSpaces ys))))) ++ ys ++ " and " ++ ys ++ (compatValue(lastLPHI(removeAsterisks(newString (removeSpaces ys) (removeSpaces xs))))) ++ xs




---- Part 4 ----


{-takes an argument [a] xs and a y and returns an integer displaying how many elements are in xs before an element of value y occurs-}
findNum :: (Eq a, Num b) => [a] -> a -> b
findNum xs y 
	|xs == [] = 0
	|head xs == y = 0
	|head xs /= y = 1 + (findNum (tail xs) y)
	
{-takes an argument [a] xs and a y and returns an integer displaying how many times y occurs in xs-}
findOcc :: (Eq a, Num b) => [a] -> a -> b	
findOcc xs y
	|xs == [] = 0
	|head xs == y = 1 + (findOcc (tail xs) y)
	|head xs /= y = 0 + (findOcc (tail xs) y)
	
{-takes an argument [a] xs and a y and returns a list of integers. if findOcc xs y is equal to 0 it returns just the list findNum xs y as there are no more instances
of y to worry about. if findOcc xs y is greater than or equal to 1 then it returns a list findNum xs y plus the function itself of (drop ((findNum xs y) + 1) xs) y
as it stills need to deal with the remaining of occurences of y-}	
createList :: (Eq a, Num b) => [a] -> a -> [b]
createList xs y
	|findOcc xs y == 0 = [findNum xs y] 
	|findOcc xs y >= 1 = [findNum xs y] ++ (createList (drop ((findNum xs y) + 1) xs) y)

{-takes an argument [a] xs and a y and returns a list of integers representing the number of elements in each segment of the list xs split up at each occurence of y.
the function filters out zeroes becuase the function createList can return zeroes if there are two consecutive occurences of y in xs-}
lsplit :: (Num b, Eq a) => [a] -> a -> [b]
lsplit xs y = filter (/=0) (createList xs y)
