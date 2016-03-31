-- PA5.hs
-- Joshua Guerrero
-- March 27 2016
--
-- CS 331 Spring 2016
-- Assignment 5 Exercise B

module PA5 where

-- =======================
-- == Collatz Functions ==

-- collatzCounts
-- List of ALL collatzCount where k is the number of iterations
-- it takes to get from k+1 to 1 using the Collatz function
collatzCounts = map collatz [0..]

-- collatz
-- Passes n to collatzAlgo which returns number of
-- times it takes to get through collatzCount
-- n = number given
collatz n
	| (n == 0)		= 0		-- no application
	| otherwise		= collatzAlgo (n+1) 0 --passes value and starting count

-- collatzAlgo
-- n = value calculated
-- count = number of times calculated
-- returns n if a == 1 or runs collatzAlgo (based on even or odd)
collatzAlgo n count
	| (n == 1)			= count 	-- when 1 is reached, stop
	| (mod n 2) == 0	= collatzAlgo (div n 2) (count+1) -- even
	| otherwise			= collatzAlgo ((3*n)+1) (count+1) -- odd


-- ========================
-- == findList Functions ==

-- findList (Function)
-- Takes two lists of sames type and returns a 'Maybe Int'
findList a b
	| (length a <= 0) 			= Just 0
	| (length a > length b)	= Nothing
	-- if number of items found in comparison match length of a, it's a sublist
	| (compareLists a b 0 0 0) == (length a)	
		= Just(findFirstIndex a b 0 0 0)
	| otherwise					= Nothing


-- compareLists
-- a = sublist				,index_A = index of a
-- b = List compared to 	,index_B = index of b
-- count = number of matches
compareLists a b index_A index_B count
	-- if end of list b, or if index of a exceeds list a
	-- then return number of times counted
	| ((index_A >= (length a)) || (index_B >= (length b)))	= count
	-- if index elements of a and b match, go to next item in both lists
	-- increase count by 1
	| (a !! index_A) == (b !! index_B)	
		= compareLists a b (index_A+1) (index_B+1) (count+1)
	-- if index elements do not match, go to next item of main list
	| (a !! index_A) /= (b !! index_B)
		= compareLists a b (0) (index_B-count+1) (0)
	| otherwise 		= 0


-- findFirstIndex
-- compare first element of list with elements of list b
-- return index of first element of list B
-- to match first element of list A
--
-- a = subset list
-- b = list compared to
-- n = index
findFirstIndex a b index_A index_B count
	| index_B > length b			= -1
	| count == length a 			= (index_B-count)
	| ( (a !! index_A)  == (b !! index_B))	
		= findFirstIndex a b (index_A+1) (index_B+1) (count+1)
	| (a !! index_A ) /= (b !! index_B)
		= findFirstIndex a b 0 (index_B-count+1) 0


-- ========================
-- == infix ## Functions ==

-- Infix operator ##
-- Takes two lists of same type and returns
-- number of matching indices
a ## b = infixListCompare a b 0 0

infixListCompare a b index count
	|	index >= (length a) || index >= (length b)	= count
	|	a !! index == b !! index
		= infixListCompare a b (index+1) (count+1)
	|	otherwise	= infixListCompare a b (index+1) count

-- ==========================
-- == filterAB Functions ==

-- filterAB (Function)
-- Takes a boolean function and two lists.
-- Returns a list of all items in the second list
-- for whcih the corresponding item in the first list
-- makes the boolean function true

-- Empty lists
filterAB booleanFunction [] [] = []

-- Empty list, list with items
filterAB booleanFunction [] (x:xs) = []

-- List with items, empty list
filterAB booleanFunction (x:xs) [] = []

-- Two lists with items in them
filterAB booleanFunction (x:xs) (y:ys)
	| booleanFunction x = y:rest
	| otherwise		= rest where
		rest = filterAB booleanFunction xs ys

-- ==========================
-- == sumEvenOdd Functions ==

-- sumEvenOdd (Function)
-- Takes a list of numbers and returns a tuple of two numbers
-- The sum of the even-index items in the given list,
-- and the sum of the odd-index items in the given list.
sumEvenOdd list = evenOdd list 0 [] []

-- evenOdd
-- Used by sumEvenOdd to pass index and two empty lists
evenOdd list index retListA retListB 
	-- 
	| index >= (length list)	= (foldl (+) 0 retListB, foldl (+) 0 retListA)
	-- even elements, (i.e. every other)
	| ((mod index 2) == 0)		
		= evenOdd list (index+1) retListA (list !! index : retListB)
	-- odd elements
	| otherwise					
		= evenOdd list (index+1) (list !! index : retListA) retListB
