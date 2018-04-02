import Data.Map (fromListWith, toList)
import Text.Printf

-- frequency function to compare signatures
freq:: (Ord x) => [x] -> [(x, Int)]
freq xs = toList (fromListWith (+) [(x, 1) | x <- xs])

-- no comment
a0r :: [Int] -> Int -> [Int]
a0r li n = [l+n | l <- li]

-- arities of functions created by applying an n-ary function to functions of
-- arities i \in li
a0l :: Int -> [Int] -> [Int]
a0l 0 li = [0]
a0l 1 li = li
a0l n li = foldl (++) [] [a0r li i | i <- a0l (n-1) li]

-- arities of functions created by applying n-ary functions of arities n \in
-- lil to functions of arities i \in lir
alr :: [Int] -> [Int] -> [Int]
alr [] lir = []
alr lil [] = lil
alr (l:ls) lir = (a0l l lir) ++ (alr ls lir)

-- arities of mth-order functions created from 0th-order functions of arities
-- in li (recurse on the function)
ah :: Int -> [Int] -> [Int]
ah 0 li = li
ah m li = alr (ah (m-1) li) li

-- arities of mth-order functions created from 0th-order functions of arities
-- in li (recurse on the arguments)
at :: Int -> [Int] -> [Int]
at 0 li = li
at m li = alr li (at (m-1) li)

-- verify equality in a few cases
compareht m li = do
	let h = ah m li
	let t = at m li
	printf "\n***\n\n"
	printf "ah %d %s: size = %d\n" m (show li) (length h)
	printf "at %d %s: size = %d\n" m (show li) (length t)
	printf "max arity = %d\n" ((foldl max 0 li) ^ (m+1))

	let sh = freq h
	let st = freq t
	printf "same sig: "
	print (sh == st)
	printf "sig ah = %s\n" (show sh)

main = do  
	compareht 2 [0,1,2]
	compareht 3 [0,1,2]
	compareht 2 [0,1,2,2]
	compareht 2 [0,3]
	compareht 1 [0,1,2,3]
	compareht 2 [0,1,3]
