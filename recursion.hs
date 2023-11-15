import Data.List
-- // some little things on recursion

factorial :: Int -> Int
-- // this definition order is important, since Haskell looks from the top down
-- /  when it comes to function definitions
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- // non explicitly recursive
-- factorial n = product [1..n]

-- // the closest thing to a loop in haskell
go_factorial :: Int -> Int
-- // go is an auxiliary function which actually performs the factorial
-- /  calculation, and it takes an extra argument res which is an accumulating
-- /  parameter
-- // call go n 1
go_factorial n = go n 1
    where
        -- // define go
        go n res
            | n > 1     = go (n - 1) (res * n)
            | otherwise = res

mult :: Int -> Int -> Int
-- // anything multiplied by zero is zero
mult _ 0 = 0
-- // multiply by one less and add a copy
mult n m = (mult n (m - 1)) + n

go_mult :: Int -> Int -> Int
go_mult n m = go n m 0
    where
        go n m res
            | m >= 1    = go n (m - 1) (res + n)
            | otherwise = res

recursive_power :: Int -> Int -> Int
recursive_power _ 0 = 1
recursive_power x y = x * recursive_power x (y - 1)

plus_one :: Int -> Int
plus_one x = x + 1

recursive_add :: Int -> Int -> Int
recursive_add x 0 = x
recursive_add x y = plus_one (recursive_add x (y - 1))

go_recursive_add :: Int -> Int -> Int
go_recursive_add x y = go x y x
    where
        go x y res
            | y > 0     = go x (y - 1) (plus_one res)
            | otherwise = res


-- // I don't think there's a good alternative to not iterating here
go_log2 :: Int -> Int
go_log2 x = go x 0
    where
        go x res
            | 2 ^ next_res <= x = go x next_res
            | otherwise         = res
                where
                    next_res = res + 1

recursive_length :: [a] -> Int
-- // base case
recursive_length []     = 0
-- // remove the first element in a list and add one
-- // xs here meaning the plural form of x
recursive_length (x:xs) = 1 + recursive_length xs

concatination :: [a] -> [a] -> [a]
concatination [] ys = ys
concatination (x:xs) ys = x : (concatination xs ys)

fill_list :: Int -> a -> [a]
fill_list 0 x = []
fill_list n x = x : fill_list (n - 1) x

at_index :: [a] -> Int -> a
-- // would probably be good to add this case
-- at_index [] n =
at_index (l:ls) 0 = l
at_index (l:ls) n = at_index ls (n - 1)

-- // same as zip
slam :: [a] -> [b] -> [(a, b)]
slam [] b = []
slam a [] = []
slam (a:as) (b:bs) = (a, b) : slam as bs

-- multiply_list :: Int -> [Int] -> [Int]
-- multiply_list _ [] = []
-- multiply_list m (n:ns) = (m * n) : multiply_list m ns

double_list :: [Int] -> [Int]
-- double_list xs = multiply_list 2 xs
double_list = multiply_list 2

take_int :: Int -> [Int] -> [Int]
take_int 0 l = []
take_int n [] = []
take_int n (l:ls) = l : take_int (n - 1) ls

drop_int :: Int -> [Int] -> [Int]
drop_int 0 l = l
drop_int n [] = []
drop_int n (l:ls) = drop_int (n - 1) ls

sum_int :: [Int] -> Int
sum_int [] = 0
sum_int (l:ls) = l + sum_int ls

-- // there's a complex way of doing this that I don't want to do
-- scan_sum :: [Int] -> [Int]
-- scan_sum [] = []
-- scan_sum (l:ls) = l : l + scan_sum ls

-- // a less abstract version of map
apply_to_ints :: (Int -> Int) -> [Int] -> [Int]
apply_to_ints _ [] = []
apply_to_ints f (n:ns) = (f n) : apply_to_ints f ns

multiply_list :: Int -> [Int] -> [Int]
-- // can also be thought of as
-- /  multiply_list :: Int -> ([Int] -> [Int])
-- /  and (*) can be thought of as this
-- /  (*) :: Num a => a -> (a -> a)
-- /  so, functions with multiple arguments, take the first passed variable and
-- /  return a function with that variable not needed to be passed anymore
-- /  (since we already have it), and that new function is used on the second
-- /  argument. So, if we take (*) 5 7, (*) turns into a new function like
-- /  5* :: Num a => a -> a, which takes the 7 from before, and returns the
-- /  result of the two. Very weird and different, I know
-- // all of these are the same
-- multiply_list num = apply_to_ints ((*) num)
-- multiply_list num = apply_to_ints (num *)
-- multiply_list num list = apply_to_ints ((*) num) list
-- multiply_list num list = apply_to_ints (num *) list

-- // better version using map
-- /  map :: (a -> b) -> [a] -> [b]
-- /  can also be thought of as
-- /  map :: (a -> b) -> ([a] -> [b])
-- /  and multiply_list
-- /  multiply_list :: Int -> ([Int] -> [Int])
-- /  If you can tell, when multiply list takes an Int, it returns a function
-- /  of type [Int] -> [Int], and when map gets a function of type (a -> b), it
-- /  returns a function of type [a] -> [b], so the return types of these two
-- /  in this case are the same, and they both become functions that want a
-- /  list of ints
multiply_list num = map ((*) num)
-- multiply_list num list = map ((*) num) list

heads :: [[a]] -> [a]
heads = map head

negate_list :: [Int] -> [Int]
negate_list = map ((*) (-1))

divisors p = [ f | f <- [1..p], p `mod` f == 0 ]
list_divisors :: [Int] -> [[Int]]
list_divisors = map divisors

negate_list_divisors :: [[Int]] -> [[Int]]
negate_list_divisors = map negate_list

compress_rle :: [Char] -> (Int, Char)
compress_rle (char:group_str) = ((length group_str) + 1, char)
encode_rle :: [Char] -> [(Int, Char)]
encode_rle str = map (compress_rle) (group str)

uncompress_rle :: (Int, Char) -> [Char]
uncompress_rle (0, _) = []
uncompress_rle (num, char) = char : uncompress_rle (num - 1, char)
decode_rle :: [(Int, Char)] -> [Char]
decode_rle rle = concat (map (uncompress_rle) rle)

last_in_list :: [a] -> a
last_in_list list = list !! ((length list) - 1)

except_last :: [a] -> [a]
-- except_last (_:[]) = []
except_last [_] = []
except_last [] = []
except_last (a:list) = a : except_last list

-- // folds
-- // these are like map where they take a function and a list, but they fold
-- /  the list down into a result value
-- // examples include
-- /  sum :: [Int] -> Int
-- /  sum [] = 0
-- /  sum (x:xs) = x + sum xs
-- /  product :: [Int] -> Int
-- /  product [] = 1
-- /  product (x:xs) = x * product xs
-- /  These all fold down lists
-- // These are the fold functions defined in Prelude
-- /  foldr :: (a -> b -> b) -> b -> [a] -> b
-- /  foldr f acc []     = acc
-- /  foldr f acc (x:xs) = f x (foldr f acc xs)
-- /  In 'sum', 'f' is (+) and 'acc' is 0. In 'concat', 'f' is (++) and 'acc'
-- /  is []
-- /  foldl :: (b -> a -> b) -> b -> [a] -> b
-- /  foldl f acc []     = acc
-- /  foldl f acc (x:xs) = foldl f (f acc x) xs
-- /  foldl' (fold L prime) calls f immediatly at each step so you don't run
-- /  out of memory, so use that

echoes :: [Int] -> [Int]
-- // the '\' is a lambda function, which is an unnamed function that we won't
-- /  use again anywhere else
-- // infinite lists work for this version
echoes = foldr (\ x xs -> (replicate x x) ++ xs) []
-- /  but not for this version because of how foldl works
-- echoes = foldl (\ x xs -> xs ++ (replicate x x)) []

my_and :: [Bool] -> Bool
-- my_and [b] = b
-- my_and (b:bs) = b && my_and bs
my_and = foldr1 (&&)

my_or :: [Bool] -> Bool
-- my_or [b] = b
-- my_or (b:bs) = b || my_or bs
my_or = foldr1 (||)

list_maximum :: Ord a => [a] -> a
list_maximum = foldr1 (\ x1 x2 -> (max x1 x2))

list_minimum :: Ord a => [a] -> a
list_minimum = foldr1 (\ x1 x2 -> (min x1 x2))

list_reverse :: [a] -> [a]
-- // this works because at the end of the recursion, xs is equal to the
-- /  accumulator, which is an empty list
list_reverse = foldl (\ x xs -> xs : x) []

my_scanr :: (a -> b -> b) -> b -> [a] -> [b]
-- my_scanr f acc [] = [acc]
-- my_scanr f acc (x:xs) = (f x (head partial_result)) : partial_result
--     where partial_result = my_scanr f acc xs

-- // stole this from the answers lol
-- my_scanr f acc xs = foldr (f x (head xs)):xs) [acc] xs
my_scanr f acc xs = foldr f' [acc] xs
    where f' x xs = (f x (head xs)) : xs

-- my_scanr1 :: (a -> a -> a) -> [a] -> [a]
-- my_scanr1 f [] = []
-- my_scanr1 f [a] = [a]
-- my_scanr1 f (x:xs) = my_scanr f xs : xs
-- my_scanr1 f = foldr1 (\ x xs -> f x

my_scanl :: (b -> a -> b) -> b -> [a] -> [b]
-- my_scanl f acc [] = [acc]
-- // you use the accumulator to add x to and use recursivly through
-- // this is a kinda weird function and I don't know a use case yet
-- my_scanl f acc (x:xs) = acc : my_scanl f (f acc x) xs
my_scanl f acc xs = (reverse . foldl f' [acc]) xs
    where f' xs x = (f (head xs) x):xs

-- // filtering
is_even :: Int -> Bool
is_even n = (mod n 2) == 0

retain_evens :: [Int] -> [Int]
-- // if statement way
-- retain_evens [] = []
-- retain_evens (n:ns) =
--     if (mod n 2) == 0
--        then n : (retain_evens ns)
--        else retain_evens ns

-- // better way
-- retain_evens = filter (\ n -> (mod n 2) == 0)

-- // list comprehensions
-- // these are kinda like map + filter
-- // starting from the middle, draw each element of es to the variable n, and
-- /  test the boolean condition to the right of it. If that bool condition is
-- /  true, append n to the list being created
retain_evens es = [n | n <- es, is_even n]

-- // you can have as many conditions as you want (even zero)
retain_large_evens :: [Int] -> [Int]
retain_large_evens es = [n | n <- es, is_even n, n > 100]

evens_minus_one :: [Int] -> [Int]
evens_minus_one es = [n - 1 | n <- es, is_even n]

first_for_even_ys :: [(Int, Int)] -> [Int]
-- first_for_even_ys points = [
--     fst point | point <- points, is_even (snd point)
-- ]
first_for_even_ys points = [x | (x, y) <- points, is_even y]

all_pairs :: [(Int, Int)]
all_pairs = [(x, y) | x <- [1..4], y <- [5..8]]

some_pairs :: [(Int, Int)]
some_pairs = [(x, y) | x <- [1..4], y <- [5..8], x + y > 7]

list_comprehension_map :: (a -> b) -> [a] -> [b]
list_comprehension_map f xs = [f x | x <- xs]

list_comprehension_filter :: (a -> Bool) -> [a] -> [a]
list_comprehension_filter f xs = [x | x <- xs, f x]

