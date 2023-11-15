-- // some sillies about higher order functions

-- // for toLower in insensitive
import Data.Char

-- // higher order functions are functions that take functions as parameters,
-- /  like map and fold

-- // kinda a naive quick_sort, but it's ok
quick_sort :: (Ord a) => [a] -> [a]
quick_sort [] = []
quick_sort (value:list) =
    (quick_sort less) ++ (value : same) ++ (quick_sort more)
    where
        less = filter (< value) list
        same = filter (== value) list
        more = filter (> value) list

-- // almost all basic data types in haskell are members of Ord, which is for
-- /  ordering tests, where as Eq is for equality tests. Ord defines the
-- /  "natural" ordering of a givern type, and provides the compare function
-- /  compate :: (Ord a) => a -> a -> Ordering
-- /  the output Ordering, is LT if a1 < a2, EQ if a1 == a2, and GT if a1 > a2.
-- /  Really, (<) (==) and (>) can be seen as shortcuts to compare

-- // a more general quick sort, where we take a comparing function
-- // if you think about this in the concept of currying, this is a function
-- /  that returns functions of type quick_sort, after taking a function to use
-- /  for the comparison
quick_sort' :: (Ord a) => (a -> a -> Ordering) -> [a] -> [a]
quick_sort' _ [] = []
quick_sort' compare (first : list) =
    (quick_sort' compare less) ++ (first : same) ++ (quick_sort' compare more)
        where
            less = filter (\ y -> y `compare` first == LT) list
            same = filter (\ y -> y `compare` first == EQ) list
            more = filter (\ y -> y `compare` first == GT) list

testing_list = ["I", "have", "a", "thing", "for", "Linux"]

-- // with this version, we can do all kinds of things, like sort backwards by
-- /  giving a compare function that gives the opposite results as the original
descending x y = compare y x
insensitive x y = (map toLower x) `compare` (map toLower y)

-- // an implementation of a for loop
for :: a -> (a -> Bool) -> (a -> a) -> (a -> IO ()) -> IO ()
-- for i p f job =
--     if p i
--         then do job i
--                 for (f i) p f job
--         else

for i p f job
    | p i = do job i
               for (f i) p f job
    -- // the return here is to give back an IO ()
    | otherwise = return ()

-- // I don't think this works, because there is no Show (IO ()) ??
-- print_list :: (Show a, Num a) => [a] -> [IO ()]
-- print_list list = map (print) list

 -- // had to look this one up lol
sequence_IO :: [IO a] -> IO [a]
sequence_IO [] = return []
sequence_IO (action:actions) = do
    result <- action
    results <- sequence_IO actions
    return (result : results)

-- sequence_IO actions = return (go actions ())
--     where
--         go (action:actions) acc = do
--             result <- action
--             if actions == []
--                 then result
--                 else result ++ go actions acc

-- // Some cool functions in prelude
-- /  - flip: takes a function of two args and returns the same func with the
-- /  args swapped. Ex (flip (/)) 3 1 (flip map) [1, 2, 3] (*2)
--
-- /  - (.): takes two functions and returns a new function which applies the
-- /  second function to the argument and then the first
-- /  Ex my_inits :: [a] -> [[a]] my_inits = map reverse . scanl (flip (:)) []
--
-- /  - ($): apply the function given to the second argument.
-- /  Ex (head $ "abc") == (head "abc). The thing that makes it cool is that it
-- /  has low precedence, meaning it'll be used after regular function
-- /  precedence, so we can avoid a lot of parentheses nesting in complex funcs.
-- /  We can also do fun stuff like map ($ 2) [(2*), (4*), (8*)], because it's
-- /  just a function that happens to apply functions
--
-- /  - uncurry: takes a functio of two args and converts it to a function that
-- /  takes a pair as it's only argument. Ex uncurry ($) (reverse, "stressed")
-- /  which equals "desserts". You can also use the curry function to undo
-- /  uncurry, but since all functions are automatically curried, it's less
-- /  common than uncurry
--
-- /  - id: the identity function, it returns it's argument unchanged
--
-- /  - const: takes two arguments, discards the second, and returns the first

my_curry :: ((a, b) -> c) -> a -> b -> c
my_curry f x y = f (x, y)

my_uncurry :: (a -> b -> c) -> (a, b) -> c
my_uncurry f (x, y) = f x y

my_const :: a -> b -> a
my_const x y = x

-- // very hard problem
-- // use foldr to implement foldl
-- // foldr f acc (a:b:c:[]) = f a (f b (f c acc))
-- // foldl f acc (a:b:c:[]) = f (f (f acc a) b) c)

-- // boring answer
-- // sometimes the boring answer is faster than the cool answer
sad_foldl :: (b -> a -> b) -> b -> [a] -> b
sad_foldl f acc xs = foldr (flip f) acc (reverse xs)
-- // more point free
-- my_foldl f acc = foldr (flip f) acc . reverse

 -- // think about how you would go about composing all functions in a list
-- my_foldl :: (b -> a -> b) -> b -> [a] -> b
-- my_foldl f acc xs =

compose_list :: a -> [a -> a] -> a
-- // at the end of the foldr, we give val as the input to the weird function
-- /  it produces. Don't really need the ($) there but it helps to understand
-- compose_list val = foldr (.) id fs $ val
-- // super point free haha
compose_list val = ($ val) . foldr (.) id

-- // this is kinda sick
-- // this is 2* as slow as sad_foldl lol
cool_foldl :: (b -> a -> b) -> b -> [a] -> b
-- cool_foldl f acc xs = foldr (flip (.)) id (map (flip f) xs) acc
-- // excessively point free haha
cool_foldl f acc = ($ acc) . foldr (flip (.)) id . map (flip f)
-- // more understandable
-- cool_foldl f acc xs = foldr (\ g h -> h . g) id (map step xs) $ acc
--     where
--         step x = \ acc -> f acc x

