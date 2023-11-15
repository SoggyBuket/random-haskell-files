-- // a cool case in defining folds and maps to weird data types

data Tree a = Leaf a | Branch (Tree a) (Treea a) deriving (Show)

-- // map :: (a -> b) -> [a] -> [b]
-- /  map _ [] = []
-- /  map f (x:xs) = f x : map f xs

-- tree_map :: (a -> b) -> Tree a -> Tree b
-- tree_map f (Leaf x) = Leaf (f x)
-- tree_map f (Branch left right) = Branch (tree_map f left) (tree_map f right)

-- // this comes with the idea that "tree_map f :: Tree a -> Tree b", which is
-- /  actually really cool
tree_map :: (a -> b) -> Tree a -> Tree b
tree_map f = g where
    g (Leaf x) = Leaf (f x)
    g (Branch left right) = Branch (g left) (g right)

-- // foldr :: (a -> b -> b) -> b -> [a] -> b
-- /  foldr f acc [] = acc
-- /  foldr f acc (x:xs) = f x (foldr f acc xs)

tree_fold :: (b -> b -> b) -> (a -> b) -> Tree a -> b
tree_fold fbranch fleaf = g where
    g (Leaf x) = fleaf x
    g (Branch left right) = fbranch (g left) (g right)

-- // test functions
tree1 :: Tree Int
tree1 =
    Branch
        (Branch
            (Branch
                (Leaf 1)
                (Branch (Leaf 2) (Leaf 3)))
            (Branch
                (Leaf 4)
                (Branch (Leaf 5) (Leaf 6))))
        (Branch
            (Branch (Leaf 7) (Leaf 8))
            (Leaf 9))

double_tree = tree_map (2*)
sum_tree = tree_fold (+) id
fringe_tree = tree_fold (++) (: [])

-- // weird datatype
data Weird a b = First a
               | Second b
               | Third [(a, b)]
               | Fourth (Weird a b)

-- // weird takes 2 data types, so we need to take a function that acts on "a"
-- /  and a function that acts on "b"
weird_map :: (a -> c) -> (b -> d) -> Weird a b -> Weird c d
weird_map fa fb = g
    where
        g (First x)     = First (fa x)
        g (Second y)    = Second (fb y)
        -- // we wanted to try and call g recursively, but since it can't call
        -- /  straight on the tail of the list that it's pattern matching, we
        -- /  had to wrap it in a lot, where map just acts on lists so it's all
        -- /  cool
        g (Third z)     = Third (map (\(x, y) -> (fa x, fb y)) z)
        -- // can also write it as a list comprehension
        -- g (Third z)     = Third [ (fa x, fb y) | (x, y) <- z ]
        g (Fourth w)    = Fourth (g w)

-- // nice and long haha
weird_fold :: (a -> c) -> (b -> c) -> ([(a, b)] -> c) -> (c -> c) -> Weird a b -> c
weird_fold f1 f2 f3 f4 = g
    where
        g (First x)     = f1 x
        g (Second y)    = f2 y
        g (Third z)     = f3 z
        g (Fourth w)    = f4 (g w)
