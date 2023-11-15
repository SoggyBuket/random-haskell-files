-- // More info on let and where, and functions in general

-- // could write it like this
-- add_str :: Float -> String -> Float
-- add_str x str = x + read str

-- sum_str :: [String] -> Float
-- sum_str = foldl add_str 0.0

-- // this is pretty cool
-- sum_str :: [String] -> Float
-- sum_str =
--     let add_str x str = x + read str
--      in foldl add_str 0.0

-- // this is also pretty cool
-- sum_str :: [String] -> Float
-- sum_str = foldl add_str 0.0
--     where add_str x str = x + read str

-- // using lambda functions is very cool
sum_str :: [String] -> Float
sum_str = foldl (\ x str -> x + read str) 0.0

-- // the main difference between those is that let...in is an expression,
-- /  but the where clause is like a guard, and not an expression
silly x =
    if x > 0
        then (let lsq = (log x) ^ 2 in tan lsq) * sin x
        else 0

-- // but you can put a where clause in a case
data Color = Black | White | RGB Int Int Int
describe_color :: Color -> String
describe_color color =
    "This color "
    ++ case color of
           Black -> "is black"
           White -> "is white"
           RGB r g b -> " has an average of the components of " ++ show avg
              -- // if we want to use a regular prefix function as an infix,
              -- /  we put the back ticks around it
              where avg = (r + g + b) `div` 3
    ++ ", yeah?"

-- // can't really use a let...in here without it being worse than where
do_stuff :: Int -> String
do_stuff x
    | x < 3     = report "less than three"
    | otherwise = report "normal"
    where
        report y = "the input is " ++ y

-- // cool use of sections
multiply_list :: Int -> [Int] -> [Int]
multiply_list m = map (m*)

-- // can even use sections with these
element :: (Eq a) => a -> [a] -> Bool
x `element` xs = any (==x) xs

