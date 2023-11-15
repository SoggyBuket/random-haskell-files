-- // some control structures

describe_letter :: Char -> String
-- // basic if statement
-- describe_letter c =
--     if c >= 'a' && c <= 'z'
--         then "Lower case"
--         else if c >= 'A' && c <= 'Z'
--             then "Upper case"
--             else "Not an ASCII character"

-- // using guards is a little better looking than if statements
describe_letter c
    | c >= 'a' && c <= 'z' = "LOWER CASE"
    | c >= 'A' && c <= 'Z' = "upper case"
    | otherwise            = "Not an ASCII character"

-- // because if else constructs are expressions, we can put them in places
-- /  such as this
silly x y = (if x == 0 then 1 else sin x / x) * y

-- // you can avoid excessive effort by using || and && since they only
-- /  evaluate the second argument if it needs to be evaluated. They can also
-- /  be used to avoid runtime errors:
-- // TODO idk why this doesn't
last_non_zero :: (Eq a, Num a) => [a] -> Maybe a
last_non_zero a = go a (length a-1)
    where
        go a l
            | l >= 0 && a !! l == 0 = go a (l-1)
            | l < 0 = Nothing
            | otherwise = Just (a !! l)

-- // case statement is the same as this
-- silly_2 0 = 18
-- silly_2 1 = 15
-- silly_2 2 = 12
-- silly_2 x = 12 - x

silly_2 x =
    case x of
        0 -> 18
        1 -> 15
        2 -> 12
        _ -> 12 - x

-- // can be used for binding just like the piece-wise function definition too
describe_string :: String -> String
describe_string str =
    case str of
        (x:xs) -> "The first character of the string is: " ++ [x] ++ "; and" ++
                  "there are " ++ show (length xs) ++ " more characters in it."
        []     -> "This is an empty string."

data Colour = Black | White | RGB Int Int Int

-- // just like if expressions case can go anywhere an expression goes
describe_black_or_white :: Colour -> String
describe_black_or_white colour =
    "This colour is"
    ++ case colour of
        Black           -> " black"
        White           -> " white"
        RGB 0 0 0       -> " black"
        RGB 255 255 255 -> " white"
        _               -> "... somethin idk"
    ++ ", yeah?"

do_guessing num = do
    putStrLn "Enter your guess: "
    guess <- getLine
    case compare (read guess) num of
        -- // need the do in the first two since we want to print before we
        -- /  call do_guessing
        LT -> do putStrLn "Too low!"
                 do_guessing num
        GT -> do putStrLn "Too high!"
                 do_guessing num
        EQ -> putStrLn "You Win!"


main = do x <- get_x
          putStrLn x

get_x =
    do return "My Shangri-La"
       return "beneath"
       return "the summer moon"
       return "I will"
       return "return"
       return "again"
