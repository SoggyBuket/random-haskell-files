-- ;; might be a good way to show a me comment
-- r = 5.0
-- area = pi * r ^ 2

{- block comment -}

-- :: a better block comment. Can just add a "}" at the end of the first part
-- :  to uncomment it all
{--
silly :: String
silly = "silly"
--}

-- ;; function definition!
-- ;; <name> <params> = <function body>
-- ;; you can define the params with () but by convention they are omitted
area r = pi * r ^ 2
-- ;; described in math, that would be A(r) = pi * r^2

-- ;; (area 5) * 3
-- area 5 * 3
-- ;; ex: area 15
-- area (5 * 3)

-- :: more functions :)
double x    = 2 * x
-- ;; doing something like "quadruple x = double double x" will give an error
-- ;  that says you're giving the function double 2 variables
quadruple x = double (double x)
square x    = x * x
half x      = x / 2

-- ;; multiple args
area_rect l w = l * w
area_tri  b h = (b * h) / 2
area_square s = area_rect s s

-- ;; "where" defines local variables, like a math function would
heron_formula a b c = sqrt (s * (s - a) * (s - b) * (s - c))
    where
        s = (a + b + c) / 2

area_triangle_trig a b c = c * height / 2
    where
        cosa    = (b ^ 2 + c ^ 2 - a ^ 2) - (2 * b * c)
        sina    = sqrt (1 - cosa ^ 2)
        height  = b * sina

-- // what's interesting here is that the '==' is an actual function, it's just
-- /  written to be an 'infix operator', and is placed between it's arguments.
-- /  You can also write '(==) a b' and it would be like calling a regular func
is_equal a b = a == b

-- // and infix operator function
x /= y = not (x == y)

-- // example of using 'guards'
-- // they kinda act like piecewise functions
absolute x
    | x < 0     = -x
    | otherwise = x

real_solution a b c
    | disc > 0  = 2
    | disc == 0 = 1
    | otherwise = 0
        where
            disc = b^2 - 4*a*c

-- // this a type definition for xor
-- // the first Bool is p, the second is q and the final is the return
xor :: Bool -> Bool -> Bool
xor p q = (p || q) && not (p && q)

-- // lists
-- // every entry in a list must be of the same type. A tricky part with that
-- /  is that a list of things and the thing in the list are different types.
-- /  That means you can't have numbers = [[1, 2], 3, 4, 5]
-- // same as numbers = 1:2:3:4:5:[]
numbers = [1, 2, 3, 4, 5]
-- // adds stuff to the beginning of numbers and returns a new list
numbers_two = 5:4:3:2:1:0:numbers

-- // tuples
-- // entries can be of different type, they are of fixed size
num_and_char = ('h', 4, "ello")

-- // accessors
-- // these are all super basic, and can be problematic for empty lists
point = (2, 5)
-- // get the first value in a 2-tuple
point_x = fst point
-- // get the second value in a 2-tuple
point_y = snd point

-- // get the first value in numbers
first_number = head numbers
-- // get everything but the first value
other_numbers = tail numbers

-- // types
-- // some fun things with types over here
-- // the type of the function (+) is: (x) :: (Num a) => a -> a -> a
-- /  where a can be any type normally, but here we restrict it to a Num, which
-- /  is a typeclass that contains all types that are reguarded as numbers
-- // the type of (/) is: (/) :: (Fractional a) => a -> a -> a
-- /  and the type of the function length :: (Foldable t) => t a -> Int
-- /  so with that, if you did something like, 4 / length(numbers), you would
-- /  get an error because the output of length is not polymorphic so it can't
-- /  be implicitly converted to a Double. To get around this, theres a func
-- /  called fromIntegral :: (Integral a, Num b) => a -> b. If you couldn't
-- /  guess, it takes a type like Int and converts it to a polymorphic value

-- // pattern matching
my_signum_if x =
    if x < 0
       then -1
    else if x > 0
        then 1
    else 0

my_signum_guard x =
    | x < 0     = -1
    | x > 0     = 1
    | otherwise = 0

-- // yuck!
-- pts :: Int -> Int
-- pts x =
--     if x == 1
--         then 10
--     else if x == 2
--         then 6
--     else if x == 3
--         then 4
--     else if x == 4
--         then 3
--     else if x == 5
--         then 2
--     else if x == 6
--         then 1
--     else 0

-- // piecewise notation in pattern matching
-- pts :: Int -> Int
-- pts 1 = 10
-- pts 2 = 6
-- pts 3 = 4
-- pts 4 = 3
-- pts 5 = 2
-- pts 6 = 1
-- pts _ = 0

-- // a better version of the above
pts :: Int -> Int
pts 1 = 10
pts 2 = 6
pts x
    | x <= 6    = 7 - x
    | otherwise = 0

-- // let bindings
roots a b c =
    let sdisc = sqrt ( b * b - 4 * a * c)
        twice_a = 2 * a
    in  ((-b + sdisc) / twice_a,
         (-b - sdisc) / twice_a)

-- // composing functions
square x = x ^ 2
f x = x + 3

-- // one way to do it
square_of_f x = square (f x)
f_of_square x = f (square x)

-- // cooler way
square_of_f x = (square . f) x
f_of_square x = (f . square) x

-- // even cooler! Since everything is math functions here, you can take out
-- /  the x from both sides
square_of_f = square . f
f_of_sqaure = f . square

