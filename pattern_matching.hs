-- // In pattern matching, we attempt to match values against patterns and, if
-- /  so desired, bind variables to successful matches (stolen from the
-- /  haskell wiki)

-- // pattern matching is everywhere. Take map for example

my_map _ [] = []
my_map f (x:xs) = f x : my_map f xs

-- //
-- /  f is a pattern that matches anything at all, and binds to f what matched
-- /  (x:xs) matches a non-empty list which is formed by something, which we
-- /      bind to x which was cons'd onto something else, which we bind to xs.
-- /      x and xs can be thought of as sub-patterns used to match the parts of
-- /      the list
-- /  [] matches to the empty list, and we don't bind it to anything
-- /  _ matches anything without binding, as a wildcard don't care pattern
-- //
--
-- // from this we can see that pattern matching is useful for recognizing
-- /  values, binding variables (by using variable names as patterns) and to
-- /  break down values into parts, like (x:xs)
--
-- // most functions are not allowed in pattern matching, except for
-- /  constructors, just like how we did with the Anniversary type in types.hs
-- // the reason we can use [] and (:) is because they're constructors
-- // because of that, we can't do something like
-- drop_three :: [a] -> [a]
-- drop_three ([x,y,z] ++ xs) = xs
-- // but we can do this:
drop_three :: [a] -> [a]
drop_three (_:_:_:xs) = xs
-- // reasonable default if the list doesn't have 3 elements
drop_three _          = []

-- // you could just as easily use drop to do this, so don't reinvent the wheel

first_plus_second :: (Num a) => (a, a) -> a
first_plus_second (x, y) = x + y

normalize_3d :: (Floating a) => (a, a, a) -> a
normalize_3d (x, y, z) = sqrt (x^2 + y^2 + z^2)

-- // with tuple constructors, you can do stuff like this:
full_name :: (String, String, String)
full_name = (,,) "Margot" "Elizabeth" "Bouma"

the_beatles = (,,,) "George" "John" "Paul" "Ringo"

-- // things like this only work with literal values
f :: Int -> Int
f 0 = 1
f 1 = 5
f 2 = 2
f _ = -1

-- /  so you can't do
-- /  k = 1
-- /  h :: Int -> Bool
-- /  h k = True
-- /  h _ = False
-- /  because k isn't a literal value

-- // when using something like (x:xs) it may be useful to also set a variable
-- /  name to the whole value being sub-patterned, and you do that in
-- /  var@pattern
contrived_map :: ([a] -> a -> b) -> [a] -> [b]
contrived_map f [] = []
contrived_map f list@(x:xs) = f list x : contrived_map f xs

my_scanr :: (a -> b -> b) -> b -> [a] -> [b]
my_scanr f acc [] = [acc]
-- // partial_result is used to go all the way to the end of the list, and then
-- /  use acc as the result
my_scanr f acc (x:xs) = (f x (last_result)) : partial_result
    where partial_result@(last_result:_) = my_scanr f acc xs

-- // records can be used to name values in a datatype
data Stuff = Thing
           | Thing2 { num::Int, name::String }

stuff_grab :: Stuff -> Int
stuff_grab Thing2 {name=thing_name} = length thing_name
-- stuff_grab Thing2 {} = 0

-- // can construct by either a name or the order now
my_thing2 = Thing2 1 "silly"
my_thing22 = Thing2 {name = "more silly", num = 2}

-- // the {} pattern can be used for matching a constructor reguardless of the
-- /  elements it holds

-- // wherever you can bind variables, you can pattern match
-- // let and where do local variable binding, so you can pattern match in them
-- y = let (x:_) = map (*2) [1, 2, 3]
--     in x + 5

-- // x is equal to the first element that map gives back
y = x + 5
    where
        (x:_) = map (*2) [1, 2, 3]

-- // can also pattern match in lambdas
my_swap :: (b, a) -> (a, b)
my_swap = \(x, y) -> (y, x)
