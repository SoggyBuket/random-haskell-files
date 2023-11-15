-- // 'do' puts actions together in sequence
main =
 do putStrLn "Please enter your name:"
    -- // assigning a variable name to the output
    -- // can be use on any action except the last
    name <- getLine
    -- // doesn't work because make_loud doesn't return an action variable
    -- loud_name <- make_loud name
    -- // create a regular variable out of the output of make_loud
    -- // this 'let' doesn't need an 'in' in do blocks
    let loud_name = make_loud name
    -- // our whole program has type IO () because of the output of this
    putStrLn ("Hello, " ++ name ++ ", how are you ?")

make_loud :: String -> String
make_loud s = map toUpper s
-- // putStrLn :: String -> IO ()
-- // getLine :: IO String
-- // the IO type is to show side effects happening outside of the program and
-- /  is called an action type. For instance, IO String is not a String...
-- /  yet. It becomes a String when it's run, but before that, it is a
-- /  completely different type
