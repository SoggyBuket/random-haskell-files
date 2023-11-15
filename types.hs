-- // some stuffs with types yay

-- // data defines new data types
-- /  type defines an alternate name for an existing type
-- /  newtype defines new data types equivalent to existing ones

-- // type type

-- // good for convenience and clarity, but don't abuse it
type Name = String
-- type AnniversaryBook = [Anniversary]

-- // date type
-- // Date in year, month, day format
data Date = Date Int Int Int
-- // looks a little like an enum
-- // Anniversary can be either a Birthday or a Wedding
-- /  Birthday contains name, year, month, day
-- /  Wedding contains spouse name 1, spouse name 2, year, month, day
-- // upon calling this, we get constructor functions named Birthday and Wedding
data Anniversary = Birthday Name Date
                 | Wedding Name Name Date

john_smith :: Anniversary
john_smith = Birthday "John Smith" (Date 1968 7 3)

smith_wedding :: Anniversary
smith_wedding = Wedding "Jane Smith" "John Smith" (Date 1987 3 4)

type AnniversaryBook = [Anniversary]

john_smith_anniversaries :: AnniversaryBook
john_smith_anniversaries = [john_smith, smith_wedding]

show_date :: Date -> String
show_date (Date year month day) =
    show year ++ "-" ++ show month ++ "-" ++ show day

show_anniversary :: Anniversary -> String
-- // binding variables to name year month day
show_anniversary (Birthday name date) =
    name ++ " born " ++ show_date date
show_anniversary (Wedding name1 name2 date) =
    name1 ++ " married " ++ name2 ++ " on " ++ show_date date

