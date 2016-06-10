data Thing = Shoe
           | Ship
           | SealingWax
           | Cabbage
           | King
    deriving Show
data Person = Person String Int Thing
 deriving Show

brent :: Person
brent = Person "Brent" 31 SealingWax

getAge :: Person -> Int
getAge (Person _ a _) = a

baz :: Person -> String
baz p@(Person n _ _) = " The name field of (" ++ show p ++ ") is " ++ n
