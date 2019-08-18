module Stack exposing (Stack, push, peek, pop, empty)

type alias Stack a = List a

push : a -> Stack a -> Stack a
push val stack =
  [val] ++ stack

peek : Stack a -> Maybe a
peek stack =
  case stack of
    [] -> Nothing
    x :: xs -> Just x

pop : Stack a -> Stack a
pop stack =
  case stack of
    [] -> []
    x :: xs -> xs

empty : Stack a
empty = []
