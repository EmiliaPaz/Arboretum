module Tree exposing (Tree(..), Children(..))

type Tree a =
  Tree
    { node: a
    , children: Children a }

type Children a = Children (List (Tree a))