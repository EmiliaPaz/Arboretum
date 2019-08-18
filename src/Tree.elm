module Tree exposing (Tree(..))

type Tree a =
  Tree
    { node: a
    , children: List (Tree a) }