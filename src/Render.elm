module Render exposing (RenderTree)

import Tree exposing (Tree(..), Children(..))
import Types exposing (Term)
import Typecheck exposing (CheckResult)

type alias RenderNode =
  { render: Bool
  , term: Term 
  , check: CheckResult }

type alias RenderTree = Tree RenderNode

buildRenderTree : CheckTree -> Int -> RenderTree
buildRenderTree tree depth =
  let
    node =
      { render = depth > 0
      , term   = tree.node.term
      , check  = tree.node.check }
  
    children =
      Children (map (\c -> buildRenderTree c (depth - 1))) tree.children
  in
    Tree node children

