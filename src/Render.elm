module Render exposing (RenderTree)

import Tree exposing (Tree, Children)
import Types exposing (Term)
import Typecheck exposing (CheckResult)

type alias RenderNode =
  { render: Bool
  , term: Term 
  , check: CheckResult }

type alias RenderTree = Tree RenderNode