module Render exposing (RenderTree)

import Types exposing (Term)


type RenderTree = 
  RenderTree
    { node: RenderNode
    , children: Children }

type Children = Children (List RenderTree)

type alias RenderNode =
  { render: Bool
  , term: Term }