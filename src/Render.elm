module Render exposing (renderCallTree)

import Html exposing (Html, button, div, text, h1, h3, input, span, textarea, br, p)
import Html.Attributes exposing (..)
import List exposing (map)

import Evaluate exposing (Val)
import Tree exposing (Tree(..))
import Types exposing (Term(..), BinOp(..))
import Typecheck exposing (CallTree, TSubst, typeToString, tsubstToString)


{-type alias RenderTree = Tree RenderNode

type alias RenderNode =
  { render: Bool
  , term: Term 
  , check: CheckResult }


buildRenderTree : CheckTree -> Int -> RenderTree
buildRenderTree (Tree tree) depth =
  let
    node =
      { render = depth > 0
      , term   = tree.node.term
      , check  = tree.node.check }
  
    children = (map (\c -> buildRenderTree c (depth - 1))) tree.children
  in
    Tree {node = node, children = children}


render : RenderTree -> Html msg
render (Tree tree) =
  if tree.node.render then
    div [ class "tree-div" ] ( map render tree.children ++ [ renderTerm tree.node ] )
  else
    div [] []

renderTerm : RenderNode -> Html msg
renderTerm node =
  let
    spanClass =
      case node.check of
        Checks _    -> "type-checks"
        Fails _ _ _ _ -> "type-fails"
        Partial _   -> "type-partial"
        Invalid     -> "type-fails"
  in
    div [ class "text-div" ]
    [ renderTermInline node.check node.term
    , text " : "
    , span [ class spanClass ] [ text (checkResultToString node.check) ]
    ]

-- renders the inline portion of the term, which may contain spans
renderTermInline : CheckResult -> Term -> Html msg
renderTermInline result t =
  let
    argTerms = listSubterms t
    isOp =
      case t of
        CTerm _   -> False
        VTerm _   -> False
        Tuple _ _ -> False
        _         -> True
    opStr =
      case t of
        CTerm _   -> ""
        VTerm _   -> ""
        BinTerm op _ _ ->
          case op of
            Plus  -> "+"
            Minus -> "-"
            Times -> "*"
            Div   -> "/"
            Mod   -> "%"
            Eq    -> "=="
            And   -> "&&"
            Or    -> "||"

        Lam _ _   -> "->"
        App _ _   -> " "
        _         -> ""

    subterms = renderSubterms argTerms result
  in
    case isOp of
      True ->
        case subterms of
          x :: xs ->
            case t of 
              _         -> span [] ([x, text (" " ++ opStr)] ++ xs)
            
          _ ->
            text "rendering error"
      False ->
        text (Evaluate.termToString t)-}
    

{-listSubterms : Term -> List Term
listSubterms t =
  case t of
    CTerm _     -> []
    VTerm _     -> []
    BinTerm _ x y -> [x, y]
    Lam x y     -> [VTerm ("\\" ++ x), y]
    App x y     -> [x, y]
    Tuple x y -> [x, y]

intersperse : a -> List a -> List a
intersperse i xs =
  case xs of
    [] -> []
    [x] -> [i, x]
    x :: rem -> [i, x] ++ intersperse i rem-}


{-renderSubterms : List Term -> CheckResult -> List (Html msg)
renderSubterms t c =
  let
    subterms = intersperse ( text " " ) ( renderSubtermsRec 1 t c )
  in
    case subterms of
      x :: xs -> xs
      _       -> []


renderSubtermsRec : Int -> List Term -> CheckResult -> List (Html msg)
renderSubtermsRec i ts c =
  let
    renderNext = renderSubtermsRec (i + 1)
    isFail =
      case c of
        Fails argNum exp got out ->
          i == argNum
        _ ->
          False
  in
    case ts of
      [] -> [ text "" ]
      [t] ->
        case isFail of
          True  -> [ span [class "error-subterm"] [text (Evaluate.termToString t), renderErrorDiv c] ]
          False -> [ text (Evaluate.termToString t) ]
      t :: ts2 ->
        case isFail of
          True -> [ span [class "error-subterm"] [text (Evaluate.termToString t), renderErrorDiv c] ]  ++
            renderNext ts2 c
          False -> [ text (Evaluate.termToString t) ] ++
            renderNext ts2 c


renderErrorDiv : CheckResult -> Html msg
renderErrorDiv c =
  case c of
    Fails _ exp got out ->
      let
        expStr = typeToString exp
        gotStr = typeToString got
      in
        div [class "error-details"] [ text ("Expected: " ++ expStr), br [] [], text ("Got: " ++ gotStr) ]

    _ -> div [class "error-details"] []-}


renderCallTreeDebug : CallTree -> Html msg
renderCallTreeDebug (Tree tree) =
  let
    subsString = case tree.node.subs of
      Just s  -> tsubstToString s
      Nothing -> "Unification failed"

  in
    div [ class "tree-div" ]
      ( map renderCallTreeDebug tree.children ++
        [ div [ class "text-div" ]
          [ text (Evaluate.termToString tree.node.term ++ " : " ++ typeToString tree.node.inType ++ ", " ++ subsString)]
        ]
      )

renderCallTree : CallTree -> Int -> Html msg
renderCallTree (Tree tree) depth =
  renderCallTreeRec (Tree tree) (Maybe.withDefault [] tree.node.subs) depth

renderCallTreeRec : CallTree -> TSubst -> Int -> Html msg
renderCallTreeRec (Tree tree) subs depth =
  let
    typeString =
      case tree.node.subs of
        Just _ ->
          Typecheck.apply subs tree.node.inType
            |> typeToString
        
        Nothing ->
          "Unification Failed"

  in
    if depth > 0 then
      div [ class "tree-div" ]
        ( map (\c -> renderCallTreeRec c subs (depth - 1)) tree.children ++
          [ div [ class "text-div" ]
            [ text (Evaluate.termToString tree.node.term ++ " : " ++ typeString) ]
          ]
        )
    
    else
      div [] []

