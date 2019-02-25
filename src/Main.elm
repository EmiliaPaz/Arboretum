import List exposing (map)
import Browser exposing (Document)
import Html exposing (Html, div, button, h1, text, span)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class)

-- MAIN
main =
  Browser.document
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

{-
Compilation Examples:

a = 5 + 3

Model = VTerm ( name = "a"
              , term = Plus (CTerm (CInt 5)) (CTerm (Cint 3))
              )


a = True || False
b = 5 == a

Model = VTerm ( name = "b"
              , term = Plus (CTerm (CInt 5)) (VTerm 
                                               ( name = "a"
                                               , term = Or (CTerm (CBool True) (CBool False))
                                               )
                                             )
              )


-}



-- MODEL

type Const = CBool Bool | CInt Int
type alias Var = 
  { name: String
  , term: Term }
type Term = CTerm Const | VTerm Var | Plus Term Term | Minus Term Term | Times Term Term | Eq Term Term | And Term Term | Or Term Term

-- V(alue)Type is a type that a TreeAssembly term can evaluate to
type VType = TBool | TInt

-- Val is a value that a term can evaluate to
type Val = VBool Bool | VInt Int


boolToString : Bool -> String
boolToString b = 
  case b of 
    True -> "True"
    False -> "False"


termToString : Term -> String
termToString t =
  case t of
    CTerm x ->
      case x of
        CBool a -> boolToString a
        CInt a -> String.fromInt a
      
    VTerm x ->
      "( " ++ x.name ++ " | " ++ x.name ++ " = " ++ (termToString x.term) ++ " )"
    
    Plus t1 t2 ->
      "(" ++ (termToString t1) ++ " + " ++ (termToString t2) ++ ")"

    Minus t1 t2 ->
      "(" ++ (termToString t1) ++ " - " ++ (termToString t2) ++ ")"

    Times t1 t2 ->
      "(" ++ (termToString t1) ++ " * " ++ (termToString t2) ++ ")"

    Eq t1 t2 ->
      "(" ++ (termToString t1) ++ " == " ++ (termToString t2) ++ ")"
    
    And t1 t2 ->
      "(" ++ (termToString t1) ++ " && " ++ (termToString t2) ++ ")"

    Or t1 t2 ->
      "(" ++ (termToString t1) ++ " || " ++ (termToString t2) ++ ")"


typeToString : Maybe VType -> String
typeToString t =
  case t of
    Just TBool -> "Bool"
    Just TInt  -> "Int"
    Nothing    -> "Type Error"


valToString : Maybe Val -> String
valToString v =
  case v of
    Just (VBool x) -> boolToString x
    Just (VInt x)  -> String.fromInt x
    Nothing        -> "Undefined"


-- returns input if input is Just VType, Nothing otherwise
filterTypes : VType -> List (Maybe VType) -> Maybe VType
filterTypes t xs = 
  case xs of
    [] ->
      Nothing

    [x] ->
      if x == (Just t) then
        x
      else
        Nothing

    x :: rest ->
      if x == (Just t) then
        filterTypes t rest
      else
        Nothing


filterInts = filterTypes TInt
filterBools = filterTypes TBool


-- returns Just VType if the term typechecks, Nothing otherwise
typecheck : Term -> Maybe VType
typecheck t =
  case t of
    CTerm c ->
      case c of
        CBool _ -> Just TBool
        CInt _ -> Just TInt
    
    VTerm v -> typecheck v.term
    
    -- binary int operators all have the same behavior
    Plus x y  -> filterInts (map typecheck [x, y])
    Minus x y -> filterInts (map typecheck [x, y])
    Times x y -> filterInts (map typecheck [x, y])

    Eq x y ->
      if filterInts(map typecheck [x, y]) == Just TInt then
        Just TBool
      else if filterBools(map typecheck [x, y]) == Just TBool then
        Just TBool
      else
        Nothing
    
    And x y -> filterBools (map typecheck [x, y])
    Or x y -> filterBools (map typecheck [x, y])


tryBinFn : (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
tryBinFn f mx my = 
  case mx of
    Just x ->
      case my of
        Just y  -> Just (f x y)
        Nothing -> Nothing
    
    Nothing -> Nothing

tryBool : Maybe Val -> Maybe Bool
tryBool mx =
  case mx of
    Just (VBool x) -> Just x
    _              -> Nothing

tryInt : Maybe Val -> Maybe Int
tryInt mx =
  case mx of
    Just (VInt x) ->  Just x
    _              -> Nothing

wrapInt : Maybe Int -> Maybe Val
wrapInt c =
  case c of
    Just x  -> Just (VInt x)
    Nothing -> Nothing

wrapBool : Maybe Bool -> Maybe Val
wrapBool c =
  case c of
    Just x  -> Just (VBool x)
    Nothing -> Nothing

-- an approximaton of an 'or' operation with maybe
takeOne : (Maybe a, Maybe a) -> Maybe a
takeOne (mx, my) =
  case mx of
    Just x  -> Just x
    Nothing ->
      case my of
        Just y ->  Just y
        Nothing -> Nothing


-- evaluates a term
eval : Term -> Maybe Val
eval t =
  case t of
    CTerm c ->
      case c of
        CInt x  -> Just (VInt x)
        CBool x -> Just (VBool x)
    
    VTerm v ->
      eval v.term
    
    Plus x y -> 
      wrapInt ( tryBinFn (+) (tryInt (eval x)) (tryInt (eval y)) )

    Minus x y -> 
      wrapInt ( tryBinFn (-) (tryInt (eval x)) (tryInt (eval y)) )

    Times x y -> 
      wrapInt ( tryBinFn (*) (tryInt (eval x)) (tryInt (eval y)) )

    Eq x y ->
      takeOne
        ( wrapBool ( tryBinFn (==) (tryInt (eval x)) (tryInt (eval y)) )
        , wrapBool ( tryBinFn (==) (tryBool (eval x)) (tryBool (eval y)) )
        )

    And x y -> 
      wrapBool ( tryBinFn (&&) (tryBool (eval x)) (tryBool (eval y)) )
    
    Or x y -> 
      wrapBool ( tryBinFn (||) (tryBool (eval x)) (tryBool (eval y)) )


-- Render tree represents drawing state
type alias RenderTree = 
  { render: Bool
  , renderDepth: Int
  , term: Term
  , children: RenderChildren}
type RenderChildren = RenderChildren (List RenderTree)


genRenderTree : Int -> Term -> RenderTree
genRenderTree depth t =
  let
    dnew = depth - 1
    c =
      case t of
        CTerm _   -> RenderChildren []
        VTerm x   -> RenderChildren [genRenderTree dnew x.term]
        Plus x y  -> RenderChildren [genRenderTree dnew x, genRenderTree dnew y]
        Minus x y -> RenderChildren [genRenderTree dnew x, genRenderTree dnew y]
        Times x y -> RenderChildren [genRenderTree dnew x, genRenderTree dnew y]
        Eq x y    -> RenderChildren [genRenderTree dnew x, genRenderTree dnew y]
        And x y   -> RenderChildren [genRenderTree dnew x, genRenderTree dnew y]
        Or x y    -> RenderChildren [genRenderTree dnew x, genRenderTree dnew y]

  in
  { render = True
  , renderDepth = depth
  , term = t
  , children = c}


type alias Model =
  { term: Term
  , renderTree : RenderTree }

testTerm = And (Eq (Times (Plus (CTerm (CInt 5)) (CTerm (CInt 1))) (CTerm (CInt 7))) (Times (CTerm (CInt 21)) (CTerm (CInt 2)))) (CTerm (CBool True))
failTerm = Or (Eq (CTerm (CInt 1)) (CTerm (CBool True))) (CTerm (CBool False))
testDepth = 3

init : () -> (Model, Cmd Msg)
init _ =
  ( { term = failTerm
    , renderTree = genRenderTree testDepth failTerm }
  , Cmd.none )


-- UPDATE
type Msg = IncDepth | DecDepth


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  let
    newDepth =
      case msg of
        IncDepth -> model.renderTree.renderDepth + 1
        DecDepth -> model.renderTree.renderDepth - 1
  in
  ( { model | renderTree = genRenderTree newDepth model.term }
  , Cmd.none)


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW
view : Model -> Document Msg
view model = 
  { title = "Tree Assembly"
  , body =
    -- This line is a deplorable hack!  Elm doesn't offer any way to use css with reactor, so
    -- this is the workaround:
    [  Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "trees.css" ] []
    , div [ class "flex-container" ]
      [ div [ class "tree-container" ] [ div [] [ renderTree model.renderTree ] ]
      , div [ class "ui-div" ]
        [ renderSummary model
        , div [ class "buttons" ]
          [ button [ onClick DecDepth ] [ text "-" ]
          , text ( String.fromInt model.renderTree.renderDepth )
          , button [ onClick IncDepth ] [ text "+" ] ]
        ]
      ]
    ]
  }

renderSummary : Model -> Html Msg
renderSummary model =
  div [ class "summary" ]
  [ h1 [ class "summary-title" ] [ text "Summary:" ]
  , text ( "Evaluation result: " ++ valToString (eval model.term) )
  ]


renderTree : RenderTree -> Html Msg
renderTree t =
  if t.render && t.renderDepth > 0 then
    div [ class "tree-div" ] ( renderChildren t.children ++ [ renderTerm t.term ] )
  else
    div [] []


renderChildren : RenderChildren -> List (Html Msg)
renderChildren (RenderChildren c) =
  map renderTree c


renderTerm : Term -> Html Msg
renderTerm t =
  div [ class "text-div" ]
  [ text (termToString t ++ " : ")
  , span [ class "type-span" ] [ text (typeToString (typecheck t)) ]
  ]