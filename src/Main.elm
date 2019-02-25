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
type Term = CTerm Const | VTerm String | Plus Term Term | Minus Term Term | Times Term Term | Eq Term Term | And Term Term | Or Term Term

-- V(alue)Type is a type that a TreeAssembly term can evaluate to
type VType = TBool | TInt

-- Val is a value that a term can evaluate to
type Val = VBool Bool | VInt Int

type alias Var = 
  { name: String
  , term: Term }
type alias Env = (String -> Maybe Term)

{-
attempts to find a Term for s in e; currying lookup with a list of vars
produces an Env
-}
lookup : List Var -> String -> Maybe Term
lookup e s =
  case e of
    [] ->
      Nothing
    
    v :: vs ->
      if v.name == s then
        Just v.term
      else lookup vs s


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
      x
    
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
typecheck : Env -> Term -> Maybe VType
typecheck e t =
  case t of
    CTerm c ->
      case c of
        CBool _ -> Just TBool
        CInt _ -> Just TInt
    
    VTerm v -> 
      case e v of
        Just subst -> typecheck e subst
        Nothing    -> Nothing
    
    -- binary int operators all have the same behavior
    Plus x y  -> filterInts (map (typecheck e) [x, y])
    Minus x y -> filterInts (map (typecheck e) [x, y])
    Times x y -> filterInts (map (typecheck e) [x, y])

    Eq x y ->
      if filterInts(map (typecheck e) [x, y]) == Just TInt then
        Just TBool
      else if filterBools(map (typecheck e) [x, y]) == Just TBool then
        Just TBool
      else
        Nothing
    
    And x y -> filterBools (map (typecheck e) [x, y])
    Or x y -> filterBools (map (typecheck e) [x, y])


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
eval : Env -> Term -> Maybe Val
eval e t =
  let
    evale = eval e
  in
  case t of
    CTerm c ->
      case c of
        CInt x  -> Just (VInt x)
        CBool x -> Just (VBool x)
    
    VTerm v ->
      case e v of
        Just subst -> evale subst
        Nothing    -> Nothing
    
    Plus x y -> 
      wrapInt ( tryBinFn (+) (tryInt (evale x)) (tryInt (evale y)) )

    Minus x y -> 
      wrapInt ( tryBinFn (-) (tryInt (evale x)) (tryInt (evale y)) )

    Times x y -> 
      wrapInt ( tryBinFn (*) (tryInt (evale x)) (tryInt (evale y)) )

    Eq x y ->
      takeOne
        ( wrapBool ( tryBinFn (==) (tryInt (evale x)) (tryInt (evale y)) )
        , wrapBool ( tryBinFn (==) (tryBool (evale x)) (tryBool (evale y)) )
        )

    And x y -> 
      wrapBool ( tryBinFn (&&) (tryBool (evale x)) (tryBool (evale y)) )
    
    Or x y -> 
      wrapBool ( tryBinFn (||) (tryBool (evale x)) (tryBool (evale y)) )


-- Render tree represents drawing state
type alias RenderTree = 
  { render: Bool
  , renderDepth: Int
  , term: Term
  , children: RenderChildren}
type RenderChildren = RenderChildren (List RenderTree)


genRenderTree : Int -> Env -> Term -> RenderTree
genRenderTree depth e t =
  let
    dnew = depth - 1
    gTree = genRenderTree dnew e
    c =
      case t of
        CTerm _   -> RenderChildren []
        VTerm x   -> 
          case e x of
            Just subst -> RenderChildren [gTree subst]
            Nothing    -> RenderChildren []
        Plus x y  -> RenderChildren [gTree x, gTree y]
        Minus x y -> RenderChildren [gTree x, gTree y]
        Times x y -> RenderChildren [gTree x, gTree y]
        Eq x y    -> RenderChildren [gTree x, gTree y]
        And x y   -> RenderChildren [gTree x, gTree y]
        Or x y    -> RenderChildren [gTree x, gTree y]

  in
  { render = True
  , renderDepth = depth
  , term = t
  , children = c}


type alias Model =
  { term: Term
  , env: Env
  , renderTree : RenderTree }

testVars = [ { name = "a", term = CTerm (CInt 5)}]
testTerm = And (Eq (Times (Plus (CTerm (CInt 5)) (CTerm (CInt 1))) (CTerm (CInt 7))) (Times (CTerm (CInt 21)) (VTerm "a"))) (CTerm (CBool True))
failTerm = Or (Eq (CTerm (CInt 1)) (CTerm (CBool True))) (CTerm (CBool False))
testDepth = 3

init : () -> (Model, Cmd Msg)
init _ =
  let
    -- this line curries lookup with our variables to produce the environment
    e = lookup testVars
  in
  ( { term = testTerm
    , env = e
    , renderTree = genRenderTree testDepth e testTerm }
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
  ( { model | renderTree = genRenderTree newDepth model.env model.term }
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
      [ div [ class "tree-container" ] [ div [] [ renderTree model.env model.renderTree ] ]
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
  , text ( "Evaluation result: " ++ valToString (eval model.env model.term) )
  ]


renderTree : Env -> RenderTree -> Html Msg
renderTree e t =
  if t.render && t.renderDepth > 0 then
    div [ class "tree-div" ] ( renderChildren e t.children ++ [ renderTerm e t.term ] )
  else
    div [] []


renderChildren : Env -> RenderChildren -> List (Html Msg)
renderChildren e (RenderChildren c) =
  map (renderTree e) c


renderTerm : Env -> Term -> Html Msg
renderTerm e t =
  div [ class "text-div" ]
  [ text (termToString t ++ " : ")
  , span [ class "type-span" ] [ text (typeToString (typecheck e t)) ]
  ]