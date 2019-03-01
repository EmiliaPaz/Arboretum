import Browser exposing (Document)
import Html exposing (Html, button, div, text, h1, input, span)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import String exposing (concat)
import List exposing (map,head,tail)
import Debug exposing (toString)

main =
  Browser.document
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }
-- MODEL

type alias Model =
  { content : String,
    termTree : Term
  }

init : () -> (Model, Cmd Msg)
init _ =
  ({content = "", termTree=EmptyTree}, Cmd.none)
  

-- UPDATE

type Msg
  = Change String | Render Term


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Change newContent ->
      ({ model | content = newContent }, Cmd.none)
    Render newTree ->
      ({ model | termTree = newTree }, Cmd.none)


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-------------------------------------- Tokenizer --------------------------------------
type Token = TokPlus | TokMinus | TokTimes | TokAssign | TokEq | TokAnd | TokOr | TokLParen | TokRParen | TokVar String | TokConstInt String | TokConstBool String | TokInvalid | TokEnd

operator : String -> Token
operator str =
            case str of
                "" -> TokInvalid
                other ->
                    if (str == "+")
                        then TokPlus
                     else if (str == "-")
                         then TokMinus
                     else if (str == "*")
                         then TokTimes
                     else if (str == "=")
                         then TokAssign
                     else if (str == "==")
                         then TokEq
                     else if (str == "&&")
                         then TokAnd
                     else if (str == "||")
                         then TokOr
                     else TokInvalid

operators = ["+", "-", "*", "=", "==", "&&", "||"]

isBoolean : String -> Bool
isBoolean x =
    case x of
        "True" -> True
        "False" -> True
        other -> False


tokenize : List String -> List Token
tokenize str =
        case str of
            [] -> []
            (x::xs) ->
                if (List.member x operators)
                    then (operator x) :: tokenize xs
                else if (isBoolean x)
                    then TokConstBool x :: tokenize xs
                else if (String.filter Char.isDigit x == x && x /= "")
                    then TokConstInt x :: tokenize xs
                else if (String.filter Char.isLower x == x && x /= "")
                    then TokVar x :: tokenize xs
                else if (x == "(")
                    then TokLParen :: tokenize xs
                else if (x == ")")
                    then TokRParen :: tokenize xs
                else if (x == "")
                    then tokenize xs
                else TokInvalid :: tokenize xs

tokenizePrint : List Token -> String
tokenizePrint tokens =
    case tokens of
        [] -> ""
        (x::xs) ->
            case x of
                _ -> (Debug.toString x) ++ ", " ++ tokenizePrint xs



type alias Var = 
  { name: String
  , term: Term }


-------------------------------------- Parser --------------------------------------
type Term = CTerm Const | VTerm Var | Plus Term Term | Minus Term Term | Times Term Term | Eq Term Term | And Term Term | Or Term Term | EmptyTree
type Const = CBool Bool | CInt Int



toMaybe : List Token -> Maybe (List Token)
toMaybe tokens = case tokens of
                  [] -> Nothing
                  _ -> Just tokens

fromMaybeList : Maybe (List Token) -> List Token
fromMaybeList ls = case ls of
                  Nothing -> []
                  Just list -> list

parse : List Token -> Term
parse tokens = let (tree, toks) = expression tokens
                in tree

fromMaybeInt : Maybe Int -> Int
fromMaybeInt x = case x of
                  Nothing -> 0
                  Just y  -> y

expression : List Token -> (Term, List Token)
expression tokens = let (termTree, tokens2) = expr tokens
                      in case head tokens2 of
                        Just (TokPlus) ->  let (expTree, tokens3) = expression (fromMaybeList(tail tokens2)) in (Plus termTree expTree, tokens3)
                        Just (TokMinus) ->  let (expTree, tokens3) = expression (fromMaybeList(tail tokens2)) in (Minus termTree expTree, tokens3)
                        Just (TokTimes) ->  let (expTree, tokens3) = expression (fromMaybeList(tail tokens2)) in (Times termTree expTree, tokens3)
                        Just (TokConstInt n) -> (termTree, tokens2)
                        _ -> (termTree, tokens2)

expr : List Token -> (Term, List Token)
expr tokens =
    case head tokens of
        Just (TokLParen)      -> let (leftTree, rightTokens) = expression (fromMaybeList(tail tokens)) 
                                  in case head rightTokens of
                                    Just (TokRParen)  -> (leftTree, fromMaybeList(tail rightTokens))
                                    _                 -> (EmptyTree, [])
        Just (TokConstInt p)  -> let p_int = fromMaybeInt(String.toInt p) in (CTerm (CInt p_int), fromMaybeList(tail tokens))
        Just (TokPlus)        -> (EmptyTree, [])
        Just (TokMinus)       -> (EmptyTree, [])
        Just (TokTimes)       -> (EmptyTree, [])
        _                     -> (EmptyTree, [])


-- Helper functions
oneAhead : List Token -> Maybe Token
oneAhead list = 
    case list of 
        [] -> Nothing
        t::ts -> Just t

-------------------------------------- Rendering --------------------------------------

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
    EmptyTree -> ""


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
    EmptyTree -> Nothing


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
    EmptyTree -> Nothing


----------- end of tree implementation -----------


-- VIEW

view : Model -> Document Msg
view model =
 { title = "Tree Assembly"
  , body =
    [
      div []
        [ input [ placeholder "Text to tokenize", value model.content, onInput Change ] []
        , div [] [ text (tokenizePrint(tokenize(String.words model.content))) ]
        , div [] [ text (toString (parse (tokenize(String.words model.content)))) ]
        ]
    , Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "trees.css" ] []
    , div [ class "tree-container" ] [ div [] [ renderTree (parse (tokenize(String.words model.content))) ] ]
    , renderSummary model
    ]
 }

renderSummary : Model -> Html Msg
renderSummary model =
  div [ class "summary" ]
  [ h1 [ class "summary-title" ] [ text "Summary:" ]
  , text ( "Evaluation result: " ++ valToString (eval model.termTree) )
  ]

renderTree : Term -> Html Msg
renderTree t =
  case t of
    CTerm x ->
      div [ class "tree-div" ] [ renderTerm t ]
    
    VTerm x ->
      div [ class "tree-div" ]
      [ renderTree x.term
      , renderTerm t ]

    Plus x y ->
      div [class "tree-div"]
      [ renderTree x
      , renderTree y
      , renderTerm t 
      ] 

    Minus x y ->
      div [class "tree-div"]
      [ renderTree x
      , renderTree y
      , renderTerm t 
      ] 

    Times x y ->
      div [class "tree-div"]
      [ renderTree x
      , renderTree y
      , renderTerm t 
      ] 

    Eq x y ->
      div [class "tree-div"]
      [ renderTree x
      , renderTree y
      , renderTerm t 
      ] 
    
    And x y ->
      div [class "tree-div"]
      [ renderTree x
      , renderTree y
      , renderTerm t 
      ] 

    Or x y ->
      div [class "tree-div"]
      [ renderTree x
      , renderTree y
      , renderTerm t 
      ] 
    
    EmptyTree -> div [class "tree-div"]
      [ div [ class "text-div" ] [ text ( "") ] ] 


renderTerm : Term -> Html Msg
renderTerm t =
  div [ class "text-div" ]
  [ text (termToString t ++ " : ")
  , span [ class "type-span" ] [ text (typeToString (typecheck t)) ]
  ]