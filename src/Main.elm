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

-- MODEL
type Const = CBool Bool | CInt Int
type alias Var = 
  { name: String
  , term: Term }
type Term = CTerm Const | VTerm Var | Plus Term Term | Minus Term Term | Times Term Term | Eq Term Term | And Term Term | Or Term Term

type alias Model = Term

init : () -> (Model, Cmd Msg)
init _ =
  ( CTerm (CBool False)
  , Cmd.none )


-- UPDATE
type Msg = None


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = (model, Cmd.none)


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW
view : Model -> Document Msg
view _ = 
  { title = "Tree Assembly"
  , body =
  [ div [] [text "TreeAssembly, comming soon!"] ]
  }
