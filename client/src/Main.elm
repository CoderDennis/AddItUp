module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)


type alias Model =
    { screen : Screen
    , digits : Int
    , numberCount : Int
    , numbers : List Int
    }


type Msg
    = NoOp


type Screen
    = Start
    | Numbers


view : Model -> Html Msg
view model =
    div [ class "page-content" ]
        [ div [ class "content-block-title" ]
            [ text "How many digits?" ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


initialModel : Model
initialModel =
    { screen = Start
    , digits = 2
    , numberCount = 2
    , numbers = []
    }


main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
