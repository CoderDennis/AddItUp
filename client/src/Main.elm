module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Random exposing (..)


type alias Model =
    { screen : Screen
    , digits : Int
    , numberCount : Int
    , numbers : List Int
    }


type Msg
    = SetDigits Int
    | SetNumberCount Int
    | Play
    | SetNumbers (List Int)


type Screen
    = Start
    | Numbers


view : Model -> Html Msg
view model =
    case model.screen of
        Start ->
            viewStart model

        Numbers ->
            viewNumbers model


viewStart : Model -> Html Msg
viewStart model =
    div [ class "page-content" ]
        [ div [ class "content-block-title" ]
            [ text "How many digits in each number?" ]
        , div [ class "content-block" ]
            [ div [ class "content-block-inner" ]
                (viewDigitsButtons model)
            ]
        , div [ class "content-block-title" ]
            [ text "How many numbers?" ]
        , div [ class "content-block" ]
            [ div [ class "content-block-inner" ]
                (viewNumberCountButtons model)
            ]
        , div [ class "content-block-title" ]
            [ a
                [ href "#"
                , class "button button-big button-fill"
                , onClick Play
                ]
                [ text "Play" ]
            ]
        ]


viewNumbers : Model -> Html Msg
viewNumbers model =
    div [ class "page-content" ]
        [ div [ class "content-block" ]
            (model.numbers |> List.map viewNumber)
        ]


viewNumber n =
    div []
        [ text (toString n) ]


viewDigitsButtons model =
    ((List.range 1 6)
        |> List.map (viewDigitsButton model)
    )


viewDigitsButton : Model -> Int -> Html Msg
viewDigitsButton model n =
    let
        classes =
            if model.digits == n then
                "button button-big button-inline"
            else
                "button button-big button-fill button-inline"
    in
        a
            [ href "#"
            , class classes
            , onClick (SetDigits n)
            ]
            [ text (digitButtonText n) ]


digitButtonText : Int -> String
digitButtonText n =
    if n == 1 then
        "1 Digit"
    else
        (toString n) ++ " Digits"


viewNumberCountButtons model =
    ((List.range 2 7)
        |> List.map (viewNumberCountButton model)
    )


viewNumberCountButton : Model -> Int -> Html Msg
viewNumberCountButton model n =
    let
        classes =
            if model.numberCount == n then
                "button button-big button-inline"
            else
                "button button-big button-fill button-inline"
    in
        a
            [ href "#"
            , class classes
            , onClick (SetNumberCount n)
            ]
            [ text ((toString n) ++ " Numbers") ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetDigits n ->
            ( { model | digits = n }, Cmd.none )

        SetNumberCount n ->
            ( { model | numberCount = n }, Cmd.none )

        Play ->
            ( model, generate SetNumbers (numberListGenerator model) )

        SetNumbers numbers ->
            ( { model
                | numbers = numbers
                , screen = Numbers
              }
            , Cmd.none
            )


numberListGenerator : Model -> Generator (List Int)
numberListGenerator model =
    list model.numberCount (numberGenerator model.digits)


numberGenerator : Int -> Generator Int
numberGenerator digits =
    let
        min =
            10 ^ (digits - 1)

        max =
            10 ^ digits - 1
    in
        int min max


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
