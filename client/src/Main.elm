module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Random exposing (..)
import String exposing (..)


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
    | StartOver


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
        [ div [ class "content-block-title" ]
            [ text "Add these numbers" ]
        , div [ class "content-block" ]
            [ div [ class "content-block-inner" ]
                (model.numbers |> List.map viewNumber)
            ]
        , div [ class "content-block-title" ]
            [ a
                [ href "#"
                , class "button button-big button-fill"
                , onClick StartOver
                ]
                [ text "Play Again" ]
            ]
        ]


viewNumber : Int -> Html Msg
viewNumber n =
    div [ class "number" ]
        [ text (prettyInt n) ]


{-| From <https://github.com/avh4/elm-number-format/blob/master/src/Number/Format.elm>
That package hasn't been updated to Elm 0.18 yet, so just copied the function and
modified it a bit.
-}
prettyInt : Int -> String
prettyInt n =
    let
        ni =
            abs n

        nis =
            String.join (String.fromChar ',') (chunksOfRight 3 <| toString ni)
    in
        if n < 0 then
            String.cons '-' nis
        else
            nis


{-| From <https://github.com/circuithub/elm-string-split/blob/1.0.3/src/String/Split.elm>
That package hasn't been updated to Elm 0.18 yet, so just copied the function and
modified to work -- removed ' in var name.
-}
chunksOfRight : Int -> String -> List String
chunksOfRight k s =
    let
        len =
            length s

        k2 =
            2 * k

        chunksOfR s_ =
            if length s_ > k2 then
                right k s_ :: chunksOfR (dropRight k s_)
            else
                right k s_ :: [ dropRight k s_ ]
    in
        if len > k2 then
            List.reverse (chunksOfR s)
        else if len > k then
            dropRight k s :: [ right k s ]
        else
            [ s ]


viewDigitsButtons model =
    ((List.range 2 7)
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

        StartOver ->
            ( { model | screen = Start }, Cmd.none )


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
