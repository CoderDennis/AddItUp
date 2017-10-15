module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href, autofocus, type_)
import Html.Attributes.Aria exposing (ariaPressed)
import Html.Events exposing (onClick, onInput)
import Random exposing (..)
import String exposing (..)


type alias Model =
    { screen : Screen
    , digits : Int
    , numberCount : Int
    , numbers : List Int
    , answer : Int
    , check : Check
    }


type Msg
    = SetDigits Int
    | SetNumberCount Int
    | Play
    | SetNumbers (List Int)
    | AppendToAnswer String
    | Backspace
    | CheckAnswer
    | StartOver


type Check
    = NotChecked
    | Correct
    | Incorrect


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
    div []
        [ h1 []
            [ text "Add It Up!" ]
        , div []
            [ text "How many digits in each number?" ]
        , div [ class "row justify-content-center" ]
            (viewDigitsButtons model)
        , div []
            [ text "How many numbers?" ]
        , div [ class "row justify-content-center" ]
            (viewNumberCountButtons model)
        , div [ class "row" ]
            [ div [ class "col" ]
                [ button
                    [ type_ "button"
                    , class "but btn-primary btn-lg btn-block"
                    , onClick Play
                    ]
                    [ text "Play" ]
                ]
            ]
        ]


viewNumbers : Model -> Html Msg
viewNumbers model =
    div []
        [ div [ class "row" ]
            [ div [ class "col" ]
                [ text "Add these numbers" ]
            , div [ class "col" ]
                (model.numbers |> List.map viewNumber)
            , div [ class "col-1" ]
                []
            ]
        , div [ class "row" ]
            [ div [ class "col" ]
                [ text "Answer" ]
            , div [ class "col" ]
                [ div [ class "number" ]
                    [ text (prettyInt model.answer) ]
                ]
            , div
                [ class "col-1 backspace" ]
                (viewBackspaceButton model)
            ]
        , div [ class "row justify-content-center" ]
            [ viewAnswerButtons ]
        , div []
            [ (viewCheckButton model) ]
        ]


viewBackspaceButton : Model -> List (Html Msg)
viewBackspaceButton model =
    if model.answer > 0 then
        [ a
            [ href "#"
            , onClick Backspace
            ]
            [ text "⌫" ]
        ]
    else
        []


viewCheckButton : Model -> Html Msg
viewCheckButton model =
    let
        ( nextMsg, buttonText ) =
            case model.check of
                NotChecked ->
                    ( CheckAnswer, "Check Answer" )

                Incorrect ->
                    ( CheckAnswer, "Incorrect, Try Again" )

                Correct ->
                    ( StartOver, "Correct! Play Again" )
    in
        button
            [ type_ "button"
            , class "but btn-primary btn-lg btn-block"
            , onClick nextMsg
            ]
            [ text buttonText ]


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


viewDigitsButtons : Model -> List (Html Msg)
viewDigitsButtons model =
    ((List.range 2 7)
        |> List.map (viewDigitsButton model)
    )


viewDigitsButton : Model -> Int -> Html Msg
viewDigitsButton model n =
    if model.digits == n then
        viewDigitsButtonPressed n
    else
        viewDigitsButtonNormal n


viewDigitsButtonNormal : Int -> Html Msg
viewDigitsButtonNormal n =
    div [ class "col-xs" ]
        [ button
            [ type_ "button"
            , class "btn btn-outline-primary btn-lg"
            , onClick (SetDigits n)
            ]
            [ text (digitButtonText n) ]
        ]


viewDigitsButtonPressed : Int -> Html Msg
viewDigitsButtonPressed n =
    div [ class "col-xs" ]
        [ button
            [ type_ "button"
            , class "btn btn-outline-primary btn-lg active"
            , ariaPressed True
            ]
            [ text (digitButtonText n) ]
        ]


digitButtonText : Int -> String
digitButtonText n =
    if n == 1 then
        "1 Digit"
    else
        (toString n) ++ " Digits"


viewNumberCountButtons : Model -> List (Html Msg)
viewNumberCountButtons model =
    ((List.range 2 7)
        |> List.map (viewNumberCountButton model)
    )


viewNumberCountButton : Model -> Int -> Html Msg
viewNumberCountButton model n =
    if model.numberCount == n then
        viewNumberCountButtonPressed n
    else
        viewNumberCountButtonNormal n


viewNumberCountButtonNormal : Int -> Html Msg
viewNumberCountButtonNormal n =
    div [ class "col-xs" ]
        [ button
            [ type_ "button"
            , class "btn btn-outline-primary btn-lg"
            , onClick (SetNumberCount n)
            ]
            [ text ((toString n) ++ " Numbers") ]
        ]


viewNumberCountButtonPressed : Int -> Html Msg
viewNumberCountButtonPressed n =
    div [ class "col-xs" ]
        [ button
            [ type_ "button"
            , class "btn btn-outline-primary btn-lg active"
            , ariaPressed True
            ]
            [ text ((toString n) ++ " Numbers") ]
        ]


viewAnswerButtons : Html Msg
viewAnswerButtons =
    div
        [ class "btn-group" ]
        ((List.range 0 9)
            |> List.map viewAnswerButton
        )


viewAnswerButton : Int -> Html Msg
viewAnswerButton n =
    button
        [ type_ "button"
        , class "btn btn-primary"
        , onClick (AppendToAnswer (toString n))
        ]
        [ text (toString n) ]


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

        AppendToAnswer a ->
            let
                answerString =
                    (toString model.answer) ++ a

                answerInt =
                    Result.withDefault 0 (String.toInt answerString)
            in
                ( { model | answer = answerInt }, Cmd.none )

        Backspace ->
            let
                answerString =
                    model.answer
                        |> toString
                        |> slice 0 -1

                answerInt =
                    Result.withDefault 0 (String.toInt answerString)
            in
                ( { model | answer = answerInt }, Cmd.none )

        CheckAnswer ->
            let
                ans =
                    List.sum model.numbers

                check =
                    if ans == model.answer then
                        Correct
                    else
                        Incorrect
            in
                ( { model | check = check }, Cmd.none )

        StartOver ->
            ( { model
                | screen = Start
                , answer = 0
                , check = NotChecked
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
    , digits = 7
    , numberCount = 7
    , numbers = []
    , answer = 0
    , check = NotChecked
    }


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
