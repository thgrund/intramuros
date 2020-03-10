module Main exposing (Model, Msg(..), getCode, init, main, subscriptions, update, view, viewEditor)

import Browser
import Dict exposing (Dict)
import Element exposing (..)
import Element.Events exposing (..)
import Html exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Http
import SyntaxHighlight exposing (elm, toBlockHtml)

numberOfEditors = 9

main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    Dict Int String

zip : List a -> List b -> List (a, b)
zip xs ys =
  List.map2 Tuple.pair xs ys

init : () -> ( Model, Cmd Msg )
init _ =
    (
    Dict.fromList (zip (List.range 1 numberOfEditors) (List.repeat numberOfEditors ""))
    , getCode )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateContent key newContent ->
            ( Dict.update key (\_ -> Just newContent) model, Cmd.none)

        CodeResponse result ->
            ( Dict.update 1 (\_ -> Just (Result.withDefault "" result)) model, Cmd.none)



type Msg
    = UpdateContent Int String
    | CodeResponse (Result Http.Error String)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


getCode =
    Http.get
        { url = "https://gist.githubusercontent.com/virtuaCode/709989669f002a4755758b76588625b3/raw/cc6ee7967a7bc28f487958c88ecb84b501a78a7a/tidal-01.hs"
        , expect = Http.expectString CodeResponse
        }


view : Model -> Html Msg
view model =
    layout [ height fill, width fill ] <|
        column [ height fill, width fill ]
            [el []
                (html <| Html.node "style" [] [ Html.text "pre {margin: 0px; word-break: break-all; white-space: pre-wrap;} .s {} " ])
            , el
                [ height fill, width fill ]
                (html <| viewEditor model 1)
            ]


viewEditor : Model -> Int -> Html Msg
viewEditor model key =
    let
        elements =
            case Dict.get key model of
                 Nothing ->
                   []
                 Just c ->
                    [ elm c
                        |> Result.map (toBlockHtml (Just 1))
                        |> Result.withDefault
                            (Html.code
                                []
                                [ Html.text "isEmpty : String -> Bool" ]
                            )
                    , Html.textarea
                        [ A.style "position" "absolute"
                        , A.style "top" "0px"
                        , A.style "left" "0px"
                        , A.style "border-size" "0px"
                        , A.style "background-color" "transparent"
                        , A.style "resize" "none"
                        , A.style "overflow" "hidden"
                        , A.style "width" "80%"
                        , A.style "height" "100%"
                        , A.style "margin" "0"
                        , A.style "padding" "1rem"
                        , A.style "box-sizing" "border-box"
                        , A.style "opacity" "1"
                        , A.style "color" "transparent"
                        , A.style "white-space" "pre-wrap"
                        , A.style "font-family" "monospace"
                        , A.style "caret-color" "white"
                        , A.style "word-break" "break-all"
                        , A.class "elmsh"
                        , A.class "s"
                        , E.onInput (UpdateContent key)
                        ]
                        [ Html.text c
                        ]
                    ]
    in
    Html.div
        [ A.attribute "spellcheck" "false"
        , A.style "background-color" "#2b2b2b"
        , A.style "padding" "1rem"
        , A.style "flex-grow" "100000"
        , A.style "width" "80%"
        ]
        elements
