module Main exposing (main)

import Basics as List
import Browser
import Dict exposing (..)
import Html exposing (Attribute, Html, code, div, p, pre, text)
import Html.Attributes exposing (class, spellcheck, tabindex, id, contenteditable)
import Html.Events exposing (on)
import Json.Decode as Json

numberOfEditors = 3

targetInnerText : Json.Decoder String
targetInnerText =
  Json.at ["target", "innerText"] Json.string

onInputInnerText : (String -> msg) -> Attribute msg
onInputInnerText tagger =
  on "input" (Json.map tagger targetInnerText)

-- UPDATE
type Msg
 = ChangeContent Int String

-- updatetUsers = Dict.update 1 (Maybe.map (\name -> "wow...")) editors2
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeContent key newContent ->
        ( Dict.update key (\_ -> Just newContent) model, Cmd.none)


-- MODEL

type alias Model = Dict Int String

type alias Editor =
    { content : String }

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- MAIN

main =
    Browser.element    {
        init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
    }

zip : List a -> List b -> List (a, b)
zip xs ys =
  List.map2 Tuple.pair xs ys

init : () -> (Model, Cmd Msg)
init _ =
  (
     Dict.fromList (zip (List.range 1 numberOfEditors) (List.repeat numberOfEditors ""))

     , Cmd.none
  )

createEditorElement : Int  -> String -> Model -> Html Msg
createEditorElement i cl model =
    code [
        id (String.append "editor" (String.fromInt i))
        , tabindex 5
        , spellcheck False
        , contenteditable True
        , class (String.append "language-tidal" cl)
        , onInputInnerText (ChangeContent i)
        ]
        [text (Maybe.withDefault "" (Dict.get i model))]

createEditorElements : List Int -> Model -> List (Html Msg)
createEditorElements lst model =
       (List.map (\i -> (createEditorElement i " hidden" model)) lst)


createProxyElement : Int -> String -> Model -> Html Msg
createProxyElement i cl model =
      pre [class (String.append "proxy " cl)][
        code
           [id (String.append "proxy" (String.fromInt i))
           , spellcheck False
           , class "language-tidal"]
        [text (Maybe.withDefault "" (Dict.get i model))]
      ]

createProxyElements : List Int -> String -> Model -> List (Html Msg)
createProxyElements lst cl model =
        List.map(\i -> (createProxyElement i cl model)) lst

view : Model -> Html Msg
view model =
 div []
     [ div [class "row1"] [
          div [class "column cw-80"] [
             pre [class "editor"]
             (createEditorElement 1 "" model :: (createEditorElements (List.range 2 numberOfEditors) model))
          ]
          , div [class "column cw-20"]
              (createProxyElement 1 "active" model ::
              (createProxyElements (List.range 2 numberOfEditors) "" model))
        ]
     ]