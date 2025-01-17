module Main exposing (..)

import UI

import Browser
import Html exposing (Html, div, text, span, input)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onInput)
import Http
import List exposing (append)

main : Program () Model Msg
main = Browser.element { 
    init = init, update = update, view = view, subscriptions = subscriptions
    }

type alias ChatMessage = { author: String, text: String }
type alias Model = { chat: List ChatMessage, last: String, curMessage: String }
type Msg = GotResponse (Result Http.Error String) 
    | ResetChat | SendMessage String | SetCurMessage String

init : () -> (Model, Cmd Msg)
init _ = (Model [] "" "", Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

update : Msg -> Model -> (Model, Cmd Msg) 
update msg model = case msg of 
    GotResponse response -> (case response of 
        Ok result -> ({ model | last = result }, Cmd.none)
        Err _ -> ({ model | last = "Error" }, Cmd.none))
    ResetChat -> (Model [] "" "", Cmd.none)
    SendMessage text -> ({ model | 
        chat = append model.chat [ChatMessage "You" text],
        curMessage = ""
        }, Cmd.none)
    SetCurMessage text -> ({ model | curMessage = text}, Cmd.none)

view : Model -> Html Msg
view ({chat, last, curMessage} as model) = UI.fullscreen
    <| UI.centered
    <| UI.column
    <| [ 
        UI.card (UI.title "Elm-Powered ChatGPT Client"),
        UI.optionCard [
            UI.Clickable "Reset" ResetChat UI.Reject,
            UI.Clickable "Send" (SendMessage curMessage) UI.Accept
        ] ( div [] ((viewChat chat) ++ [
            div [] [
                input [ 
                    value curMessage, 
                    onInput SetCurMessage, style "width" "120px"
                ] []
            ]
        ]))
    ]

viewChat : List ChatMessage -> List (Html Msg)
viewChat chat = case chat of 
    [] -> [div [] []] -- only hit when there are no messages at all
    [c] -> [div [] [text c.author, text c.text]]
    c :: cs -> div [] [text c.author, text c.text] :: viewChat cs