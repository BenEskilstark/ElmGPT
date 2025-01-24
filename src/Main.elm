module Main exposing (..)

import UI
import Secrets

import Browser
import Html exposing (Html, div, text, span, b, input)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onInput)
import Http
import List exposing (append, head, tail, reverse)
import Json.Decode exposing (Decoder, map2, field)
import Json.Encode as E
import Maybe exposing (withDefault)



main : Program () Model Msg
main = Browser.element { init = init, update = update, view = view, subscriptions = subs }

type alias ChatMessage = { role: String, content: String }
type alias Chat = { 
    messages: List ChatMessage,
    model: String
    }
type alias Model = { chat: Chat, curMessage: String }
type Msg = GotResponse (Result Http.Error (List String))
    | ResetChat | SendMessage String | Undo
    | SetCurMessage String

init : () -> (Model, Cmd Msg)
init _ = (Model (Chat [] "gpt-4-0613") "", Cmd.none)

subs : Model -> Sub Msg
subs _ = Sub.none



update : Msg -> Model -> (Model, Cmd Msg) 
update msg ({chat} as model) = case msg of 
    GotResponse response -> (case response of 
        Ok result -> ({ model | 
            chat = addMessage "assistant" (head result |> withDefault "") chat
            }, Cmd.none)
        Err errType -> (case errType of 
            Http.BadUrl err -> ({ model | chat = addMessage "Error" err chat}, Cmd.none)
            Http.Timeout -> ({ model | chat = addMessage "Error" "Request Timeout" chat}, Cmd.none)
            Http.NetworkError -> ({ model | chat = addMessage "Error" "Network Error" chat}, Cmd.none)
            Http.BadStatus int -> ({ model | chat = addMessage "Error" ("Status: " ++ (String.fromInt int)) chat}, Cmd.none)
            Http.BadBody err -> ({ model | chat = addMessage "Error" err chat}, Cmd.none)
            )
        )
    ResetChat -> (Model (Chat [] "gpt-4-0613") "", Cmd.none)
    Undo -> ({ model | chat = undoMessage chat, curMessage = ""}, Cmd.none)
    SendMessage text -> (
        { model | chat = addMessage "user" text chat, curMessage = ""}, 
        apiRequest (addMessage "user" text chat)
        )
    SetCurMessage text -> ({ model | curMessage = text}, Cmd.none)

addMessage : String -> String -> Chat -> Chat
addMessage role msg chat = {chat | messages = append chat.messages [ChatMessage (role ++ "") msg] }

undoMessage : Chat -> Chat
undoMessage ({messages} as chat) = {chat | messages = 
    reverse messages |> tail |> withDefault [] |> reverse
    }

-- api request

apiRequest : Chat -> Cmd Msg
apiRequest chat = 
    Http.request {
        method = "POST",
        url = "https://api.openai.com/v1/chat/completions",
        headers = [
            -- Http.header "Content-Type" "application/json",
            Http.header "Authorization" ("Bearer " ++ Secrets.apiKey)
        ],
        body = Http.jsonBody (encodeChat chat),
        expect = Http.expectJson GotResponse choicesDecoder,
        timeout = Nothing,
        tracker = Nothing
        }

encodeChat : Chat -> E.Value
encodeChat chat =
  E.object [ 
    ("model", E.string chat.model),
    ("messages", ((E.list encodeMessage) chat.messages))
    ]

encodeMessage : ChatMessage -> E.Value
encodeMessage {role, content} = E.object [
    ("role", E.string role), 
    ("content", E.string content)
    ]

choicesDecoder : Decoder (List String)
choicesDecoder = field "choices" (Json.Decode.list messageDecoder)

messageDecoder : Decoder String
messageDecoder = field "message" (field "content" Json.Decode.string)

-- keypress handling

onEnter : Msg -> Html.Attribute Msg
onEnter msg = 
    let isEnter : Decoder Bool 
        isEnter = field "key" Json.Decode.string |> Json.Decode.map (\key -> key == "Enter") 
    in Html.Events.on "keydown" (Json.Decode.andThen 
        (\isEnterKey -> if isEnterKey then Json.Decode.succeed msg else Json.Decode.fail "Not enter key") 
    isEnter)


-- view

view : Model -> Html Msg
view ({chat, curMessage} as model) = UI.fullscreen
    <| UI.centered
    <| UI.column
    <| [ 
        UI.card (UI.title "Elm-Powered ChatGPT Client"),
        UI.optionCard [
            UI.Clickable "Reset" ResetChat UI.Reject,
            UI.Clickable "Undo" Undo UI.Default,
            UI.Clickable "Send" (SendMessage curMessage) UI.Accept
        ] ( div [] ((viewMessages chat.messages) ++ [
            div [] [
                input [ 
                    value curMessage, 
                    onInput SetCurMessage, onEnter (SendMessage curMessage),
                    style "width" "120px"
                ] []
            ]
        ]))
    ]

viewMessages : List ChatMessage -> List (Html Msg)
viewMessages chat = case chat of 
    [] -> [div [] []] -- only hit when there are no messages at all
    [c] -> [div [] [b [] [ text (c.role ++ ":")], text c.content]]
    c :: cs -> div [] [b [] [ text (c.role ++ ":")], text c.content] :: viewMessages cs