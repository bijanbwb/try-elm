module SignupForm exposing (..)

import Html.App
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (id, type', for, value, class)
import Http

view model =
    form [ id "signup-form" ]
        [ h1 [] [ text "Signup Form" ]
        , label [ for "username-field" ] [ text "username: " ]
        , input
            [ id "username-field"
            , type' "text"
            , value model.username
            , onInput (\str -> { msgType = "SET_USERNAME", payload = str })
            ]
            []
        , div [ class "validation-error" ] [ text model.errors.username ]
        , label [ for "password" ] [ text "password: " ]
        , input
            [ id "password-field"
            , type' "password"
            , value model.password
            , onInput (\str -> { msgType = "SET_PASSWORD", payload = str })
            ]
            []
        , div [ class "validation-error" ] [ text model.errors.password ]
        , div [ class "signup-button", onClick { msgType = "VALIDATE", payload = "" } ] [ text "Sign Up!" ]
        ]

getErrors model =
    { username =
        if model.username == "" then
            "Please enter a username!"
        else
            ""
    , password =
        if model.password == "" then
            "Please enter a password!"
        else
            ""
    }

update msg model =
    if msg.msgType == "VALIDATE" then
        ( { model | errors = getErrors model }, Cmd.none )
    else if msg.msgType == "SET_USERNAME" then
        ( { model | username = msg.payload }, Cmd.none )
    else if msg.msgType == "SET_PASSWORD" then
        ( { model | password = msg.payload }, Cmd.none )
    else
        ( model, Cmd.none )

initialErrors =
    { username = "", password = "" }

initialModel =
    { username = "", password = "", errors = initialErrors }

main =
    Html.App.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

