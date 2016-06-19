module SignupForm exposing (..)

import Html.App
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (id, type', for, value, class)
import Http
import Task exposing (Task)
import Json.Decode exposing (succeed)

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
        , div [ class "validation-error" ] [ text (viewUsernameErrors model) ]
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

viewUsernameErrors model =
   if model.errors.usernameTaken then
       "That username is taken!"
   else
       model.errors.username

getErrors model =
    { username =
        if model.username == "" then
            "Please enter a username!"
        else
            ""
    , usernameTaken = model.errors.usernameTaken
    , password =
        if model.password == "" then
            "Please enter a password!"
        else
            ""
    }

update msg model =
    if msg.msgType == "VALIDATE" then
        let
            url =
                "https://api.github.com/users/" ++ model.username

            failureToMsg err =
                { msgType = "USERNAME_AVAILABLE", payload = "" }

            successToMsg result =
                { msgType = "USERNAME_TAKEN", payload = "" }

            request =
                Http.get (succeed "") url

            cmd =
                Task.perform failureToMsg successToMsg request
        in
            ( { model | errors = getErrors model }, cmd )
    else if msg.msgType == "USERNAME_TAKEN" then
        ( withUsernameTaken True model, Cmd.none )
    else if msg.msgType == "USERNAME_AVAILABLE" then
        ( withUsernameTaken False model, Cmd.none )
    else if msg.msgType == "SET_USERNAME" then
        ( { model | username = msg.payload }, Cmd.none )
    else if msg.msgType == "SET_PASSWORD" then
        ( { model | password = msg.payload }, Cmd.none )
    else
        ( model, Cmd.none )

withUsernameTaken isTaken model =
    let
        currentErrors =
            model.errors

        newErrors =
            { currentErrors | usernameTaken = isTaken }
    in
        { model | errors = newErrors }

initialErrors =
    { username = "", password = "", usernameTaken = False }

initialModel =
    { username = "", password = "", errors = initialErrors }

main =
    Html.App.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

