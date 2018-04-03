module View.Error exposing (view)

import Html exposing (Html, div, text, button)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http exposing (Error(..))


-- type Error
-- = BadUrl String
-- | Timeout
-- | NetworkError
-- | BadStatus (Response String)
-- | BadPayload String (Response String)


type alias Model a =
    { a | errorStatus : Maybe Error }


type alias Config msg =
    { onRetry : msg }


errorMessage : Error -> String
errorMessage err =
    let
        e =
            "There was a error in processing the request! "
    in
        case err of
            BadUrl str ->
                e
                    ++ "This wasn't your fault though. "
                    ++ "This was an issue with the url."
                    ++ "Contact an admin for immediate assistance."

            Timeout ->
                e ++ "It took too long and timed out. Please check your network connection and try again."

            NetworkError ->
                e ++ "Check your network connection and try again."

            BadStatus res ->
                e
                    ++ "Here's what I know: "
                    ++ (toString res.status.code)
                    ++ " "
                    ++ res.status.message

            BadPayload str res ->
                e
                    ++ "This wasn't your fault though. "
                    ++ "The data was not in the correct format. "
                    ++ "Contact an admin for immediate assistance."


show : Maybe Error -> String
show err =
    Maybe.withDefault "" <| Maybe.map errorMessage err


view : Config msg -> Model a -> Html msg
view config { errorStatus } =
    case errorStatus of
        Nothing ->
            div [] []

        Just err ->
            div [ class "error-panel-wrapper" ]
                [ div [ class "error" ] [ text <| show errorStatus ]
                , viewRetry config.onRetry
                ]


viewRetry : msg -> Html msg
viewRetry msg =
    div [ class "error-button-wrapper" ]
        [ button
            [ onClick msg ]
            [ text "Retry" ]
        ]
