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
        stdErr =
            "There was a error in processing the request! "
    in
        case err of
            BadUrl str ->
                stdErr
                    ++ "This wasn't your fault though. "
                    ++ "This was an issue with the url."
                    ++ "Contact the developer ASAP to fix this issue."

            Timeout ->
                stdErr ++ "It took too long and timed out. Please check your network connection and try again."

            NetworkError ->
                stdErr ++ "Check your network connection and try again."

            BadStatus res ->
                stdErr ++ res.body

            BadPayload str res ->
                stdErr
                    ++ "This wasn't your fault though. "
                    ++ "Contact the developer ASAP to fix this issue: "
                    ++ res.body


show : Maybe Error -> String
show err =
    Maybe.withDefault "" <| Maybe.map errorMessage err


view : Config msg -> Model a -> Html msg
view config { errorStatus } =
    case errorStatus of
        Nothing ->
            div [] []

        Just err ->
            div []
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
