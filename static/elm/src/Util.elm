module Util
    exposing
        ( (=>)
        , onChange
        , onClickWithPosition
        , (@)
        )

import Json.Decode as Json exposing (Decoder)
import Mouse exposing (Position)
import Html exposing (Attribute)
import Html.Events exposing (on, onWithOptions, targetValue)


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


onChange : (String -> msg) -> Attribute msg
onChange tagger =
    on "change" (Json.map tagger targetValue)


onClickWithPosition : (Position -> msg) -> Attribute msg
onClickWithPosition msg =
    onWithOptions
        "click"
        { stopPropagation = True
        , preventDefault = False
        }
        (Json.map msg positionDecoder)


positionDecoder : Json.Decoder Position
positionDecoder =
    Json.map2 Position
        (Json.field "pageX" Json.int)
        (Json.field "pageY" Json.int)


(@) : String -> a -> a
(@) message a =
    Debug.log message a
infixr 0 @
