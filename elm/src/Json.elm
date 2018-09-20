module Json exposing
    ( Color(..)
    , Person
    , decoded
    , example
    , main
    , personDecoder
    )

import Browser
import Html exposing (..)
import Json.Decode as D
import Json.Encode as E


type alias Person =
    { name : String
    , age : Int
    , email : Maybe String
    , colors : List Color
    }


type Color
    = NamedColor String
    | HexColor ( Int, Int, Int )


example =
    """
{ 
    "name": "John",
    "email": null,
    "foo": "bar",
    "colors": ["red", [255, 0, 0]],
    "age": 42
}
"""


colorDecoder : D.Decoder Color
colorDecoder =
    D.oneOf
        [ hexColorDecoder
        , D.map NamedColor D.string
        ]


intColorDecoder : D.Decoder Int
intColorDecoder =
    D.int
        |> D.andThen
            (\x ->
                if x >= 0 && x <= 255 then
                    D.succeed x

                else
                    D.fail "Wrong interval for color"
            )


hexColorDecoder : D.Decoder Color
hexColorDecoder =
    D.list intColorDecoder
        |> D.andThen
            (\x ->
                case x of
                    [ r, g, b ] ->
                        D.succeed (HexColor ( r, g, b ))

                    _ ->
                        D.fail "Hex colors must have exactly 3 components"
            )


personDecoder : D.Decoder Person
personDecoder =
    D.map4 Person
        (D.field "name" D.string)
        (D.field "age" D.int)
        (D.field "email" (D.maybe D.string))
        (D.field "colors" (D.list colorDecoder))


decoded =
    D.decodeString personDecoder example


main =
    div []
        [ div []
            [ pre [] [ text example ] ]
        , div
            []
            [ text (Debug.toString decoded) ]
        ]
