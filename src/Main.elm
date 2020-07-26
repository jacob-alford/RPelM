module Main exposing (main)

import Array exposing (Array, append, fromList, get, slice)
import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Maybe exposing (withDefault)
import String exposing (fromFloat)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { stack : Array Float
    , tape : List String
    }


init : Model
init =
    { stack = Array.empty, tape = [] }


type Msg
    = Enter Float
    | Drop


update : Msg -> Model -> Model
update msg model =
    let
        fst =
            get 0 model.stack
    in
    case msg of
        Enter num ->
            { stack = prepend num model.stack, tape = cat ( "Enter ", num ) :: model.tape }

        Drop ->
            { stack = unshift 1 model.stack, tape = getNextTape model.tape ( "Dropped ", fst ) }


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick <| Enter 69.0 ] [ text "Enter 69" ]
        , div []
            [ text (fromFloat <| withDefault 0 <| get 0 model.stack) ]
        , button
            [ onClick Drop ]
            [ text "drop" ]
        ]


getNextTape : List String -> ( String, Maybe Float ) -> List String
getNextTape tape ( m, num ) =
    case num of
        Nothing ->
            tape

        Just a ->
            cat ( m, a ) :: tape


cat : ( String, Float ) -> String
cat ( s, n ) =
    s ++ fromFloat n


flip : (a -> b -> c) -> b -> a -> c
flip f b a =
    f a b


unshift : Int -> Array a -> Array a
unshift =
    flip slice 0


prepend : a -> Array a -> Array a
prepend a =
    append <| fromList [ a ]
