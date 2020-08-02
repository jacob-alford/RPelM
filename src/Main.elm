module Main exposing (main)

import Array exposing (Array, append, fromList, get, slice)
import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Maybe
import Maybe.Extra exposing (combine, combineArray)
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
    | Swap
    | Drop


update : Msg -> Model -> Model
update msg model =
    let
        fst =
            get 0 model.stack

        snd =
            get 1 model.stack
    in
    case msg of
        Enter num ->
            { stack = prepend num model.stack, tape = cat ( "Enter ", num ) :: model.tape }

        Drop ->
            { stack = unshift 1 model.stack, tape = getNextTape model.tape ( "Dropped ", fst ) }

        Swap ->
            { stack = maybeAppend (combineArray <| fromList [ snd, fst ]) (unshift 2 model.stack), tape = binNextTape model.tape ( "Swapped ", fst, snd ) }


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick <| Enter 69.0 ] [ text "Enter 69" ]
        , div []
            [ text (fromFloat <| Maybe.withDefault 0 <| get 0 model.stack) ]
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


binNextTape : List String -> ( String, Maybe Float, Maybe Float ) -> List String
binNextTape tape ( m, num1, num2 ) =
    case combine [ num1, num2 ] of
        Nothing ->
            tape

        Just [ n1, n2 ] ->
            catBin ( m, n1, n2 ) :: tape

        Just _ ->
            tape


catBin : ( String, Float, Float ) -> String
catBin ( s, n1, n2 ) =
    s ++ fromFloat n1 ++ ", " ++ fromFloat n2


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


maybeAppend : Maybe (Array a) -> Array a -> Array a
maybeAppend val vals =
    case val of
        Nothing ->
            vals

        Just newVals ->
            append newVals vals
