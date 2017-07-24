module Main exposing (..)

import Html exposing (Html, text, div, programWithFlags)
import Html.Attributes exposing (class, style)
import AnimationFrame
import Time
import Array exposing (Array)


type alias Flags =
    { d : String
    }


type alias Col =
    { earth : Float, water : Float, vl : Float, vr : Float }


type alias Model =
    { cols : Array Col, numCols : Int }


type Msg
    = Tick Float


newCol : Int -> Int -> Col
newCol total i =
    if True then
        { earth = ((sin <| toFloat i / 3) + 1.5) * 20 + ((sin <| toFloat i + 0.67) + 1.5) * 5
        , water = (((sin <| toFloat i) + 1.5) * 5) + ((sin <| toFloat i) + 1.5)
        , vl = 0.0
        , vr = 0.0
        }
            |> (\col -> { col | vl = height col * 0.001, vr = height col * 0.001 })
    else
        { earth = toFloat i * 100.0 / toFloat total
        , water = (toFloat (total - i) * 100.0 / toFloat (total * 2))
        , vl = 0.0
        , vr = 0.0
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        numCols =
            50

        cols =
            List.range 0 numCols |> Array.fromList |> Array.map (newCol numCols)
    in
        { cols = cols, numCols = numCols } ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            { model | cols = tick model.cols } ! []


tick : Array Col -> Array Col
tick cols =
    Array.indexedMap
        (\i c -> tickCol c (Array.get (i - 1) cols) (Array.get (i + 1) cols))
        cols


tickCol : Col -> Maybe Col -> Maybe Col -> Col
tickCol col left right =
    let
        h =
            height col

        vTransfer =
            0.2

        col_ =
            { col
                | vl = col.vl * (1 - vTransfer) + (right |> Maybe.map .vl |> Maybe.withDefault 0) * vTransfer
                , vr = col.vr * (1 - vTransfer) + (left |> Maybe.map .vr |> Maybe.withDefault 0) * vTransfer
                , water =
                    col.water
                        + ((left |> Maybe.map .vr |> Maybe.withDefault 0) * vTransfer)
                        + ((right |> Maybe.map .vl |> Maybe.withDefault 0) * vTransfer)
                        - (col.vl * vTransfer)
                        - (col.vr * vTransfer)
            }

        transfer =
            case ( left, right ) of
                ( Just l, Just r ) ->
                    (waterTransfer col_ l) + (waterTransfer col_ r)

                ( Just l, Nothing ) ->
                    waterTransfer col_ l

                ( Nothing, Just r ) ->
                    waterTransfer col_ r

                ( Nothing, Nothing ) ->
                    0
    in
        { col_
            | water =
                (col_.water + transfer - 0.0)
                    |> max 0
            , vl = col_.vl * 0.99
            , vr = col_.vr * 0.99
        }


waterTransfer : Col -> Col -> Float
waterTransfer mine neighbour =
    let
        diff =
            height mine - height neighbour
    in
        if diff > 0 then
            min mine.water -(diff * 0.2)
        else if diff < 0 then
            min neighbour.water -(diff * 0.2)
        else
            0


height : Col -> Float
height c =
    c.earth + c.water


view : Model -> Html Msg
view model =
    div [ class "cols" ]
        (model.cols
            |> Array.map (viewCol model.numCols)
            |> Array.toList
        )


viewCol : Int -> Col -> Html Msg
viewCol total col =
    div [ class "col", style [ "width" => toString (100.0 / toFloat total) ++ "%" ] ]
        [ div
            [ class "col-water"
            , style
                [ "transform" => translateY col.earth ++ scaleY (col.water / 100)
                ]
            ]
            [ text <| toString col.water ]
        , div
            [ class "col-earth"
            , style
                [ "transform" => "scaleY(" ++ toString (col.earth / 100) ++ ")" ]
            ]
            []
        ]


scaleY : Float -> String
scaleY s =
    "scaleY(" ++ toString s ++ ")"


translateY : Float -> String
translateY t =
    "translateY(-" ++ toString t ++ "%)"


subscriptions : Model -> Sub Msg
subscriptions model =
    if False then
        Time.every (300 * Time.millisecond) Tick
    else
        AnimationFrame.times Tick


main : Program Flags Model Msg
main =
    programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


{-| infixl 0 means the (=>) operator has the same precedence as (<|) and (|>),
meaning you can use it at the end of a pipeline and have the precedence work out.
-}
infixl 0 =>
