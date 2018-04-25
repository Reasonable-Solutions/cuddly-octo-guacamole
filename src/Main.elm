module Main exposing (..)

import Array as Array exposing (fromList, get, set)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


---- MODEL ----


type Player
    = X
    | O


type alias Model =
    { grid : Array.Array (Maybe Player)
    , turn : Player
    }


init : ( Model, Cmd Msg )
init =
    ( { grid =
            fromList
                [ Nothing
                , Nothing
                , Nothing
                , Nothing
                , Nothing
                , Nothing
                , Nothing
                , Nothing
                , Nothing
                ]
      , turn = O
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Noop
    | Play Player Int
    | Win Player


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        Play p pos ->
            case p of
                O ->
                    ( { model | turn = X, grid = set pos (Just O) model.grid }
                    , case model.grid |> win of
                        Nothing ->
                            Cmd.none

                        Just win ->
                            Winner win
                    )

                X ->
                    ( { model | turn = O, grid = set pos (Just X) model.grid }, Cmd.none )



---- VIEW ----


playSquare : Model -> Int -> Html Msg
playSquare m n =
    span
        [ class
            (case get n m.grid of
                Just p ->
                    case p of
                        Just p ->
                            toString p

                        Nothing ->
                            "nothing"

                Nothing ->
                    "nothing"
            )
        , onClick <| Play m.turn n
        ]
        [ text <| toString n ]


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ playSquare model 0
            , playSquare model 1
            , playSquare model 2
            ]
        , div []
            [ playSquare model 3
            , playSquare model 4
            , playSquare model 5
            ]
        , div []
            [ playSquare model 6
            , playSquare model 7
            , playSquare model 8
            ]
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
