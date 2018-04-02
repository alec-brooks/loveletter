module Main exposing (..)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Style exposing (..)
import Random exposing (Seed, generate)
import Random.List exposing (shuffle)
import Array exposing (Array, get, set)


type alias Card =
    String


type alias Model =
    { deck : List Card
    , currentPlayer : Int
    , players :
        Array
            { hand : List Card
            , discard : List Card
            }
    }


playerStartSetting =
    { hand = [], discard = [] }


numberOfPlayers =
    4


init : ( Model, Cmd Msg )
init =
    ( Model
        []
        0
        (Array.repeat numberOfPlayers playerStartSetting)
    , (generate ShuffledDeck
        (shuffle
            (List.repeat 5 "Guard"
                ++ List.repeat 2 "Royal Subject"
                ++ List.repeat 2 "Gossip"
                ++ List.repeat 2 "Companion"
                ++ List.repeat 2 "Hero"
                ++ [ "Wizard" ]
                ++ [ "Lady" ]
                ++ [ "Princess" ]
            )
        )
      )
    )


type Msg
    = Draw
    | Play Card
    | ShuffleDeck
    | ShuffledDeck (List Card)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        oldPlayer =
            Maybe.withDefault playerStartSetting (get model.currentPlayer model.players)
    in
        case msg of
            Draw ->
                let
                    newPlayer =
                        { oldPlayer
                            | hand = List.append oldPlayer.hand (List.take 1 model.deck)
                        }
                in
                    ( { model
                        | deck = Maybe.withDefault [] (List.tail model.deck)
                        , players = Array.set model.currentPlayer newPlayer model.players
                      }
                    , Cmd.none
                    )

            Play card ->
                let
                    newPlayer =
                        { oldPlayer
                            | hand = Maybe.withDefault [] (List.tail oldPlayer.hand)
                            , discard = List.append oldPlayer.discard (List.take 1 oldPlayer.hand)
                        }
                in
                    ( { model
                        | players = Array.set model.currentPlayer newPlayer model.players
                        , currentPlayer = (model.currentPlayer + 1) % numberOfPlayers
                      }
                    , Cmd.none
                    )

            ShuffleDeck ->
                ( model, generate ShuffledDeck (shuffle model.deck) )

            ShuffledDeck shuffledDeck ->
                { model | deck = shuffledDeck } ! []


view : Model -> Html Msg
view model =
    let
        currentPlayer =
            Maybe.withDefault playerStartSetting (get model.currentPlayer model.players)
    in
        div []
            [ text ("Current Player: " ++ toString model.currentPlayer)
            , button [ onClick Draw ] [ text "Draw" ]
            , div []
                (List.map (\x -> button [ onClick (Play x) ] [ text x ]) currentPlayer.hand)
            , div [ style [ fontSize ".75em" ] ]
                (Array.toList
                    (Array.map
                        text
                        (Array.indexedMap
                            (\playerNum player ->
                                toString playerNum
                                    ++ ": "
                                    ++ String.join ", " player.discard
                            )
                            model.players
                        )
                    )
                )
            ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
