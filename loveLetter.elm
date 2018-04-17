module Main exposing (..)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Style exposing (..)
import Random exposing (Seed, generate)
import Random.Array exposing (shuffle)
import Array exposing (Array, get, set, slice, length)


type alias Card =
    String


type alias PlayerId =
    Int


type alias Player =
    { hand : List Card
    , discard : List Card
    }


type alias Model =
    { deck : Array Card
    , currentPlayer : PlayerId
    , players : Array Player
    }


playerStartSetting =
    { hand = [], discard = [] }


numberOfPlayers =
    4


init : ( Model, Cmd Msg )
init =
    ( Model
        Array.empty
        0
        (Array.repeat numberOfPlayers playerStartSetting)
    , (generate ShuffledDeck
        (shuffle
            (Array.fromList <|
                List.repeat 5 "Guard"
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
    = Draw PlayerId
    | Play Card
    | ShuffleDeck
    | ShuffledDeck (Array Card)
    | StartGame


getPlayerById : Model -> Int -> Player
getPlayerById model playerId =
    Maybe.withDefault playerStartSetting (get playerId model.players)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        oldPlayer =
            getPlayerById model model.currentPlayer
    in
        case msg of
            Draw playerId ->
                let
                    chosenPlayer =
                        getPlayerById model playerId

                    newPlayer =
                        { oldPlayer
                            | hand = List.append chosenPlayer.hand (Array.toList <| slice 0 1 model.deck)
                        }
                in
                    ( { model
                        | deck = slice 1 (length model.deck) model.deck
                        , players = Array.set model.currentPlayer newPlayer model.players
                      }
                    , Cmd.none
                    )

            Play card ->
                let
                    newPlayer =
                        { oldPlayer
                            | hand = List.drop 1 oldPlayer.hand
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

            StartGame ->
                ( { model
                    | players =
                        Array.indexedMap
                            (\index player ->
                                { player
                                    | hand = List.append player.hand (Array.toList <| slice index (index + 1) model.deck)
                                }
                            )
                            model.players
                    , deck = slice numberOfPlayers (length model.deck) model.deck
                  }
                , Cmd.none
                )


view : Model -> Html Msg
view model =
    let
        currentPlayer =
            getPlayerById model model.currentPlayer
    in
        div []
            [ text ("Current Player: " ++ toString model.currentPlayer)
            , button [ onClick StartGame ] [ text "Start Game" ]
            , button [ onClick (Draw model.currentPlayer) ] [ text "Draw" ]
            , div []
                (List.map (\x -> button [ onClick (Play x) ] [ text x ]) currentPlayer.hand)
            , div [ style [ fontSize ".75em" ] ]
                (Array.toList
                    (Array.map
                        text
                        (Array.indexedMap
                            (\playerNum playerId ->
                                toString playerNum
                                    ++ ": "
                                    ++ String.join ", " playerId.discard
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
