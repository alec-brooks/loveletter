module Main exposing (..)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Style exposing (..)
import Random exposing (Seed, generate)
import Random.List exposing (shuffle)


-- MODEL


type alias Card =
    String


type alias Model =
    { deck : List Card
    , hand : List Card
    , discard : List Card
    }


init : ( Model, Cmd Msg )
init =
    ( Model
        (List.repeat 5 "Guard"
            ++ List.repeat 2 "Royal Subject"
            ++ List.repeat 2 "Gossip"
            ++ List.repeat 2 "Companion"
            ++ List.repeat 2 "Hero"
            ++ [ "Wizard" ]
            ++ [ "Lady" ]
            ++ [ "Princess" ]
        )
        []
        []
    , Cmd.none
    )


type Msg
    = Draw
    | Play Card
    | ShuffleDeck
    | ShuffledDeck (List Card)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Draw ->
            ( { model
                | deck = Maybe.withDefault [] (List.tail model.deck)
                , hand = List.append model.hand (List.take 1 model.deck)
              }
            , Cmd.none
            )

        Play card ->
            ( { model
                | hand = Maybe.withDefault [] (List.tail model.hand)
                , discard = List.append model.discard (List.take 1 model.hand)
              }
            , Cmd.none
            )

        ShuffleDeck ->
            ( model, generate ShuffledDeck (shuffle model.deck) )

        ShuffledDeck shuffledDeck ->
            { model | deck = shuffledDeck } ! []


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick ShuffleDeck ] [ text "Shuffle Deck" ]
        , button [ onClick Draw ] [ text "Draw" ]
        , div []
            (List.map (\x -> button [ onClick (Play x) ] [ text x ]) model.hand)
        , div [ style [ fontSize ".75em" ] ]
            [ model.discard
                |> String.join ", "
                |> text
            ]
        ]


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
