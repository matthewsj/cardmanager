port module Main exposing (..)

import CloudModel exposing (RejectionStrategy(..), localAction, sharedAction)
import Html exposing (Html, button, div, input, li, text, ul)
import Html.Attributes as Attrs exposing (class)
import Html.Events exposing (onClick, onInput)
import Json.Decode exposing (Decoder, oneOf)
import Json.Encode
import List.Extra



-- MAIN


main =
    CloudModel.element
        { sharedMsgEncoding =
            { decoder = decodeSharedMsg
            , encoder = encodeSharedMsg
            , onDecodeError = \_ -> NoOp
            }
        , application =
            { init = init
            , updateCloud = updateShared
            , updateLocal = updateLocal
            , subscriptions = \_ _ -> Sub.none
            , view = view
            }
        , ports =
            { proposal = proposal
            , proposalResponse = proposalResponse
            , receiveEvents = receiveEvents
            }
        , options = { rejectionStrategy = ReapplyAllPending }
        }



-- MODEL


{-| The model that is shared across all clients.
-}
type alias SharedModel =
    List Player


type alias PlayerId =
    String


type alias Player =
    { name : PlayerId
    , cards : List Card
    }


type alias Card =
    String


{-| The model that is local to this individual client.
-}
type LocalModel
    = AddingPlayer AddingPlayerModel
    | AddingCard AddingCardModel
    | Displaying


type alias AddingPlayerModel =
    PlayerId


type alias AddingCardModel =
    { playerToAddTo : PlayerId
    , cardText : String
    }



-- INIT


{-| Initial values for shared and local models as well as local commands to
run on start.
-}
init : () -> ( SharedModel, LocalModel, Cmd LocalMsg )
init () =
    ( initSharedModel
    , initLocalModel
    , Cmd.none
    )


{-| The initial shared model.
-}
initSharedModel : SharedModel
initSharedModel =
    []


{-| The initial local model.
-}
initLocalModel : LocalModel
initLocalModel =
    Displaying



-- SHARED UPDATE


{-| Shared messages. These messages update the shared model and we must write
encoders and decoders for them so that they can be communicated over the
network.
-}
type SharedMsg
    = AddPlayer PlayerId
    | RemovePlayer PlayerId
    | AddCardToPlayer PlayerId String
    | RemoveCardFromPlayer PlayerId Int


{-| Encodes shared messages to JSON.
-}
encodeSharedMsg : SharedMsg -> Json.Encode.Value
encodeSharedMsg sharedModelMsg =
    case sharedModelMsg of
        AddPlayer playerId ->
            Json.Encode.object [ ( "AddPlayer", Json.Encode.string playerId ) ]

        RemovePlayer playerId ->
            Json.Encode.object [ ( "RemovePlayer", Json.Encode.string playerId ) ]

        AddCardToPlayer playerId cardText ->
            Json.Encode.object
                [ ( "AddCardToPlayer"
                  , Json.Encode.object
                        [ ( "playerId", Json.Encode.string playerId ), ( "cardText", Json.Encode.string cardText ) ]
                  )
                ]

        RemoveCardFromPlayer playerId position ->
            Json.Encode.object
                [ ( "RemoveCardFromPlayer"
                  , Json.Encode.object
                        [ ( "playerId", Json.Encode.string playerId ), ( "position", Json.Encode.int position ) ]
                  )
                ]


{-| Decodes shared messages from the JSON representation written by
`encodeSharedMsg`.
-}
decodeSharedMsg : Decoder SharedMsg
decodeSharedMsg =
    oneOf
        [ Json.Decode.field "AddPlayer" (Json.Decode.map AddPlayer Json.Decode.string)
        , Json.Decode.field "RemovePlayer" (Json.Decode.map RemovePlayer Json.Decode.string)
        , Json.Decode.field "AddCardToPlayer"
            (Json.Decode.map2 AddCardToPlayer
                (Json.Decode.field "playerId" Json.Decode.string)
                (Json.Decode.field "cardText" Json.Decode.string)
            )
        , Json.Decode.field "RemoveCardFromPlayer"
            (Json.Decode.map2 RemoveCardFromPlayer
                (Json.Decode.field "playerId" Json.Decode.string)
                (Json.Decode.field "position" Json.Decode.int)
            )
        ]


addLast : a -> List a -> List a
addLast item list =
    List.append list [ item ]


{-| Updates the shared model. Shared model updates are not allowed to issue
commands.
-}
updateShared : SharedMsg -> SharedModel -> SharedModel
updateShared msg model =
    let
        updatePlayer : PlayerId -> (Player -> Player) -> SharedModel
        updatePlayer playerId updateFn =
            List.map (updateOnlyId playerId updateFn)
                model

        updateOnlyId : PlayerId -> (Player -> Player) -> Player -> Player
        updateOnlyId id updateFn player =
            if player.name == id then
                updateFn player

            else
                player
    in
    case msg of
        AddPlayer id ->
            if List.any (\player -> player.name == id) model then
                model

            else
                List.append model [ Player id [] ]

        RemovePlayer id ->
            List.filter (\player -> player.name /= id) model

        AddCardToPlayer id card ->
            updatePlayer id (\player -> { player | cards = addLast card player.cards })

        RemoveCardFromPlayer id cardIndex ->
            updatePlayer id
                (\player ->
                    { player | cards = List.Extra.removeAt cardIndex player.cards }
                )



-- LOCAL UPDATE


type LocalMsg
    = SetMode LocalModel
    | NoOp


{-| Local messages. These messages are applied to the local model only and may
issue commands.
-}
updateLocal : LocalMsg -> LocalModel -> ( LocalModel, Cmd msg )
updateLocal msg model =
    case msg of
        SetMode mode ->
            ( mode, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- VIEW


type alias LocalOriginMsg =
    CloudModel.LocalOriginAction SharedMsg LocalMsg


view : SharedModel -> LocalModel -> Html (CloudModel.LocalOriginAction SharedMsg LocalMsg)
view sharedModel localModel =
    div [ class "Application" ]
        [ div [ class "PlayerList" ]
            (List.map viewPlayer sharedModel)
        , div [ class "controls" ]
            [ viewControls localModel ]
        ]


viewControls : LocalModel -> Html LocalOriginMsg
viewControls localModel =
    let
        default =
            div [ class "controls" ]
                [ button
                    [ onClick (localAction (SetMode (AddingPlayer ""))) ]
                    [ text "Add player" ]
                ]
    in
    case localModel of
        Displaying ->
            default

        AddingPlayer name ->
            div [ class "controls" ]
                [ Html.h3 [] [ text "Adding player" ]
                , input [ Attrs.type_ "text", Html.Events.onInput (\text -> localAction (SetMode (AddingPlayer text))) ] []
                , button
                    [ onClick (action (SetMode Displaying) (AddPlayer name)) ]
                    [ text "Add player" ]
                ]

        AddingCard { playerToAddTo, cardText } ->
            div [ class "controls" ]
                [ Html.h3 [] [ text ("Adding card to player " ++ playerToAddTo) ]
                , input [ Attrs.type_ "text", Html.Events.onInput (\text -> localAction (SetMode (AddingCard { playerToAddTo = playerToAddTo, cardText = text }))) ] []
                , button
                    [ onClick (action (SetMode Displaying) (AddCardToPlayer playerToAddTo cardText)) ]
                    [ text "Add player" ]
                ]


viewPlayer : Player -> Html LocalOriginMsg
viewPlayer { name, cards } =
    div [ class "Player" ]
        [ Html.h2 [] [ text name ]
        , ul [ class "CardList" ]
            (List.indexedMap
                (\index cardText ->
                    li [] [ text cardText, button [ onClick (sharedAction (RemoveCardFromPlayer name index)) ] [ text "Remove" ] ]
                )
                cards
            )
        , button
            [ onClick (localAction (SetMode (AddingCard { playerToAddTo = name, cardText = "" }))) ]
            [ text "Add a card" ]
        ]


action : localMsg -> sharedMsg -> CloudModel.LocalOriginAction sharedMsg localMsg
action local remote =
    { localMsg = Just local, proposedEvent = Just remote }


port proposal : Json.Encode.Value -> Cmd msg


port proposalResponse : (Json.Decode.Value -> msg) -> Sub msg


port receiveEvents : (Json.Decode.Value -> msg) -> Sub msg
