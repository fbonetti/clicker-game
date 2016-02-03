module Game where

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Signal exposing (Mailbox, Address, mailbox)
import Time exposing (..)
import String exposing (repeat)

-- SIGNALS

main : Signal Html
main =
  Signal.map (view inputs.address) (Signal.foldp update init mergedSignals)

elapsedSeconds : Signal Time
elapsedSeconds =
  Signal.map (\t -> t / 1000) (fps 60)

mergedSignals : Signal Action
mergedSignals =
  Signal.mergeMany
    [ Signal.map Tick elapsedSeconds
    , inputs.signal
    ]


-- MAILBOX

inputs : Signal.Mailbox Action
inputs =
  Signal.mailbox NoOp

-- MODEL

type alias Model =
  { total : Float
  , bank : Float
  , clickRate : Float
  , workers : Float
  , towers : Float
  }

init : Model
init =
  Model 0 0 1 0 0


-- ACTIONS

type Action
    = NoOp
    | Increment
    | Tick Float
    | BuyWorker
    | BuyTower

-- HELPERS

workerCost : Model -> Float
workerCost model =
  (ceiling >> toFloat) (15 * (1.15 ^ model.workers))

towerCost : Model -> Float
towerCost model =
  (ceiling >> toFloat) (100 * (1.15 ^ model.towers))

-- UPDATE

update : Action -> Model -> Model
update action model =
  case action of
    Increment ->
      { model | bank = model.bank + model.clickRate }
    Tick delta ->
      handleTick delta model
    BuyWorker ->
      if model.bank >= (workerCost model) then
        { model | workers = model.workers + 1, bank = model.bank - (workerCost model) }
      else
        model
    BuyTower ->
      if model.bank >= (towerCost model) then
        { model | towers = model.towers + 1, bank = model.bank - (towerCost model) }
      else
        model
    NoOp ->
      model

handleTick : Time -> Model -> Model
handleTick delta model =
  let clicks = clicksPerDelta delta model
  in {model| bank = model.bank + clicks }

workerValue : Model -> Float
workerValue model =
  model.workers

towerValue : Model -> Float
towerValue model =
  model.towers * 6

clicksPerDelta : Time -> Model -> Float
clicksPerDelta delta model =
  ((workerValue model) + (towerValue model)) * delta 


clicksPerSecond : Model -> Float
clicksPerSecond =
  clicksPerDelta 1


-- STYLES

bodyStyle : Attribute
bodyStyle =
  style
    [ ("width", "600px")
    , ("margin", "auto")
    ]

blackText : Attribute
blackText =
  style [ ("color", "black") ]

greyText : Attribute
greyText =
  style [ ("color", "grey") ]

-- VIEWS

strText : a -> Html
strText value =
  (toString >> text) value

view : Address Action -> Model -> Html
view address model =
  div [ bodyStyle ]
    [ div [ onClick address Increment ] [ text "click me" ]
    , div [] [ strText (floor model.bank) ]
    , div [] [ text <| "+ " ++ (toString <| clicksPerSecond model) ++ " per second" ]
    , table []
      [ tr [ if model.bank < (workerCost model) then greyText else blackText ]
        [ td [] [ strText (workerCost model) ]
        , td [] [ text "+1" ]
        , td
          [ onClick address BuyWorker ]
          [ text "worker" ]
        , td [] [ strText model.workers ]
        , td [] [ text <| repeat (floor model.workers) "." ]
        ]
      , tr [ if model.bank <= (towerCost model) then greyText else blackText ]
        [ td [] [ strText (towerCost model) ]
        , td [] [ text "+6" ]
        , td
          [ onClick address BuyTower ]
          [ text "tower" ]
        , td [] [ strText model.towers ]
        , td [] [ text <| repeat (floor model.towers) "|" ]
        ]
      ]
    ]