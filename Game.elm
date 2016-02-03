module Game where

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Signal exposing (Mailbox, Address, mailbox)
import Time exposing (..)
import String exposing (repeat)
import Random

-- SIGNALS

main : Signal Html
main =
  Signal.map (view inputs.address) (Signal.foldp update init mergedSignals)

ticker : Signal Time
ticker =
  every second

mergedSignals : Signal Action
mergedSignals =
  Signal.mergeMany
    [ Signal.map Tick ticker
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
  , lane : Lane
  }

init : Model
init =
  Model 0 0 1 0 0 emptyLane


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
      handleTick (delta / 1000) model
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
  let 
    clicks = clicksPerDelta delta model
  in
    {model| bank = (model.bank + clicks) - (toFloat <| laneDamage <| moveEnemies model.lane),
            lane = moveEnemies <| addBarbarian model model.lane
    }

workerValue : Model -> Float
workerValue model =
  model.workers

towerValue : Model -> Float
towerValue model =
  model.towers * 6

clicksPerDelta : Time -> Model -> Float
clicksPerDelta delta model =
  ((workerValue model) + (towerValue model)) 


clicksPerSecond : Model -> Float
clicksPerSecond model =
  clicksPerDelta second model

type alias Enemy =
  { kind : String
  , power : Int
  , speed : Int
  , location : Int
  }

barbarian : Enemy
barbarian = 
  { kind = "Barbarian"
  , power = 1
  , speed = 1
  , location = -1
  }

moveEnemy : Enemy -> Enemy
moveEnemy enemy =
  {enemy| location = enemy.location + enemy.speed}

type alias Lane =
  { length : Int
  , enemies : List Enemy
  }

emptyLane : Lane
emptyLane = 
  { length = 10
  , enemies = []
  }

addBarbarian : Model -> Lane -> Lane
addBarbarian model lane =
  if shouldAddBarbarian model then
    {lane| enemies = barbarian :: lane.enemies}
  else
    lane

moveEnemies : Lane -> Lane
moveEnemies lane =
  {lane| enemies = List.filter (not << (reachedBase lane)) <| List.map moveEnemy lane.enemies}

laneDamage : Lane -> Int
laneDamage lane =
  let
    reached = List.filter (reachedBase lane) <| .enemies <| moveEnemiesKeep lane
  in
    List.sum <| List.map .power reached

moveEnemiesKeep : Lane -> Lane
moveEnemiesKeep lane =
  {lane| enemies = List.map moveEnemy lane.enemies}

reachedBase : Lane -> Enemy -> Bool
reachedBase lane enemy =
  lane.length < enemy.location


shouldAddBarbarian : Model -> Bool
shouldAddBarbarian model =
  (fst >> ((==) 1)) (Random.generate (Random.int 0 3) (Random.initialSeed <| floor model.bank))

renderLane : Lane -> Html
renderLane lane =
  table [] <|
    List.reverse <| List.map (renderLaneSection lane) <| [0..lane.length]

renderLaneSection : Lane -> Int -> Html
renderLaneSection lane index =
  tr []
   [td [] [text <| "|" ++ ( toString <| enemiesAt lane index ) ++ "|"]]

enemiesAt : Lane -> Int -> Int
enemiesAt lane i =
  List.length <| List.filter (.location >> ((==) i) ) lane.enemies

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
    , renderLane model.lane
    ]


