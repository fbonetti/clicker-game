module Game where

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Signal exposing (Mailbox, Address, mailbox)
import Time exposing (..)
import String exposing (repeat)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Color exposing (..)
import List exposing (..)

-- SIGNALS

main : Signal Html
main =
  Signal.map (view inputs.address) (Signal.foldp update init mergedSignals)

elapsedSeconds : Signal Time
elapsedSeconds =
  Signal.map (\t -> t / 1000) (fps 60)

everyThreeSeconds : Signal Time
everyThreeSeconds =
  every <| 3 * second 

mergedSignals : Signal Action
mergedSignals =
  Signal.mergeMany
    [ Signal.map Tick elapsedSeconds
    , inputs.signal
    , Signal.map SpawnBarbarian everyThreeSeconds
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
  Model 0 0 1 0 0 startingLane


-- ACTIONS

type Action
    = NoOp
    | Increment
    | Tick Float
    | BuyWorker
    | BuyTower
    | SpawnBarbarian Float

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
    SpawnBarbarian delta ->
      if model.workers > 0 then
        {model | lane = addBarbarian model.lane}
      else
        model  
    NoOp ->
      model

handleTick : Time -> Model -> Model
handleTick delta model =
  model |>
  takeDamage delta |> 
  updateBank delta |>
  moveEnemies delta |>
  fireTowers delta

takeDamage : Time -> Model -> Model
takeDamage delta model =
  {model | workers = max 0.0 <| model.workers - (workerDamage delta model.lane),
           bank = max 0.0 <| model.bank - (clickDamage delta model.lane)}

updateBank : Time -> Model -> Model
updateBank delta model =
  let 
    clicks = clicksPerDelta delta model
  in
    {model | bank = model.bank + clicks}

moveEnemies : Time -> Model -> Model
moveEnemies delta model =
  {model | lane = moveLaneEnemies delta model.lane}

fireTowers : Time -> Model -> Model
fireTowers delta model =
  {model | lane = takeTowerDamage model.lane (Debug.watch "damage" (delta * model.towers) * 0.25)}

clickDamage : Time -> Lane -> Float
clickDamage delta lane =
  sum <| map .power <| reachingBase delta lane

workerDamage : Time -> Lane -> Float
workerDamage delta lane =
  sum <| map .workers <| reachingBase delta lane


workerValue : Model -> Float
workerValue model =
  model.workers

towerValue : Model -> Float
towerValue model =
  model.towers * 6

clicksPerDelta : Time -> Model -> Float
clicksPerDelta delta model =
  workerValue model * delta 


clicksPerSecond : Model -> Float
clicksPerSecond =
  clicksPerDelta 1

-- LANE

type alias Lane =
  { length: Float
  , enemies: List Enemy
  }

startingLane : Lane
startingLane =
  { length = 10.0
  , enemies = []
  }

takeTowerDamage : Lane -> Float -> Lane
takeTowerDamage lane amount =
  {lane| enemies =
    filter (.defense >> ((<) 0.0)) <| doDamage amount <| reverse lane.enemies
  }

moveLaneEnemies : Time -> Lane -> Lane
moveLaneEnemies delta lane =
  {lane | enemies = filter (reachedBase lane >> not) <| map (moveEnemy delta) lane.enemies}

reachingBase : Time -> Lane -> List Enemy
reachingBase delta lane =
  filter (wouldReachBase delta lane) lane.enemies

renderLane : Lane -> Html
renderLane lane =
  let 
    full_lane = rect 100 (lane.length * 50) |> filled black
  in
    fromElement <|
    collage 100 (floor <| lane.length * 50) <|
    full_lane :: (map (enemyToForm lane) lane.enemies)

addBarbarian : Lane -> Lane
addBarbarian lane =
  {lane| enemies = barbarian :: lane.enemies}


-- ENEMY

type alias Enemy =
  { name: String
  , power: Float
  , defense: Float
  , position: Float
  , speed: Float
  , workers: Float
  }

enemyToForm : Lane -> Enemy -> Form
enemyToForm lane enemy =
  rect 50 25 |> filled green |> (moveY <| enemyPosition lane enemy)

enemyPosition : Lane -> Enemy -> Float
enemyPosition lane enemy =
  (enemy.position * 50) - (lane.length * 25)

barbarian : Enemy
barbarian = 
  { name = "Barbarian"
  , power = 6.0
  , defense = 1.0
  , position = 0.0
  , speed = 0.5
  , workers = 1.0
  }

moveEnemy : Time -> Enemy -> Enemy
moveEnemy delta enemy =
  {enemy| position = (enemy.speed * delta) + enemy.position}

reachedBase : Lane -> Enemy -> Bool
reachedBase lane enemy =
  enemy.position >= lane.length

doDamage : Float -> List Enemy -> List Enemy
doDamage damage enemies =
  case enemies of
    [] -> []
    (enemy::rest) -> {enemy| defense = enemy.defense - damage} :: rest

wouldReachBase : Time -> Lane -> Enemy -> Bool
wouldReachBase delta lane enemy =
  (moveEnemy delta enemy).position >= lane.length

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
  (toString >> htext) value

htext : String -> Html
htext value =
  Html.text value

hbutton : List Attribute -> String -> Html
hbutton click value =
  button click [htext value]

view : Address Action -> Model -> Html
view address model =
  div [ bodyStyle ]
    [ hbutton [ onClick address Increment ] "click me"
    , div [] [ strText (floor model.bank) ]
    , div [] [ htext <| "+ " ++ (toString <| clicksPerSecond model) ++ " per second" ]
    , table []
      [ tr [ if model.bank < (workerCost model) then greyText else blackText ]
        [ td [] [ strText (workerCost model) ]
        , td [] [ htext "+1" ]
        , td
          []
          [ hbutton [onClick address BuyWorker] "worker" ]
        , td [] [ strText model.workers ]
        , td [] [ htext <| String.repeat (floor model.workers) "." ]
        ]
      , tr [ if model.bank <= (towerCost model) then greyText else blackText ]
        [ td [] [ strText (towerCost model) ]
        , td [] [  ]
        , td
          []
          [ hbutton [onClick address BuyTower] "tower" ]
        , td [] [ strText model.towers ]
        , td [] [ htext <| String.repeat (floor model.towers) "|" ]
        ]
      ]
    , renderLane model.lane
    ]