module Sound where

import StartApp
import Task exposing (Task)
import Signal exposing (Signal, Address)
import Effects exposing (Effects, Never)
import Html exposing (Html, Attribute, div, input, label, text)
import Html.Attributes exposing (id, for, value)
import Html.Events exposing (on, targetValue)
import String

--
-- StartApp boilerplate
--
app =
  StartApp.start { init = init, view = view, update = update, inputs = [] }

main : Signal Html
main =
  app.html

port tasks : Signal (Task Never ())
port tasks =
  app.tasks

--
-- My type declarations
--
type alias Model =
  { frequency: Int
  , soundSpeed: Float
  }

type Action
  = NoOp
  | FreqUpdate String

--
-- My functions
--
init : (Model, Effects Action)
init = ({ frequency = 1000, soundSpeed = 345.41 }, Effects.none)

freqToWavelength : Model -> Float
freqToWavelength model =
  model.soundSpeed / (toFloat model.frequency)

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp -> (model, Effects.none)
    FreqUpdate freqString ->
      let
          intResult = String.toInt(freqString)
      in
         case intResult of
           Ok value -> ({ model | frequency = value }, Effects.none)
           Err msg -> (model, Effects.none)

onInput : Signal.Address a -> (String -> a) -> Attribute
onInput address contentToValue =
  on "input" targetValue (\str -> Signal.message address (contentToValue str) )

view : Address Action -> Model -> Html
view address model =
  div []
    [ label [ (for "freq") ] [ text "Frequency" ]
    , input [ (id "freq"), (value (toString model.frequency)), onInput address FreqUpdate ] []
    , div [] [ text (toString (freqToWavelength model)) ]
    ]
