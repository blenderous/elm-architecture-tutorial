import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import AnimationFrame exposing (..)
import Time exposing (Time, second)
import Task
import Random

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL


type alias Model = 
  { 
    dieFace : Int,
    circleRadius : Int
  }


init : (Model, Cmd Msg)
init = 
  ({dieFace = 1, circleRadius = 0}, Cmd.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  AnimationFrame.times CanAnimate


-- UPDATE


type Msg
  = Roll
  | NewFace Int
  | CanAnimate Time


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    Roll ->
      (model, Random.generate NewFace (Random.int 1 6))

    NewFace newFace ->
      ({model | dieFace = newFace}, Cmd.none)

    CanAnimate time ->
      ({model | circleRadius = (model.circleRadius + incrementBy (model.dieFace, model.circleRadius))}, Cmd.none)
      

-- VIEW


view : Model -> Html Msg
view model = 
  div [ Html.Attributes.class "container"]
    [ h1 [] [ Html.text "Animation" ]
    , div [ Html.Attributes.class "leftcol" ] 
      [ 
        diceSvg model 
        , p [] 
          [ 
            button [ 
              onClick Roll
            ] [ Html.text "Roll" ] 
          ]
      ]
    , div [ Html.Attributes.class "rightcol" ] [ animateSvg model ]
    ]

diceSvg : Model -> Svg Msg
diceSvg model = 
  let (c1Radius, c2Radius, c3Radius, c4Radius, c5Radius, c6Radius, c7Radius) = 
    case model.dieFace of 
      1 -> 
        (0, 0, 0, 30, 0, 0, 0)
      2 -> 
        (0, 30, 0, 0, 0, 30, 0)
      3 -> 
        (30, 0, 0, 30, 0, 0, 30)
      4 -> 
        (30, 0, 30, 0, 30, 0, 30)
      5 -> 
        (30, 0, 30, 30, 30, 0, 30)
      6 -> 
        (30, 30, 30, 0, 30, 30, 30)
      _ -> 
        (0, 0, 0, 0, 0, 0, 0)
  in 
    svg [ Svg.Attributes.width "400", Svg.Attributes.height "400" ]
      [ 
        rect [ Svg.Attributes.style "opacity:1; fill:#e13850; fill-opacity:1; fill-rule:nonzero; stroke:none; stroke-width:4; stroke-linecap:butt; stroke-linejoin:round; stroke-miterlimit:4; stroke-dasharray:none; stroke-opacity:1"
               , Svg.Attributes.width "360"
               , Svg.Attributes.height "360"
               , Svg.Attributes.x "20"
               , Svg.Attributes.y "20"
               , Svg.Attributes.ry "130" ] [] 
      , Svg.circle [ cy "130", cx "130", r (toString c1Radius), Svg.Attributes.fill "#ffffff" ] [] 
      , Svg.circle [ cy "200", cx "130", r (toString c2Radius), Svg.Attributes.fill "#ffffff" ] [] 
      , Svg.circle [ cy "270", cx "130", r (toString c3Radius), Svg.Attributes.fill "#ffffff" ] [] 
      , Svg.circle [ cy "200", cx "200", r (toString c4Radius), Svg.Attributes.fill "#ffffff" ] [] 
      , Svg.circle [ cy "130", cx "270", r (toString c5Radius), Svg.Attributes.fill "#ffffff" ] [] 
      , Svg.circle [ cy "200", cx "270", r (toString c6Radius), Svg.Attributes.fill "#ffffff" ] [] 
      , Svg.circle [ cy "270", cx "270", r (toString c7Radius), Svg.Attributes.fill "#ffffff" ] [] 
      ]

animateSvg : Model -> Svg Msg
animateSvg model = 
  svg [ Svg.Attributes.width "500", Svg.Attributes.height "500" ]
    [
      Svg.circle [ cy "130", cx "130", r (toString model.circleRadius), Svg.Attributes.fill "#63e0be" ] [] 
    ]


-- HELPERS

incrementBy : (Int, Int) -> Int
incrementBy (dieface, radius) = 
  if ((dieface * 20) < radius) then -1
  else if ((dieface * 20) > radius) then 1
  else 0