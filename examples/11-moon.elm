import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (..)
import Task
import Time exposing (Time)



main = 
  Html.program
    { init = init 
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model = 
  { phase : String 
  , illumination : Float
  , age : Float
  , fmdt : String
  , fmut : Float
  , nmdt : String
  , nmut : Float
  , imagesrc : String
  , timestamp : String 
  }


init : (Model, Cmd Msg)
init = 
  ( Model "waiting.." 0.0 0.0 "full moon date and time" 0.0 "new moon date and time" 0.0 "moon-phases/loading.gif" "unknown"
   , Task.perform NewTime Time.now -- get current state
  )



-- UPDATE


type Msg
  = GetPhase (Result Http.Error String)
  | Click 
  | NewTime Time


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of

    GetPhase (Ok stringResult) -> 
      ( { model | 
          phase = getPhase stringResult , 
          illumination = getIllumination stringResult ,
          age = getAge stringResult ,
          fmdt = getFMDT stringResult ,
          fmut = getFMUT stringResult ,
          nmdt = getNMDT stringResult ,
          nmut = getNMUT stringResult ,
          imagesrc = generateImageName (getPhase stringResult) (getIllumination stringResult)
        } , Cmd.none )

    GetPhase (Err _) -> 
      ( { model | phase = "error" }, Cmd.none)

    Click -> 
      ( model, Task.perform NewTime Time.now )

    NewTime time -> 
      ( { model | timestamp = createTimestamp time }, getMoonPhase ( createTimestamp time ) )


-- VIEW


view : Model -> Html Msg
view model = 
  div [ Html.Attributes.style [("padding-left","30px"), ("padding-top","20px"), ("padding-bottom","20px")] ]
    [ p [] [text ("Phase: " ++ model.phase) ]
    , p [] [text ("Age: " ++ toString model.age) ]
    , p [] [text ("Illumination: " ++ toString model.illumination) ]
    , p [] [text ("Full moon time: " ++  model.fmdt) ]
    , p [] [text ("Full moon timestamp: " ++ String.slice 0 10 (toString model.fmut)) ]
    , p [] [text ("New moon time: " ++  model.nmdt) ]
    , p [] [text ("New moon timestamp: " ++ String.slice 0 10 (toString model.nmut)) ]
    , p [] [text ("Current time: " ++ model.timestamp) ]
    , img [ width 300, height 300, src (model.imagesrc) ] []
    , p [] [text ("Moon images by MarkieAnn Packer")]
    , button [ onClick Click ] [ text "Update phase" ]
    ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model = 
  Sub.none


-- Helper functions

-- decoding phase from the string json
getPhase : String -> String
getPhase json =
  case Decode.decodeString (Decode.at ["stage"] Decode.string) json of
    Ok string -> 
      string
    Err error -> 
      error

-- decoding age from the string json
getAge : String -> Float
getAge json =
  case Decode.decodeString (Decode.at ["age"] Decode.float) json of
    Ok float -> 
      float
    Err error -> 
      0.0

-- decoding illumination from the string json
getIllumination : String -> Float
getIllumination json =
  case Decode.decodeString (Decode.at ["illumination"] Decode.float) json of
    Ok float ->
      float 
    Err error ->
      0.0 

-- decoding full moon date and time from the string json
getFMDT : String -> String
getFMDT json = 
  case Decode.decodeString (Decode.at ["FM","DT"] Decode.string) json of
    Ok string -> 
      string 
    Err error -> 
      error

-- decoding full moon date and time from the string json
getFMUT : String -> Float
getFMUT json = 
  case Decode.decodeString (Decode.at ["FM","UT"] Decode.float) json of
    Ok float -> 
      float 
    Err error -> 
      0.0

-- decoding full moon date and time from the string json
getNMDT : String -> String
getNMDT json = 
  case Decode.decodeString (Decode.at ["NNM","DT"] Decode.string) json of
    Ok string -> 
      string 
    Err error -> 
      error

-- decoding full moon date and time from the string json
getNMUT : String -> Float
getNMUT json = 
  case Decode.decodeString (Decode.at ["NNM","UT"] Decode.float) json of
    Ok float -> 
      float 
    Err error -> 
      0.0

-- string trim function (to create timestamps)
createTimestamp : Time -> String
createTimestamp time = 
  String.slice 0 10 (toString time)

-- function that creates request command
getMoonPhase : String -> Cmd Msg
getMoonPhase timestamp = 
  let 
    url = 
      "http://api.burningsoul.in/moon/" ++ timestamp

    request = 
      requestForString url
  in
    Http.send GetPhase request

-- generate image name based on current phase and illumination
generateImageName : String -> Float -> String
generateImageName phase illumination = 
  if ( (phase == "waxing" || phase == "waning") && (illumination < 1) ) then "moon-phases/new-moon.svg"
  else if ( (phase == "waxing") && (illumination >= 1 && illumination < 40) ) then "moon-phases/waxing-cresent.svg"
  else if ( (phase == "waxing") && (illumination >= 40 && illumination < 60) ) then "moon-phases/first-quarter.svg"
  else if ( (phase == "waxing") && (illumination >= 60 && illumination < 90) ) then "moon-phases/waxing-gibbous.svg"
  else if ( (phase == "waxing" || phase == "waning") && illumination >= 99) then "moon-phases/full-moon.svg"
  else if ( (phase == "waning") && ( illumination < 99 && illumination >= 60) ) then "moon-phases/waning-gibbous.svg"
  else if ( (phase == "waning") && ( illumination < 60 && illumination >= 40) ) then "moon-phases/last-quarter.svg"
  else if ( (phase == "waning") && ( illumination < 40 && illumination >= 1) ) then "moon-phases/waning-cresent.svg"
  else "couldn't figure out the phase"

-- request for string
requestForString : String -> Http.Request String
requestForString url = 
  Http.getString url

-- request for list of tuples
createRequest : String -> Http.Request ( List ( String, Int )) 
createRequest url = 
  Http.get url getKeyValue 

-- Decoder
getKeyValue : Decode.Decoder ( List ( String, Int ))
getKeyValue = 
  -- decodeString (keyValuePairs int) "{ \"alice\": 42, \"bob\": 99 }" == [("alice", 42), ("bob", 99)]
  Decode.keyValuePairs Decode.int

-- needs to be a Decode.Decoder a
decodeMoonUrl : Decode.Decoder String
decodeMoonUrl = 
  -- decodeString (at ["person", "name"] string) json  == Ok "tom"
  Decode.at ["stage"] Decode.string

-- get particular field from list of tuples

getFieldsList : (List ( String, Int )) -> List Int
getFieldsList list = 
  Tuple.second (List.unzip list)

getNthFromList : List Int -> Int -> String
getNthFromList list n = 
  case (List.head (List.reverse(List.take n list))) of 
    Nothing ->
      "error at getNthFromList"
    Just int ->
      toString int
