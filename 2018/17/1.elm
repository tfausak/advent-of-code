module Main exposing (main)

import Browser
import Dict
import Html
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Parser exposing ((|.), (|=))
import Set
import Svg
import Svg.Attributes as SvgA
import Svg.Keyed as SvgK
import Time


main : Program Flags Model Message
main =
    Browser.element
        { init = initialModel
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


subscriptions : Model -> Sub Message
subscriptions model =
    case model.output of
        Err _ ->
            Sub.none

        Ok state ->
            case state.playback of
                Playing ->
                    Time.every 10 (\_ -> Tick)

                Paused ->
                    Sub.none


type alias Flags =
    ()


type alias Model =
    { input : String
    , output : Result String State
    }


type Message
    = Tick
    | UpdateInput String
    | Play
    | Pause
    | Step
    | Reset
    | ToggleRender


initialModel : Flags -> ( Model, Cmd Message )
initialModel _ =
    ( { input = initialInput
      , output = parseInput initialInput
      }
    , Cmd.none
    )


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        ToggleRender ->
            case model.output of
                Err _ ->
                    ( model, Cmd.none )

                Ok state ->
                    ( { model
                        | output = Ok { state | render = not state.render }
                      }
                    , Cmd.none
                    )

        Tick ->
            case model.output of
                Err _ ->
                    ( model, Cmd.none )

                Ok state ->
                    if state.playback == Playing then
                        let
                            newState =
                                step state
                        in
                        if newState.water == state.water then
                            ( { model | output = Ok { state | playback = Paused } }
                            , Cmd.none
                            )

                        else
                            ( { model | output = Ok (step state) }, Cmd.none )

                    else
                        ( model, Cmd.none )

        UpdateInput input ->
            ( { model
                | input = input
                , output = parseInput input
              }
            , Cmd.none
            )

        Play ->
            case model.output of
                Err _ ->
                    ( model, Cmd.none )

                Ok state ->
                    ( { model | output = Ok { state | playback = Playing } }
                    , Cmd.none
                    )

        Pause ->
            case model.output of
                Err _ ->
                    ( model, Cmd.none )

                Ok state ->
                    ( { model | output = Ok { state | playback = Paused } }
                    , Cmd.none
                    )

        Step ->
            case model.output of
                Err _ ->
                    ( model, Cmd.none )

                Ok state ->
                    ( { model | output = Ok (step state) }, Cmd.none )

        Reset ->
            case model.output of
                Err _ ->
                    ( model, Cmd.none )

                Ok state ->
                    ( { model
                        | output =
                            Ok
                                { state
                                    | iteration = 0
                                    , water = Dict.empty
                                    , playback = Paused
                                }
                      }
                    , Cmd.none
                    )


view : Model -> Html.Html Message
view model =
    Html.div []
        [ Html.h1 []
            [ Html.a
                [ HtmlA.href "https://adventofcode.com/2018/day/17" ]
                [ Html.text "Reservoir Research" ]
            ]
        , Html.div []
            [ Html.textarea
                [ HtmlE.onInput UpdateInput ]
                [ Html.text model.input ]
            ]
        , case model.output of
            Err message ->
                Html.p [] [ Html.text message ]

            Ok state ->
                Html.div []
                    [ Html.div []
                        [ case state.playback of
                            Paused ->
                                Html.button [ HtmlE.onClick Play ] [ Html.text "Play" ]

                            Playing ->
                                Html.button [ HtmlE.onClick Pause ] [ Html.text "Pause" ]
                        , Html.button [ HtmlE.onClick Step ] [ Html.text "Step" ]
                        , Html.button [ HtmlE.onClick Reset ] [ Html.text "Reset" ]
                        , Html.button [ HtmlE.onClick ToggleRender ] [ Html.text "Toggle redering" ]
                        ]
                    , Html.p []
                        [ Html.text <|
                            "Iteration: "
                                ++ String.fromInt state.iteration
                                ++ ", "
                                ++ "Water: "
                                ++ String.fromInt (Dict.size state.water)
                        ]
                    , if state.render then
                        Svg.svg
                            [ SvgA.width (toWidth state)
                            , SvgA.height (toHeight state)
                            , SvgA.viewBox (toViewBox state)
                            , SvgA.style "width: 60%; height: auto; background: wheat"
                            ]
                            [ SvgK.node "g"
                                []
                                (unitSquareAt "blue" ( center, 0 )
                                    :: List.map renderClay (Set.toList state.clay)
                                    ++ List.map renderWater (Dict.toList state.water)
                                )
                            ]

                      else
                        Html.p [] [ Html.text "Not rendering anything ..." ]
                    ]
        ]


renderClay : Point -> ( String, Svg.Svg Message )
renderClay point =
    unitSquareAt "grey" point


renderWater : ( Point, Water ) -> ( String, Svg.Svg Message )
renderWater ( point, water ) =
    unitSquareAt
        (case water of
            Flowing ->
                "aqua"

            Resting ->
                "teal"
        )
        point


center : Int
center =
    500


unitSquareAt : String -> Point -> ( String, Svg.Svg Message )
unitSquareAt color ( x, y ) =
    ( "s-" ++ String.fromInt x ++ "-" ++ String.fromInt y
    , Svg.rect
        [ SvgA.width "1"
        , SvgA.height "1"
        , SvgA.x (String.fromInt x)
        , SvgA.y (String.fromInt y)
        , SvgA.fill color
        ]
        []
    )


toViewBox : State -> String
toViewBox state =
    String.fromInt (state.minimumX - 1)
        ++ " 0 "
        ++ toWidth state
        ++ " "
        ++ toHeight state


toWidth : State -> String
toWidth state =
    String.fromInt (state.maximumX - state.minimumX + 3)


toHeight : State -> String
toHeight state =
    String.fromInt (state.maximumY + 1)


initialInput : String
initialInput =
    String.trim """
x=495, y=2..7
y=7, x=495..501
x=501, y=3..7
x=498, y=2..4
x=506, y=1..2
x=498, y=10..13
x=504, y=10..13
y=13, x=498..504
"""


type Water
    = Flowing
    | Resting


step : State -> State
step state =
    { state
        | iteration = state.iteration + 1
        , water =
            state.water
                |> flowWater state.clay
                |> restWater state.clay
                |> trimWater state.maximumY
                |> Dict.insert ( 500, 1 ) Flowing
    }


trimWater : Int -> Dict.Dict Point Water -> Dict.Dict Point Water
trimWater maximumY =
    Dict.filter (\( _, y ) _ -> y <= maximumY)


restWater : Set.Set Point -> Dict.Dict Point Water -> Dict.Dict Point Water
restWater clay waters =
    waters
        |> findRows clay
        |> List.filter (isSupported clay waters)
        |> List.foldl
            (\( y, xs ) dict ->
                List.foldl
                    (\x -> Dict.insert ( x, y ) Resting)
                    dict
                    xs
            )
            waters


isSupported : Set.Set Point -> Dict.Dict Point Water -> ( Int, List Int ) -> Bool
isSupported clay waters ( y, xs ) =
    List.all
        (\x ->
            let
                down =
                    ( x, y + 1 )

                fromBelow =
                    Set.member down clay || Dict.get down waters == Just Resting

                left =
                    ( x - 1, y )

                onLeft =
                    Set.member left clay || Dict.member left waters

                right =
                    ( x + 1, y )

                onRight =
                    Set.member right clay || Dict.member right waters
            in
            fromBelow && onLeft && onRight
        )
        xs


findRows : Set.Set Point -> Dict.Dict Point Water -> List ( Int, List Int )
findRows clay waters =
    waters
        |> Dict.filter (\_ water -> water == Flowing)
        |> Dict.keys
        |> List.sortBy (\( x, y ) -> ( y, x ))
        |> List.foldl
            (\( x, y ) rows ->
                case rows of
                    ( y1, x1 :: xs ) :: rest ->
                        if y == y1 && x == x1 + 1 then
                            ( y, x :: x1 :: xs ) :: rest

                        else
                            ( y, [ x ] ) :: ( y1, x1 :: xs ) :: rows

                    _ ->
                        ( y, [ x ] ) :: rows
            )
            []


flowWater : Set.Set Point -> Dict.Dict Point Water -> Dict.Dict Point Water
flowWater clay waters =
    List.foldl
        (\( ( x, y ), water ) dict ->
            case water of
                Resting ->
                    Dict.insert ( x, y ) Resting dict

                Flowing ->
                    let
                        down =
                            ( x, y + 1 )

                        left =
                            ( x - 1, y )

                        right =
                            ( x + 1, y )

                        occupied point =
                            Set.member point clay || Dict.get point waters == Just Resting
                    in
                    if occupied down then
                        case ( occupied left, occupied right ) of
                            ( True, True ) ->
                                Dict.insert ( x, y ) Resting dict

                            ( True, False ) ->
                                Dict.insert right Flowing dict

                            ( False, True ) ->
                                Dict.insert left Flowing dict

                            ( False, False ) ->
                                Dict.insert left Flowing (Dict.insert right Flowing dict)

                    else
                        Dict.insert down Flowing dict
        )
        Dict.empty
        (Dict.toList waters)


type alias State =
    { clay : Set.Set Point
    , minimumX : Int
    , maximumX : Int
    , maximumY : Int
    , water : Dict.Dict Point Water
    , iteration : Int
    , playback : Playback
    , render : Bool
    }


type Playback
    = Paused
    | Playing


type alias Point =
    ( Int, Int )


scanToState : Scan -> Result String State
scanToState scan =
    let
        clay =
            Set.fromList (List.concatMap veinToPoints scan)

        xs =
            Set.toList (Set.map Tuple.first clay)

        ys =
            Set.toList (Set.map Tuple.second clay)
    in
    case ( List.minimum xs, List.maximum xs, List.maximum ys ) of
        ( Just minimumX, Just maximumX, Just maximumY ) ->
            Ok
                { clay = clay
                , minimumX = minimumX
                , maximumX = maximumX
                , maximumY = maximumY
                , water = Dict.empty
                , iteration = 0
                , playback = Paused
                , render = True
                }

        _ ->
            Err "empty scan"


veinToPoints : Vein -> List Point
veinToPoints vein =
    List.concatMap (\x -> List.map (Tuple.pair x) vein.ys) vein.xs


parseInput : String -> Result String State
parseInput input =
    Parser.run scanParser input
        |> Result.mapError Parser.deadEndsToString
        |> Result.andThen scanToState


scanParser : Parser.Parser Scan
scanParser =
    Parser.loop [] scanParserHelper


scanParserHelper : Scan -> Parser.Parser (Parser.Step Scan Scan)
scanParserHelper veins =
    Parser.oneOf
        [ veinParser
            |> Parser.andThen lineToVein
            |> Parser.map (\vein -> Parser.Loop (vein :: veins))
        , Parser.succeed (scanParserDone veins)
        ]


type alias Vein =
    { xs : List Int
    , ys : List Int
    }


lineToVein : Line -> Parser.Parser Vein
lineToVein line =
    if line.secondaryStart > line.secondaryEnd then
        Parser.problem "starts after end"

    else
        case ( line.primaryAxis, line.secondaryAxis ) of
            ( X, Y ) ->
                Parser.succeed
                    { xs = [ line.primaryValue ]
                    , ys = List.range line.secondaryStart line.secondaryEnd
                    }

            ( Y, X ) ->
                Parser.succeed
                    { xs = List.range line.secondaryStart line.secondaryEnd
                    , ys = [ line.primaryValue ]
                    }

            _ ->
                Parser.problem "same axis"


veinParser : Parser.Parser Line
veinParser =
    Parser.succeed Line
        |= axisParser
        |. Parser.token "="
        |= intParser
        |. Parser.token ", "
        |= axisParser
        |. Parser.token "="
        |= intParser
        |. Parser.token ".."
        |= intParser
        |. Parser.oneOf [ Parser.token "\n", Parser.end ]


scanParserDone : Scan -> Parser.Step a Scan
scanParserDone veins =
    Parser.Done (List.reverse veins)


intParser : Parser.Parser Int
intParser =
    Parser.getChompedString (Parser.chompWhile Char.isDigit)
        |> Parser.andThen stringToInt


stringToInt : String -> Parser.Parser Int
stringToInt string =
    string
        |> String.toInt
        |> Maybe.map Parser.succeed
        |> Maybe.withDefault (Parser.problem "expecting int")


type Axis
    = X
    | Y


axisParser : Parser.Parser Axis
axisParser =
    Parser.oneOf
        [ Parser.map (always X) (Parser.token "x")
        , Parser.map (always Y) (Parser.token "y")
        ]


type alias Scan =
    List Vein


type alias Line =
    { primaryAxis : Axis
    , primaryValue : Int
    , secondaryAxis : Axis
    , secondaryStart : Int
    , secondaryEnd : Int
    }
