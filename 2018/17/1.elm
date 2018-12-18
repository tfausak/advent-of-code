module Main exposing (main)

import Browser
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
                    Time.every 1 (\_ -> Tick)

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
                                    , resting = Set.empty
                                    , flowing = Set.empty
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
                                ++ String.fromInt (Set.size state.resting + Set.size state.flowing)
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
                                    ++ List.map renderRestingWater (Set.toList state.resting)
                                    ++ List.map renderFlowingWater (Set.toList state.flowing)
                                )
                            ]

                      else
                        Html.p [] [ Html.text "Not rendering anything ..." ]
                    ]
        ]


renderClay : Point -> ( String, Svg.Svg Message )
renderClay point =
    unitSquareAt "grey" point


renderRestingWater : Point -> ( String, Svg.Svg Message )
renderRestingWater point =
    unitSquareAt "teal" point


renderFlowingWater : Point -> ( String, Svg.Svg Message )
renderFlowingWater point =
    unitSquareAt "aqua" point


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
    let
        flowing =
            flowWater state.maximumY state.clay state.resting state.flowing

        resting =
            restWater state.clay state.resting flowing
    in
    { state
        | iteration = state.iteration + 1
        , flowing = Set.insert ( 500, 1 ) (Set.diff flowing resting)
        , resting = resting
    }


restWater : Set.Set Point -> Set.Set Point -> Set.Set Point -> Set.Set Point
restWater clay resting flowing =
    flowing
        |> findRows
        |> List.filter (isSupported clay resting flowing)
        |> List.foldl
            (\( y, xs ) r -> List.foldl (\x -> Set.insert ( x, y )) r xs)
            resting


isSupported : Set.Set Point -> Set.Set Point -> Set.Set Point -> ( Int, List Int ) -> Bool
isSupported clay resting flowing ( y, xs ) =
    List.all
        (\x ->
            (let
                p =
                    ( x, y + 1 )
             in
             Set.member p clay || Set.member p resting
            )
                && (let
                        p =
                            ( x - 1, y )
                    in
                    Set.member p clay || Set.member p resting || Set.member p flowing
                   )
                && (let
                        p =
                            ( x + 1, y )
                    in
                    Set.member p clay || Set.member p resting || Set.member p flowing
                   )
        )
        xs


findRows : Set.Set Point -> List ( Int, List Int )
findRows flowing =
    flowing
        |> Set.toList
        |> List.sortBy yFirst
        |> List.foldl findOne []


yFirst ( x, y ) =
    ( y, x )


findOne ( x, y ) rows =
    case rows of
        ( y1, x1 :: xs ) :: rest ->
            if y == y1 && x == x1 + 1 then
                ( y, x :: x1 :: xs ) :: rest

            else
                ( y, [ x ] ) :: ( y1, x1 :: xs ) :: rows

        _ ->
            ( y, [ x ] ) :: rows


flowWater : Int -> Set.Set Point -> Set.Set Point -> Set.Set Point -> Set.Set Point
flowWater maximumY clay resting flowing =
    Set.foldl (flowOne maximumY clay resting) Set.empty flowing


flowOne maximumY clay resting ( x, y ) set =
    let
        newY =
            y + 1

        down =
            ( x, newY )
    in
    if isOccupied clay resting down then
        let
            left =
                ( x - 1, y )

            right =
                ( x + 1, y )
        in
        case ( isOccupied clay resting left, isOccupied clay resting right ) of
            ( True, True ) ->
                Set.insert ( x, y ) set

            ( True, False ) ->
                Set.insert right set

            ( False, True ) ->
                Set.insert left set

            ( False, False ) ->
                Set.insert right (Set.insert left set)

    else if newY > maximumY then
        set

    else
        Set.insert down set


isOccupied clay resting point =
    Set.member point clay || Set.member point resting


type alias State =
    { clay : Set.Set Point
    , minimumX : Int
    , maximumX : Int
    , maximumY : Int
    , resting : Set.Set Point
    , flowing : Set.Set Point
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
                , resting = Set.empty
                , flowing = Set.empty
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
