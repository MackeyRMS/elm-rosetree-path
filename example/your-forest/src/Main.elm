module Main exposing (main)

import Angle
import Browser
import Browser.Dom
import Collage exposing (Collage)
import Collage.Events
import Collage.Layout
import Collage.Render
import Collage.Text
import Color exposing (Color, rgb, rgba)
import Direction2d
import Forest.Navigate exposing (Forest)
import Forest.Path exposing (ForestPath)
import Html
import Html.Attributes
import Html.Events
import Json.Decode
import List.Extra as List
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Random
import Task
import Tree exposing (Tree, tree)
import Tree.Navigate
import Tree.Path exposing (TreePath)
import Vector2d


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = viewDocument
        }


type alias Model =
    { trees : Forest NodeInfo
    , selectedPath : Maybe ForestPath
    , windowSize : { width : Float, height : Float }
    , dragged : Maybe ForestPath
    , mousePosition : Point2d Pixels Float
    }


type alias NodeInfo =
    { translate : Point2d Pixels Float
    , factor : Float
    }


init : ( Model, Cmd Msg )
init =
    ( { trees = []
      , selectedPath = Nothing
      , windowSize = { width = 0, height = 0 }
      , dragged = Nothing
      , mousePosition = Point2d.origin
      }
    , [ Browser.Dom.getViewport
            |> Task.perform
                (\v -> { width = v.viewport.width, height = v.viewport.height } |> WindowSized)
      , Random.generate InitialTreeGenerated
            (randomBranches { depth = 0 })
      ]
        |> Cmd.batch
    )


type Msg
    = BranchSelected ForestPath
    | DoubleClicked
    | WindowSized { width : Float, height : Float }
    | InitialTreeGenerated (Forest NodeInfo)
    | MouseMoved (Point2d Pixels Float)
    | PressedOn ForestPath (Point2d Pixels Float)
    | MouseLifted
    | RightClickedOn ForestPath
    | ContextMenuActivated


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BranchSelected indexAndPathToSelect ->
            ( { model
                | selectedPath =
                    Just indexAndPathToSelect
              }
            , Cmd.none
            )

        DoubleClicked ->
            ( let
                selectedTree : Maybe (Tree NodeInfo)
                selectedTree =
                    model.selectedPath
                        |> Maybe.andThen
                            (\path ->
                                model.trees |> Forest.Navigate.to path
                            )
              in
              { model
                | trees =
                    case selectedTree of
                        Nothing ->
                            model.trees

                        Just selected ->
                            model.trees
                                ++ [ selected
                                        |> Tree.mapLabel
                                            (updateTranslate (\_ -> model.mousePosition))
                                   ]
              }
            , Cmd.none
            )

        WindowSized windowSize ->
            ( { model
                | windowSize =
                    { width = windowSize.width - 3.5, height = windowSize.height - 3.5 }
              }
            , Cmd.none
            )

        InitialTreeGenerated branches ->
            ( { model
                | trees =
                    [ tree { translate = Point2d.origin, factor = 1 }
                        branches
                    ]
              }
            , Cmd.none
            )

        MouseMoved positionOnScreen ->
            let
                mousePosition : Point2d Pixels Float
                mousePosition =
                    Point2d.fromRecord Pixels.float
                        { x =
                            (positionOnScreen |> Point2d.xCoordinate |> Pixels.toFloat)
                                - (model.windowSize.width / 2)
                        , y =
                            (model.windowSize.height / 2)
                                - (positionOnScreen |> Point2d.yCoordinate |> Pixels.toFloat)
                        }
            in
            ( case model.dragged of
                Just path ->
                    { model
                        | selectedPath = Just path
                        , mousePosition = mousePosition
                        , trees =
                            let
                                mouseMovement =
                                    Vector2d.from model.mousePosition mousePosition
                            in
                            model.trees
                                |> Forest.Navigate.alter path
                                    (Tree.mapLabel
                                        (updateTranslate
                                            (\translate ->
                                                translate |> Point2d.translateBy mouseMovement
                                            )
                                        )
                                    )
                    }

                Nothing ->
                    { model | mousePosition = mousePosition }
            , Cmd.none
            )

        PressedOn indexAndPath position ->
            ( { model
                | dragged = Just indexAndPath
                , selectedPath =
                    if model.selectedPath == Just indexAndPath then
                        Nothing

                    else
                        Just indexAndPath
              }
            , Cmd.none
            )

        MouseLifted ->
            ( { model | dragged = Nothing }
            , Cmd.none
            )

        RightClickedOn path ->
            ( { model
                | selectedPath =
                    if model.selectedPath == Just path then
                        Nothing

                    else
                        model.selectedPath
                , trees =
                    model.trees
                        |> Forest.Navigate.remove path
              }
            , Cmd.none
            )

        ContextMenuActivated ->
            ( model, Cmd.none )


randomBranches :
    { depth : Int }
    -> Random.Generator (List (Tree NodeInfo))
randomBranches { depth } =
    if depth >= 6 then
        Random.constant []

    else
        Random.int 1 4
            |> Random.andThen
                (\branchCount ->
                    Random.list branchCount
                        (Random.lazy
                            (\() ->
                                randomBranches { depth = depth + 1 }
                            )
                        )
                        |> Random.map
                            (List.indexedMap
                                (\i branches ->
                                    tree
                                        { translate =
                                            let
                                                directionInRadians =
                                                    Angle.turns
                                                        ((1 / 4)
                                                            * ((toFloat i * 2 + 1)
                                                                / toFloat branchCount
                                                              )
                                                        )
                                            in
                                            Point2d.origin
                                                |> Point2d.translateBy
                                                    (directionInRadians
                                                        |> Direction2d.fromAngle
                                                        |> Vector2d.withLength
                                                            (0.1 * toFloat (10 - depth) ^ 3 |> Pixels.float)
                                                    )
                                        , factor = 0.9 * toFloat (depth + 1) ^ -1.4
                                        }
                                        branches
                                )
                            )
                )


viewDocument : Model -> Browser.Document Msg
viewDocument model =
    { title = "tree from branches"
    , body =
        [ [ viewTrees model
          , [ "right click to delete"
            , "double click to clone 🟦"
            ]
                |> List.map
                    (\line ->
                        line
                            |> Collage.Text.fromString
                            |> Collage.Text.color (rgb 1 1 1)
                            |> Collage.Text.size 26
                            |> Collage.rendered
                            |> Collage.Layout.align Collage.Layout.base
                    )
                |> List.intersperse (Collage.Layout.spacer 0 10)
                |> Collage.Layout.vertical
                |> Collage.Layout.align Collage.Layout.base
                |> Collage.shiftY -200
          , Collage.rectangle model.windowSize.width model.windowSize.height
                |> Collage.filled
                    (rgb 0 0 0 |> Collage.uniform)
          ]
            |> Collage.group
            |> Collage.Events.onMouseUp (\_ -> MouseLifted)
            |> Collage.Events.onMouseMove (Point2d.fromTuple Pixels.float >> MouseMoved)
            |> Collage.Events.onDoubleClick DoubleClicked
            |> Collage.Render.svgBox ( model.windowSize.width, model.windowSize.height )
            |> (\collage ->
                    Html.div
                        [ Html.Attributes.style
                            "background-color"
                            "black"
                        , Html.Events.preventDefaultOn "contextmenu"
                            (Json.Decode.succeed ( ContextMenuActivated, True ))
                        ]
                        [ collage ]
               )
        ]
    }


viewTrees : Model -> Collage Msg
viewTrees { trees, selectedPath } =
    let
        viewTree :
            { path : ForestPath, label : NodeInfo, children : List { ui : Collage Msg, translate : Point2d Pixels Float } }
            -> { ui : Collage Msg, translate : Point2d Pixels Float }
        viewTree sub =
            let
                viewLabel =
                    Collage.square (50 * sub.label.factor)
                        |> Collage.filled
                            ((if selectedPath == Just sub.path then
                                rgb 0 0.5 0.9

                              else
                                rgb 0 0.7 0.4
                             )
                                |> Collage.uniform
                            )
                        |> Collage.Events.onMouseDown (Point2d.fromTuple Pixels.float >> PressedOn sub.path)
                        |> Collage.Events.on "contextmenu"
                            (Json.Decode.succeed (RightClickedOn sub.path))

                viewConnection childTranslate =
                    Collage.path
                        [ ( 0, 0 )
                        , childTranslate |> Point2d.toTuple Pixels.toFloat
                        ]
                        |> Collage.traced
                            (Collage.solid (4 * sub.label.factor)
                                (rgb 0.6 0.4 0 |> Collage.uniform)
                            )
            in
            { translate = sub.label.translate
            , ui =
                viewLabel
                    :: (sub.children |> List.map .ui)
                    ++ (sub.children |> List.map (.translate >> viewConnection))
                    |> Collage.group
                    |> Collage.shift (sub.label.translate |> Point2d.toTuple Pixels.toFloat)
            }
    in
    trees
        |> List.indexedMap
            (\index tree ->
                tree
                    |> Tree.Navigate.restructure
                        (\sub ->
                            viewTree
                                { label = sub.label
                                , children = sub.children
                                , path = Forest.Path.fromIndex index sub.path
                                }
                        )
                    |> .ui
            )
        |> Collage.group



--


type alias Update inner outer =
    (inner -> inner) -> outer -> outer


updateTranslate : Update t { r_ | translate : t }
updateTranslate changeTranslate =
    \t ->
        { t
            | translate =
                t.translate
                    |> changeTranslate
        }
