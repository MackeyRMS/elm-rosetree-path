module Main exposing (main)

import Browser
import Browser.Dom
import Collage exposing (Collage)
import Collage.Events
import Collage.Layout
import Collage.Render
import Collage.Text
import Color exposing (Color, rgb, rgba)
import Html
import Html.Attributes
import Html.Events
import Json.Decode
import List.Extra as List
import Random
import Task
import Tree exposing (Tree, tree)
import Tree.Extra.Lue as Tree exposing (leaf)
import TreePath exposing (TreePath)
import Xy exposing (Xy)


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = viewDocument
        }


type alias Model =
    { trees : List (Tree NodeInfo)
    , selectedPath : Maybe { treeIndex : Int, path : TreePath }
    , windowSize : Xy Float
    , dragged : Maybe { treeIndex : Int, path : TreePath }
    , mousePosition : Xy Float
    }


type alias NodeInfo =
    { translate : Xy Float
    , factor : Float
    }


init : ( Model, Cmd Msg )
init =
    ( { trees = []
      , selectedPath = Nothing
      , windowSize = Xy.zero
      , dragged = Nothing
      , mousePosition = ( 0, 0 )
      }
    , [ Browser.Dom.getViewport
            |> Task.perform
                (.viewport >> Xy.fromSize >> WindowSized)
      , Random.generate InitialTreeGenerated
            (randomBranches { depth = 0 })
      ]
        |> Cmd.batch
    )


type Msg
    = BranchSelected { treeIndex : Int, path : TreePath }
    | DoubleClicked
    | WindowSized (Xy Float)
    | InitialTreeGenerated (List (Tree NodeInfo))
    | MouseMoved (Xy Float)
    | PressedOn { treeIndex : Int, path : TreePath } (Xy Float)
    | MouseLifted
    | RightClickedOn { treeIndex : Int, path : TreePath }
    | NoOp


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
                selectedTree =
                    model.selectedPath
                        |> Maybe.andThen
                            (\{ treeIndex, path } ->
                                model.trees
                                    |> List.getAt treeIndex
                                    |> Maybe.andThen (Tree.at path)
                            )
              in
              { model
                | trees =
                    case selectedTree of
                        Just selected ->
                            model.trees
                                ++ [ selected
                                        |> Tree.mapLabel
                                            (updateTranslate
                                                (\_ -> model.mousePosition)
                                            )
                                   ]

                        Nothing ->
                            model.trees
              }
            , Cmd.none
            )

        WindowSized windowSize ->
            ( { model
                | windowSize =
                    windowSize
                        |> Xy.map (\c -> c - 3.5)
              }
            , Cmd.none
            )

        InitialTreeGenerated branches ->
            ( { model
                | trees =
                    [ tree { translate = ( 0, 0 ), factor = 1 }
                        branches
                    ]
              }
            , Cmd.none
            )

        MouseMoved positionOnScreen ->
            let
                mousePosition =
                    ( Xy.x positionOnScreen
                        - (Xy.x model.windowSize / 2)
                    , (Xy.y model.windowSize / 2)
                        - Xy.y positionOnScreen
                    )
            in
            ( case model.dragged of
                Just indexAndPath ->
                    { model
                        | dragged = Just indexAndPath
                        , selectedPath = Just indexAndPath
                        , mousePosition = mousePosition
                        , trees =
                            let
                                { treeIndex, path } =
                                    indexAndPath

                                mouseMovement =
                                    Xy.map2 (\now previous -> now - previous)
                                        mousePosition
                                        model.mousePosition
                            in
                            model.trees
                                |> List.updateAt treeIndex
                                    (Tree.updateAt path
                                        (Tree.mapLabel
                                            (updateTranslate
                                                (Xy.map2 (+) mouseMovement)
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

        RightClickedOn indexAndPath ->
            ( { model
                | selectedPath =
                    if model.selectedPath == Just indexAndPath then
                        Nothing

                    else
                        model.selectedPath
                , trees =
                    let
                        { treeIndex, path } =
                            indexAndPath
                    in
                    case path of
                        [] ->
                            model.trees |> List.removeAt treeIndex

                        _ ->
                            model.trees
                                |> List.updateAt treeIndex
                                    (Tree.mapChildren (Tree.removeAt path))
              }
            , Cmd.none
            )

        NoOp ->
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
                                            Xy.direction
                                                (turns
                                                    ((1 / 4)
                                                        * ((toFloat i * 2 + 1)
                                                            / toFloat branchCount
                                                          )
                                                    )
                                                )
                                                |> Xy.map
                                                    ((*) (0.1 * toFloat (10 - depth) ^ 3))
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
          , [ Collage.Text.fromString "right click to delete"
            , Collage.Text.fromString "double click to clone 🟦"
            ]
                |> List.map
                    (Collage.Text.color (rgb 1 1 1)
                        >> Collage.Text.size 26
                        >> Collage.rendered
                        >> Collage.Layout.align Collage.Layout.base
                    )
                |> List.intersperse (Collage.Layout.spacer 0 10)
                |> Collage.Layout.vertical
                |> Collage.Layout.align Collage.Layout.base
                |> Collage.shiftY -200
          , Xy.to Collage.rectangle model.windowSize
                |> Collage.filled
                    (rgb 0 0 0 |> Collage.uniform)
          ]
            |> Collage.group
            |> Collage.Events.onMouseUp (\_ -> MouseLifted)
            |> Collage.Events.onMouseMove MouseMoved
            |> Collage.Events.onDoubleClick DoubleClicked
            |> Collage.Render.svgBox model.windowSize
            |> (\collage ->
                    Html.div
                        [ Html.Attributes.style
                            "background-color"
                            "black"
                        , Html.Events.preventDefaultOn "contextmenu"
                            (Json.Decode.succeed ( NoOp, True ))
                        ]
                        [ collage ]
               )
        ]
    }


viewTrees : Model -> Collage Msg
viewTrees { trees, selectedPath } =
    let
        viewTree :
            { treeIndex : Int, path : TreePath }
            -> Tree NodeInfo
            -> Collage Msg
        viewTree indexAndPath tree_ =
            let
                { path, treeIndex } =
                    indexAndPath

                viewConnection child =
                    Collage.path
                        [ ( 0, 0 )
                        , Tree.label child |> .translate
                        ]
                        |> Collage.traced
                            (Collage.solid
                                (4 * (Tree.label tree_).factor)
                                (rgb 0.6 0.4 0 |> Collage.uniform)
                            )
            in
            (Collage.square
                (50 * (Tree.label tree_).factor)
                |> Collage.filled
                    ((if selectedPath == Just indexAndPath then
                        rgb 0 0.5 0.9

                      else
                        rgb 0 0.7 0.4
                     )
                        |> Collage.uniform
                    )
                |> Collage.Events.onMouseDown
                    (PressedOn indexAndPath)
                |> Collage.Events.on "contextmenu"
                    (Json.Decode.succeed
                        (RightClickedOn indexAndPath)
                    )
            )
                :: (Tree.children tree_
                        |> List.map viewConnection
                   )
                |> (++)
                    (Tree.children tree_
                        |> List.indexedMap
                            (\index ->
                                viewTree
                                    { treeIndex = treeIndex
                                    , path = path |> TreePath.toChild index
                                    }
                            )
                    )
                |> Collage.group
                |> Collage.shift
                    (Tree.label tree_).translate
    in
    trees
        |> List.indexedMap
            (\index ->
                viewTree
                    { treeIndex = index
                    , path = TreePath.atTrunk
                    }
            )
        |> Collage.group



--


type alias Update inner outer =
    (inner -> inner) -> outer -> outer


updateTranslate : Update t { r | translate : t }
updateTranslate changeTranslate =
    \t ->
        { t
            | translate =
                t.translate
                    |> changeTranslate
        }
