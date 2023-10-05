# rosetree-path

A path is the location of a branch in a tree (or forest).
With this package you can use paths to navigate and alter
a [gampleman/elm-rosetree](https://package.elm-lang.org/packages/gampleman/elm-rosetree/latest/) or a list of them, also called a "forest".

An alternative way of
keeping track of one focus is a [`Tree.Zipper`](https://package.elm-lang.org/packages/gampleman/elm-rosetree/latest/Tree-Zipper).
However! Paths are nice if you want to
- keep track of many different locations
- easily deal with _potentially_ focussed nodes etc.
- don't want to pollute `Msg`es with potentially large, changing tree content

Example: A big tree on the screen where subtrees can be moved and deleted.

```elm
import Tree.Path exposing (TreePath)
import Tree.Navigate


type alias Model =
    { tree : Tree { translate : ( Float, Float ) }
    , dragged : Maybe TreePath
    }

type Msg =
    = RightClickedOn ForestPath
    | MouseMoved ( Float, Float )
  --| ...


viewTree =
    Tree.Navigate.restructure
        (\sub ->
            (...
                |> onMouseDown (PressedOn sub.path)
                |> (case sub.path |> Tree.Path.step of
                        Just childPath ->
                            onRightClick (RightClickedOn childPath)
                        Nothing ->
                            -- top level node should not be removable
                            identity
                   )
            )
                :: sub.children
                |> group
                |> shift sub.label.translate
        )

update msg model =
    case msg of
        RightClickedOn path ->
            { model
              | tree =
                  model.tree
                    |> Tree.mapChildren
                        (Forest.Navigate.remove path)
            }
        
        MouseMoved mousePosition ->
            case model.dragged of
                Just path ->
                    { model
                      | tree =
                          model.tree
                            |> Tree.Navigate.alter path
                                (Tree.mapLabel
                                    (\l -> { l | translate = ... })
                                )
                    }

                Nothing ->
                    model
        
      --...
```

Complete implementation in [example/](https://github.com/MackeyRMS/rosetree-path/tree/master/example/).
