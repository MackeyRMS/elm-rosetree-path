# tree-with-path

A path is the location of a branch in a tree.
With this package you can use paths to navigate [zwilias/elm-rosetree](https://package.elm-lang.org/packages/zwilias/elm-rosetree/latest/).

A `Tree.Zipper` can also show a specific node in the tree but if you want to
- keep track of many locations
- deal with things like one _potentially_ selected node more easily

→ paths can be a nice alternative.

Example: There's a big tree on the screen. You want to be able to move and delete a subtree.

See in [examples](https://github.com/lue-bird/rosetree-path/tree/master/examples/).

```elm
import TreePath exposing (TreePath)
import Tree.Extra.Lue as Tree

type alias Model =
    { tree : Tree
    , dragged : Maybe TreePath
    }

type Msg =
    = RightClickedOn TreePath
    | MouseMoved ( Float, Float )
    --| ...

update msg model =
    case msg of
        RightClickedOn path ->
            { model
              | trees =
                  model.tree
                      |> Tree.mapChildren
                          (Tree.removeAt path)
            }
        
        MouseMoved mousePosition ->
            case model.dragged of
                Just path ->
                    { model
                      | tree =
                          Tree.updateAt path
                              (Tree.mapLabel
                                  (updateTranslate ...)
                              )
                    }

                Nothing ->
                    model
        
        --...
```
