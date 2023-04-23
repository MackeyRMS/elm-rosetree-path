# rosetree-path

A path is the location of a branch in a tree.
With this package you can use paths to navigate and alter a [zwilias/elm-rosetree](https://package.elm-lang.org/packages/zwilias/elm-rosetree/latest/).

A [`Tree.Zipper`](https://package.elm-lang.org/packages/zwilias/elm-rosetree/latest/Tree-Zipper) can also show a specific node in the tree.
However! Paths can be a nice alternative if you want to
- keep track of many locations
- easily deal with _potentially_ focussed nodes etc.
- don't want to pollute `Msg`es with potentially large, changing tree content

Example: There's a big tree on the screen. You want to be able to move and delete a subtree.

See in [examples](https://github.com/lue-bird/rosetree-path/tree/master/example/).

```elm
import Tree.Path exposing (TreePath)
import Tree.Navigate


type alias Model =
    { tree : Tree { translate : ( Float, Float ) }
    , dragged : Maybe TreePath
    }

viewTree =
    Tree.restructure
        (\sub ->
            (sub.label
                |> ...
                |> onMouseDown (PressedOn sub.path)
                |> onRightClick (RightClickedOn sub.path)
            )
                :: sub.children
                |> group
                |> shift sub.label.translate
        )

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
                          (Tree.Navigate.remove path)
            }
        
        MouseMoved mousePosition ->
            case model.dragged of
                Just path ->
                    { model
                      | tree =
                          Tree.Navigate.alter path
                              (Tree.mapLabel
                                  (\l -> { l | translate = ... })
                              )
                    }

                Nothing ->
                    model
        
      --...
```
