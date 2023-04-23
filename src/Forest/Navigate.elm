module Forest.Navigate exposing
    ( Forest
    , to
    , alter, remove
    )

{-| Additional helpers for a list of [zwilias/elm-rosetree](https://package.elm-lang.org/packages/zwilias/elm-rosetree/latest/)s
using [`ForestPath`](Forest-Path#ForestPath)

@docs Forest


## observe

@docs to


## altering

@docs alter, remove

-}

import Forest.Path exposing (ForestPath)
import List.Extra
import Tree exposing (Tree)
import Tree.Navigate
import Tree.Path exposing (TreePath)


{-| A `List` of [zwilias/elm-rosetree](https://package.elm-lang.org/packages/zwilias/elm-rosetree/latest/)s

Example use cases

  - a tree-view without a common parent
  - nestable tabs
  - comments of a post where comment and post types are different

-}
type alias Forest label =
    List (Tree label)


{-| Following the [`ForestPath`](Forest-Path#ForestPath), where do we end?

TODO example

-}
to : ForestPath -> Forest label -> Maybe (Tree label)
to path =
    \forest ->
        forest
            |> List.Extra.getAt (Forest.Path.treeIndex path)
            |> Maybe.andThen (Tree.Navigate.to (Forest.Path.pathIntoTreeAtIndex path))


{-| Change its sub-tree at a given [`ForestPath`](Forest-Path#ForestPath) based on its current value.

TODO example

-}
alter : ForestPath -> (Tree label -> Tree label) -> Forest label -> Forest label
alter path labelChange nodes =
    List.indexedMap
        (\index ->
            if index == (path |> Forest.Path.treeIndex) then
                Tree.Navigate.alter (path |> Forest.Path.pathIntoTreeAtIndex) labelChange

            else
                identity
        )
        nodes


{-| Remove the sub-`Tree` at the end of a given [`ForestPath`](Forest-Path#ForestPath)
and its children.

    import Tree exposing (tree)
    import Forest.Navigate
    import Tree.Path
    import Forest.Path

    [ Tree.singleton "ann"
    , tree "mic"
        [ Tree.singleton "igg"
        , Tree.singleton "dee"
        , Tree.singleton "bee"
        ]
    ]
        |> Forest.Navigate.remove (Forest.Path.fromIndex 1 (Tree.Path.follow [ 2 ]))
    --> [ Tree.singleton "ann"
    --> , tree "mic"
    -->     [ Tree.singleton "igg"
    -->     , Tree.singleton "dee"
    -->     ]
    --> ]


    [ Tree.singleton "ann"
    , tree "mic"
        [ Tree.singleton "igg"
        , Tree.singleton "dee"
        ]
    ]
        |> Forest.Navigate.remove (Forest.Path.fromIndex 1 (Tree.Path.follow [ 2 ]))
    --> [ Tree.singleton "ann"
    --> , tree "mic"
    -->     [ Tree.singleton "igg"
    -->     , Tree.singleton "dee"
    -->     ]
    --> ]

-}
remove : ForestPath -> Forest label -> Forest label
remove path =
    case path |> Forest.Path.pathIntoTreeAtIndex |> Tree.Path.step of
        Nothing ->
            List.Extra.removeAt (path |> Forest.Path.treeIndex)

        Just furtherInChildren ->
            List.Extra.updateAt (path |> Forest.Path.treeIndex)
                (Tree.mapChildren (remove furtherInChildren))
