module Forest.Navigate exposing
    ( Forest
    , to
    , alter, remove, map
    )

{-| Additional helpers for a list of [zwilias/elm-rosetree](https://package.elm-lang.org/packages/zwilias/elm-rosetree/latest/)s
using [`ForestPath`](Forest-Path#ForestPath)

@docs Forest


## observe

@docs to


## altering

@docs alter, remove, map

-}

import Forest.Path exposing (ForestPath)
import List.Extra
import Tree exposing (Tree)
import Tree.Navigate
import Tree.Path


{-| A `List` of [zwilias/elm-rosetree](https://package.elm-lang.org/packages/zwilias/elm-rosetree/latest/)s

Example use cases

  - a tree-view without a common parent
  - nestable tabs
  - comments of a post where comment and post types are different

-}
type alias Forest label =
    List (Tree label)


{-| Following the [`ForestPath`](Forest-Path#ForestPath), where do we end?

    import Tree exposing (tree)
    import Forest.Navigate
    import Tree.Path
    import Forest.Path

    [ Tree.singleton "gee"
    , tree "jo"
        [ Tree.singleton "ann"
        , tree "mic"
            [ Tree.singleton "igg"
            , Tree.singleton "dee"
            , Tree.singleton "bee"
            ]
        ]
    ]
        |> Forest.Navigate.to (Forest.Path.fromIndex 1 (Tree.Path.follow [ 1, 2 ]))
    --> Just (Tree.singleton "bee")

    [ Tree.singleton "gee"
    , tree "jo"
        [ Tree.singleton "ann"
        , tree "mic"
            [ Tree.singleton "igg"
            , Tree.singleton "dee"
            ]
        ]
    ]
        |> Forest.Navigate.to (Forest.Path.fromIndex 1 (Tree.Path.follow [ 1, 2 ]))
    --> Nothing

-}
to : ForestPath -> Forest label -> Maybe (Tree label)
to path =
    \forest ->
        forest
            |> List.Extra.getAt (Forest.Path.treeIndex path)
            |> Maybe.andThen (Tree.Navigate.to (Forest.Path.pathIntoTreeAtIndex path))


{-| Change its sub-tree at a given [`ForestPath`](Forest-Path#ForestPath) based on its current value.

    import Tree exposing (tree)
    import Forest.Navigate
    import Tree.Path
    import Forest.Path

    [ Tree.singleton "gee"
    , tree "jo"
        [ Tree.singleton "ann"
        , tree "mic"
            [ Tree.singleton "igg"
            , Tree.singleton "dee"
            , Tree.singleton "bee"
            ]
        ]
    ]
        |> Forest.Navigate.alter (Forest.Path.fromIndex 1 (Tree.Path.follow [ 1, 2 ]))
            (Tree.mapLabel String.toUpper)
    --> [ Tree.singleton "gee"
    --> , tree "jo"
    -->     [ Tree.singleton "ann"
    -->     , tree "mic"
    -->         [ Tree.singleton "igg"
    -->         , Tree.singleton "dee"
    -->         , Tree.singleton "BEE"
    -->         ]
    -->     ]
    --> ]

    [ Tree.singleton "gee"
    , tree "jo"
        [ Tree.singleton "ann"
        , tree "mic"
            [ Tree.singleton "igg"
            , Tree.singleton "dee"
            ]
        ]
    ]
        |> Forest.Navigate.alter (Forest.Path.fromIndex 1 (Tree.Path.follow [ 1, 2 ]))
            (Tree.mapLabel String.toUpper)
    --> [ Tree.singleton "gee"
    --> , tree "jo"
    -->     [ Tree.singleton "ann"
    -->     , tree "mic"
    -->         [ Tree.singleton "igg"
    -->         , Tree.singleton "dee"
    -->         ]
    -->     ]
    --> ]

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
                (Tree.updateChildren (remove furtherInChildren))


{-| Change every tree label based on its [`ForestPath`](Forest-Path#ForestPath) and current value.

    import Tree exposing (tree)
    import Forest.Navigate
    import Forest.Path
    import Tree.Path

    [ Tree.singleton -1
    , tree 1
        [ Tree.singleton 2
        , tree 3 [ Tree.singleton 4 ]
        , Tree.singleton 5
        ]
    ]
        |> Forest.Navigate.map
            (\n -> ( n.label * 2, n.path ))
    --> [ Tree.singleton ( -2, Forest.Path.fromIndex 0 Tree.Path.atTrunk )
    --> , tree ( 2, Forest.Path.fromIndex 1 Tree.Path.atTrunk )
    -->     [ Tree.singleton ( 4, Forest.Path.fromIndex 1 (Tree.Path.follow [ 0 ]) )
    -->     , tree ( 6, Forest.Path.fromIndex 1 (Tree.Path.follow [ 1 ]) )
    -->         [ Tree.singleton ( 8, Forest.Path.fromIndex 1 (Tree.Path.follow [ 1, 0 ]) ) ]
    -->     , Tree.singleton ( 10, Forest.Path.fromIndex 1 (Tree.Path.follow [ 2 ]) )
    -->     ]
    --> ]

-}
map : ({ label : label, path : ForestPath } -> mappedLabel) -> (Forest label -> Forest mappedLabel)
map labelWithPathChange =
    \forest ->
        forest
            |> List.indexedMap
                (\treeIndex tree ->
                    tree
                        |> Tree.Navigate.map
                            (\sub ->
                                { label = sub.label, path = Forest.Path.fromIndex treeIndex sub.path }
                                    |> labelWithPathChange
                            )
                )
