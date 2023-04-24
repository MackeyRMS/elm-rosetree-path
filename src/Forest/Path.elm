module Forest.Path exposing
    ( ForestPath
    , fromIndex
    , treeIndex, pathIntoTreeAtIndex
    , toChild
    )

{-| Location in a [`Forest`](Forest-Navigate#Forest)

@docs ForestPath


## create

@docs fromIndex


## observe

@docs treeIndex, pathIntoTreeAtIndex


## alter

@docs toChild

-}

import Tree.Path exposing (TreePath)


{-| The unique location of a `Tree` in a [`Forest`](Forest-Navigate#Forest).

Describes which tree to step inside
plus the [`TreePath`](Tree-Path#TreePath) to navigate inside that tree.

Represented as a tuple to simplify pattern matching and enable using it as a `comparable` key.

Instead of working with that tuple directly, I recommend using the helpers here to create and operate on these paths.

-}
type alias ForestPath =
    ( Int, TreePath )


{-| Construct a [`ForestPath`](#ForestPath)
from the [`treeIndex`](#treeIndex) and [`pathIntoTreeAtIndex`](#pathIntoTreeAtIndex)

    import Tree
    import Tree.Path
    import Forest.Navigate

    [ Tree.singleton 0
    , Tree.singleton 1
    ]
        |> Forest.Navigate.to (Forest.Path.fromIndex 1 Tree.Path.atTrunk)
    --> Just (Tree.singleton 1)

-}
fromIndex : Int -> TreePath -> ForestPath
fromIndex treeIndexValue pathIntoTreeAtIndexValue =
    ( treeIndexValue, pathIntoTreeAtIndexValue )


{-| Which tree to step inside in a [`Forest`](Forest-Navigate#Forest)
-}
treeIndex : ForestPath -> Int
treeIndex =
    \( treeIndexValue, _ ) -> treeIndexValue


{-| The [`TreePath`](Tree-Path#TreePath) to navigate inside the tree at the [`treeIndex`](#treeIndex).
-}
pathIntoTreeAtIndex : ForestPath -> TreePath
pathIntoTreeAtIndex =
    \( _, pathIntoTreeAtIndexValue ) -> pathIntoTreeAtIndexValue


{-| The path to its ...th child from there.

    import Tree.Path

    Forest.Path.fromIndex 1 Tree.Path.atTrunk
        |> Forest.Path.toChild 4
        |> Forest.Path.toChild 1
        |> Forest.Path.toChild 8
    --> Forest.Path.fromIndex 1 (Tree.Path.follow [ 4, 1, 8 ])

At the first tree:

1.  Take the branch at index 4
2.  Then take its branch at index 1
3.  Its child at index 8 is where the navigation should end

-}
toChild : Int -> ForestPath -> ForestPath
toChild childIndex =
    \forestPath ->
        fromIndex
            (forestPath |> treeIndex)
            (forestPath |> pathIntoTreeAtIndex |> Tree.Path.toChild childIndex)
