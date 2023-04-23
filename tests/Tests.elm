module Tests exposing (suite)

import Array
import Expect
import Forest.Navigate
import Forest.Path
import Linear exposing (Direction(..))
import Test exposing (Test, describe, test)
import Tree exposing (Tree, tree)
import Tree.Navigate
import Tree.Path


suite : Test
suite =
    describe "rosetree-path"
        [ treePathTest
        , treeNavigateTest
        ]


treePathTest : Test
treePathTest =
    describe "Tree.Path"
        [ describe "toParent"
            [ test "parent path exists"
                (\() ->
                    Tree.Path.follow [ 1, 2, 3 ]
                        |> Tree.Path.toParent
                        |> Expect.equal
                            (Just (Tree.Path.follow [ 1, 2 ]))
                )
            , test "no parent path"
                (\() ->
                    Tree.Path.follow []
                        |> Tree.Path.toParent
                        |> Expect.equal Nothing
                )
            ]
        , describe "targetsParentOf"
            [ test "yes"
                (\() ->
                    Tree.Path.follow [ 2 ]
                        |> Tree.Path.targetsParentOf
                            (Tree.Path.follow [ 2, 3 ])
                        |> Expect.equal True
                )
            , test "no"
                (\() ->
                    Tree.Path.follow [ 2, 3, 0 ]
                        |> Tree.Path.targetsParentOf
                            (Tree.Path.follow [ 2, 3 ])
                        |> Expect.equal False
                )
            , test "equal"
                (\() ->
                    Tree.Path.follow [ 2, 0, 3 ]
                        |> Tree.Path.targetsParentOf
                            (Tree.Path.follow [ 2, 0, 3 ])
                        |> Expect.equal False
                )
            ]
        , describe "targetsChildOf"
            [ test "yes"
                (\() ->
                    Tree.Path.follow [ 2, 3, 0 ]
                        |> Tree.Path.targetsChildOf
                            (Tree.Path.follow [ 2, 3 ])
                        |> Expect.equal True
                )
            , test "no"
                (\() ->
                    Tree.Path.follow
                        [ 2 ]
                        |> Tree.Path.targetsChildOf
                            (Tree.Path.follow [ 2, 3 ])
                        |> Expect.equal False
                )
            , test "equal"
                (\() ->
                    Tree.Path.follow [ 2, 0, 3 ]
                        |> Tree.Path.targetsChildOf
                            (Tree.Path.follow [ 2, 0, 3 ])
                        |> Expect.equal False
                )
            ]
        ]


treeNavigateTest : Test
treeNavigateTest =
    describe "Tree.Navigate"
        [ describe "to"
            [ test "valid path"
                (\() ->
                    tree "jo"
                        [ leaf "ann"
                        , tree "mic"
                            [ leaf "igg"
                            , leaf "dee"
                            , leaf "bee"
                            ]
                        ]
                        |> Tree.Navigate.to (Tree.Path.follow [ 1, 2 ])
                        |> Expect.equal (Just (leaf "bee"))
                )
            , test "invalid path"
                (\() ->
                    tree "jo"
                        [ leaf "ann"
                        , tree "mic"
                            [ leaf "igg"
                            , leaf "dee"
                            ]
                        ]
                        |> Tree.Navigate.to (Tree.Path.follow [ 1, 2 ])
                        |> Expect.equal Nothing
                )
            ]
        , describe "remove"
            [ test "valid path"
                (\() ->
                    [ leaf "ann"
                    , tree "mic"
                        [ leaf "igg"
                        , leaf "dee"
                        , leaf "bee"
                        ]
                    ]
                        |> Forest.Navigate.remove (Forest.Path.fromIndex 1 (Tree.Path.follow [ 2 ]))
                        |> Expect.equal
                            [ leaf "ann"
                            , tree "mic"
                                [ leaf "igg"
                                , leaf "dee"
                                ]
                            ]
                )
            , test "invalid path"
                (\() ->
                    [ leaf "ann"
                    , tree "mic"
                        [ leaf "igg"
                        , leaf "dee"
                        ]
                    ]
                        |> Forest.Navigate.remove (Forest.Path.fromIndex 1 (Tree.Path.follow [ 2 ]))
                        |> Expect.equal
                            [ leaf "ann"
                            , tree "mic"
                                [ leaf "igg"
                                , leaf "dee"
                                ]
                            ]
                )
            ]
        , describe "alter"
            [ test "valid path"
                (\() ->
                    tree "jo"
                        [ leaf "ann"
                        , tree "mic"
                            [ leaf "igg"
                            , leaf "dee"
                            , leaf "bee"
                            ]
                        ]
                        |> Tree.Navigate.alter (Tree.Path.follow [ 1, 2 ])
                            (Tree.mapLabel String.toUpper)
                        |> Expect.equal
                            (tree "jo"
                                [ leaf "ann"
                                , tree "mic"
                                    [ leaf "igg"
                                    , leaf "dee"
                                    , leaf "BEE"
                                    ]
                                ]
                            )
                )
            , test "invalid path"
                (\() ->
                    tree "jo"
                        [ leaf "ann"
                        , tree "mic"
                            [ leaf "igg"
                            , leaf "dee"
                            ]
                        ]
                        |> Tree.Navigate.alter (Tree.Path.follow [ 1, 2 ])
                            (Tree.mapLabel String.toUpper)
                        |> Expect.equal
                            (tree "jo"
                                [ leaf "ann"
                                , tree "mic"
                                    [ leaf "igg"
                                    , leaf "dee"
                                    ]
                                ]
                            )
                )
            ]
        , let
            tree0To5 : Tree Int
            tree0To5 =
                tree 0
                    [ leaf 1
                    , tree 2
                        [ leaf 3
                        , leaf 4
                        ]
                    , leaf 5
                    ]
          in
          describe "foldFrom"
            [ test "Up"
                (\() ->
                    tree0To5
                        |> Tree.Navigate.foldFrom Array.empty Up (.label >> Array.push)
                        |> Expect.equal
                            ([ 0, 1, 2, 3, 4, 5 ]
                                |> Array.fromList
                            )
                )
            , test "Down"
                (\() ->
                    tree0To5
                        |> Tree.Navigate.foldFrom Array.empty Down (.label >> Array.push)
                        |> Expect.equal
                            ([ 5, 4, 3, 2, 1, 0 ]
                                |> Array.fromList
                            )
                )
            ]
        , test "map"
            (\() ->
                tree 1
                    [ leaf 2
                    , tree 3 [ leaf 4 ]
                    , leaf 5
                    ]
                    |> Tree.Navigate.map (\n -> ( n.label * 2, n.path ))
                    |> Expect.equal
                        (tree ( 2, Tree.Path.atTrunk )
                            [ leaf ( 4, Tree.Path.follow [ 0 ] )
                            , tree ( 6, Tree.Path.follow [ 1 ] )
                                [ leaf ( 8, Tree.Path.follow [ 1, 0 ] ) ]
                            , leaf ( 10, Tree.Path.follow [ 2 ] )
                            ]
                        )
            )
        ]


{-| Short for `Tree.singleton`: A `Tree` without children.
-}
leaf : a -> Tree a
leaf =
    Tree.singleton
