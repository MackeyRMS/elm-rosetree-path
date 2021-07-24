module Tests exposing (suite)

import Expect exposing (Expectation)
import Test exposing (Test, describe, test)
import Tree exposing (tree)
import Tree.Extra.Lue as Tree exposing (leaf)
import TreePath exposing (TreePath)


suite : Test
suite =
    describe "rosetree-path"
        [ pathTest
        , pathWithTreeTest
        ]


pathTest : Test
pathTest =
    describe "path"
        [ describe "toParent"
            [ test "parent path exists"
                (\() ->
                    TreePath.go [ 1, 2, 3 ]
                        |> TreePath.toParent
                        |> Expect.equal
                            (Just (TreePath.go [ 1, 2 ]))
                )
            , test "no parent path"
                (\() ->
                    TreePath.go []
                        |> TreePath.toParent
                        |> Expect.equal Nothing
                )
            ]
        , describe "goesToParentOf"
            [ test "yes"
                (\() ->
                    TreePath.go [ 2 ]
                        |> TreePath.goesToParentOf
                            (TreePath.go [ 2, 3 ])
                        |> Expect.equal True
                )
            , test "no"
                (\() ->
                    TreePath.go [ 2, 3, 0 ]
                        |> TreePath.goesToParentOf
                            (TreePath.go [ 2, 3 ])
                        |> Expect.equal False
                )
            , test "equal"
                (\() ->
                    TreePath.go [ 2, 0, 3 ]
                        |> TreePath.goesToParentOf
                            (TreePath.go [ 2, 0, 3 ])
                        |> Expect.equal False
                )
            ]
        , describe "goesToChildOf"
            [ test "yes"
                (\() ->
                    TreePath.go [ 2, 3, 0 ]
                        |> TreePath.goesToChildOf
                            (TreePath.go [ 2, 3 ])
                        |> Expect.equal True
                )
            , test "no"
                (\() ->
                    TreePath.go
                        [ 2 ]
                        |> TreePath.goesToChildOf
                            (TreePath.go [ 2, 3 ])
                        |> Expect.equal False
                )
            , test "equal"
                (\() ->
                    TreePath.go [ 2, 0, 3 ]
                        |> TreePath.goesToChildOf
                            (TreePath.go [ 2, 0, 3 ])
                        |> Expect.equal False
                )
            ]
        ]


pathWithTreeTest : Test
pathWithTreeTest =
    describe "path together with normal tree"
        [ describe "at"
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
                        |> Tree.at (TreePath.go [ 1, 2 ])
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
                        |> Tree.at (TreePath.go [ 1, 2 ])
                        |> Expect.equal Nothing
                )
            ]
        , describe "replaceAt"
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
                        |> Tree.replaceAt (TreePath.go [ 1, 2 ])
                            (leaf "be")
                        |> Expect.equal
                            (tree "jo"
                                [ leaf "ann"
                                , tree "mic"
                                    [ leaf "igg"
                                    , leaf "dee"
                                    , leaf "be"
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
                        |> Tree.replaceAt (TreePath.go [ 1, 2 ])
                            (leaf "be")
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
        , describe "removeAt"
            [ test "valid path"
                (\() ->
                    [ leaf "ann"
                    , tree "mic"
                        [ leaf "igg"
                        , leaf "dee"
                        , leaf "bee"
                        ]
                    ]
                        |> Tree.removeAt (TreePath.go [ 1, 2 ])
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
                        |> Tree.removeAt (TreePath.go [ 1, 2 ])
                        |> Expect.equal
                            [ leaf "ann"
                            , tree "mic"
                                [ leaf "igg"
                                , leaf "dee"
                                ]
                            ]
                )
            ]
        , describe "updateAt"
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
                        |> Tree.updateAt (TreePath.go [ 1, 2 ])
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
                        |> Tree.updateAt (TreePath.go [ 1, 2 ])
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
        , test "prependChildren"
            (\() ->
                tree "dear" [ leaf "George" ]
                    |> Tree.prependChildren
                        [ leaf "May", leaf "and" ]
                    |> Expect.equal
                        (tree "dear"
                            [ leaf "May", leaf "and", leaf "George" ]
                        )
            )
        , test "appendChildren"
            (\() ->
                tree "hello" [ leaf "you" ]
                    |> Tree.appendChildren
                        [ leaf "and", leaf "you" ]
                    |> Expect.equal
                        (tree "hello"
                            [ leaf "you", leaf "and", leaf "you" ]
                        )
            )
        ]
