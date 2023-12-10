{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec

import Graphics.Layout.Arithmetic
import Data.CSS.Syntax.Tokens (tokenize, Token(..))
import Stylist (PropertyParser(..))
import Stylist.Tree (StyleTree(..))
import Data.Maybe (fromJust)

import Graphics.Layout.Box as B
import Graphics.Layout.Grid as Grid
import Graphics.Layout.Flow
import Graphics.Layout

import Graphics.Layout.CSS
import Graphics.Layout.Grid.Table
import Graphics.Layout.Inline.CSS
import Graphics.Layout.CSS.Font (placeholderFont)
import Graphics.Layout.Flex as Flex

import Data.Text.ParagraphLayout.Rich (constructParagraph,
        defaultParagraphOptions, defaultTextOptions,
        InnerNode(..), Box(..), RootNode(..))
import Data.Text.ParagraphLayout (PageOptions(..))
import Data.Text.Glyphize (Direction(..))

import Graphics.Layout.Grid.CSS (parseASCIIGrid)
import qualified Data.HashMap.Lazy as HM

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "canary" $ do
        it "test framework works" $ do
            True `shouldBe` True
    describe "calc()" $ do
        it "Can perform basic arithmatic" $ do
            runMath "42" `shouldBe` 42
            runMath "6 * 9" `shouldBe` 54
            runMath "6 * 9 - 42" `shouldBe` 12
            runMath "6 * (9 - 42)" `shouldBe` -198
            runMath "6 * calc(9 - 42)" `shouldBe` -198
            runMath "6 * abs(9 - 42)" `shouldBe` 198
    describe "Flow sizing" $ do
        -- Based on http://hixie.ch/tests/adhoc/css/box/block/
        it "Can overflow parent" $ do
            width (fst $ layoutFlow zeroBox {
                    size = Size 3 1
                } lengthBox {
                    border = Border (Pixels 0) (Pixels 0) (Pixels 2) (Pixels 2)
                } []) `shouldBe` 4
            width (fst $ layoutFlow zeroBox {
                    size = Size 3 1
                } lengthBox {
                    padding = Border (Pixels 0) (Pixels 0) (Pixels 2) (Pixels 2)
                } []) `shouldBe` 4
            width (fst $ layoutFlow zeroBox {
                    size = Size 3 1
                } lengthBox {
                    margin = Border (Pixels 0) (Pixels 0) (Pixels 2) (Pixels 2)
                } []) `shouldBe` 4
        it "Fits to parent" $ do
            width (fst $ layoutFlow zeroBox {
                    size = Size 5 1
                } lengthBox {
                    border = Border (Pixels 0) (Pixels 0) (Pixels 2) (Pixels 2),
                    size = Size Auto $ Pixels 1
                } []) `shouldBe` 5
            width (fst $ layoutFlow zeroBox {
                    size = Size 5 1
                } lengthBox {
                    padding = Border (Pixels 0) (Pixels 0) (Pixels 2) (Pixels 2),
                    size = Size Auto $ Pixels 1
                } []) `shouldBe` 5
            width (fst $ layoutFlow zeroBox {
                    size = Size 5 1
                } lengthBox {
                    margin = Border (Pixels 0) (Pixels 0) (Pixels 2) (Pixels 2),
                    size = Size Auto $ Pixels 1
                } []) `shouldBe` 5
        it "Fits children" $ do
            let child = mapX' (lowerLength 100) $ lengthBox {
                size = Size (Pixels 10) (Pixels 10)
              }
            height (fst $ layoutFlow zeroBox {
                size = Size 100 100
              } lengthBox [child, child]) `shouldBe` 20
        it "Collapses margins" $ do
            let a :: PaddedBox Length Double
                a = PaddedBox {
                    B.min = Size 0 Auto,
                    size = Size 0 Auto,
                    B.nat = Size 0 0,
                    B.max = Size 0 Auto,
                    padding = Border (Pixels 0) (Pixels 0) 0 0,
                    border = Border (Pixels 0) (Pixels 0) 0 0,
                    margin = Border (Pixels 5) (Pixels 10) 0 0
                  }
                b :: PaddedBox Length Double
                b = PaddedBox {
                    B.min = Size 0 Auto,
                    size = Size 0 Auto,
                    B.nat = Size 0 0,
                    B.max = Size 0 Auto,
                    padding = Border (Pixels 0) (Pixels 0) 0 0,
                    border = Border (Pixels 0) (Pixels 0) 0 0,
                    margin = Border (Pixels 10) (Pixels 5) 0 0
                  }
            height (fst $ layoutFlow zeroBox {
                    size = Size 100 100
                } lengthBox [a, a]) `shouldBe` 25
            height (fst $ layoutFlow zeroBox {
                    size = Size 100 100
                } lengthBox [b, b]) `shouldBe` 25
            height (fst $ layoutFlow zeroBox {
                    size = Size 100 100
                } lengthBox [a, b]) `shouldBe` 20
            height (fst $ layoutFlow zeroBox {
                    size = Size 100 100
                } lengthBox [b, a]) `shouldBe` 25
    {-describe "Grid" $ do
        it "computes single-columns widths/heights" $ do
            let (pxGrid, pxCells) = gridLayout zeroBox {
                    size = Size 100 100
                  } (buildGrid [Left $ Pixels 10] [Left $ Pixels 10])
                    [GridItem 0 1 0 1 (Size Start Start) zeroBox] True
            containerSize pxGrid `shouldBe` Size 10 10
            fst (head pxCells) `shouldBe` Size 0 0
            let (pcGrid, pcCells) = gridLayout zeroBox {
                    size = Size 100 100
                  } (buildGrid [Left $ Percent 0.5] [Left $ Percent 0.5])
                    [GridItem 0 1 0 1 (Size Start Start) zeroBox] True
            containerSize pcGrid `shouldBe` Size 50 50
            fst (head pcCells) `shouldBe` Size 0 0
            let (autoGrid, autoCells) = gridLayout zeroBox {
                    size = Size 100 100
                  } (buildGrid [Left Auto] [Left Auto])
                    [GridItem 0 1 0 1 (Size Start Start) zeroBox {
                        B.min = Size 10 10,
                        size = Size 20 20
                    }] True
            containerSize autoGrid `shouldBe` Size 20 20
            fst (head autoCells) `shouldBe` Size 0 0
            let (prefGrid, prefCells) = gridLayout zeroBox {
                    size = Size 100 100
                  } (buildGrid [Left Preferred] [Left Preferred])
                    [GridItem 0 1 0 1 (Size Start Start) zeroBox {
                        B.min = Size 10 10,
                        size = Size 15 15
                    }] True
            containerSize prefGrid `shouldBe` Size 15 15
            fst (head prefCells) `shouldBe` Size 0 0
            let (minGrid, minCells) = gridLayout zeroBox {
                    size = Size 100 100
                  } (buildGrid [Left Min] [Left Min])
                    [GridItem 0 1 0 1 (Size Start Start) zeroBox {
                        B.min = Size 10 10,
                        size = Size 15 15
                    }] True
            containerSize minGrid `shouldBe` Size 10 10
            fst (head minCells) `shouldBe` Size 0 0-}
    describe "Abstract layout" $ do
        it "Can overflow parent" $ do
            width (layoutGetBox $ head $ boxLayout zeroBox {
                    size = Size 3 1
                } (LayoutFlow () lengthBox {
                    border = Border (Pixels 0) (Pixels 0) (Pixels 2) (Pixels 2)
                } []) False) `shouldBe` 4
            height (layoutGetBox $ head $ boxLayout zeroBox {
                    size = Size 3 1
                } (LayoutFlow () lengthBox {
                    border = Border (Pixels 2) (Pixels 2) (Pixels 2) (Pixels 2)
                } []) False) `shouldBe` 4
            width (layoutGetBox $ head $ boxLayout zeroBox {
                    size = Size 3 1
                } (LayoutFlow () lengthBox {
                    padding = Border (Pixels 0) (Pixels 0) (Pixels 2) (Pixels 2)
                } []) False) `shouldBe` 4
            height (layoutGetBox $ head $ boxLayout zeroBox {
                    size = Size 3 1
                } (LayoutFlow () lengthBox {
                    padding = Border (Pixels 2) (Pixels 2) (Pixels 2) (Pixels 2)
                } []) False) `shouldBe` 4
            width (layoutGetBox $ head $ boxLayout zeroBox {
                    size = Size 3 1
                } (LayoutFlow () lengthBox {
                    margin = Border (Pixels 0) (Pixels 0) (Pixels 2) (Pixels 2)
                } []) False) `shouldBe` 4
            height (layoutGetBox $ head $ boxLayout zeroBox {
                    size = Size 3 1
                } (LayoutFlow () lengthBox {
                    margin = Border (Pixels 2) (Pixels 2) (Pixels 2) (Pixels 2)
                } []) False) `shouldBe` 4
        it "Fits to parent" $ do
            width (layoutGetBox $ head $ boxLayout zeroBox {
                    size = Size 5 1
                } (LayoutFlow () lengthBox {
                    border = Border (Pixels 0) (Pixels 0) (Pixels 2) (Pixels 2),
                    size = Size Auto $ Pixels 1
                } []) False) `shouldBe` 5
            width (layoutGetBox $ head $ boxLayout zeroBox {
                    size = Size 5 1
                } (LayoutFlow () lengthBox {
                    padding = Border (Pixels 0) (Pixels 0) (Pixels 2) (Pixels 2),
                    size = Size Auto $ Pixels 1
                } []) False) `shouldBe` 5
            width (layoutGetBox $ head $ boxLayout zeroBox {
                    size = Size 5 1
                } (LayoutFlow () lengthBox {
                    margin = Border (Pixels 0) (Pixels 0) (Pixels 2) (Pixels 2),
                    size = Size Auto $ Pixels 1
                } []) False) `shouldBe` 5
        it "Fits children" $ do
            let child = LayoutFlow () lengthBox {
                size = Size (Pixels 10) (Pixels 10),
                B.max = Size (Pixels 10) (Pixels 10)
              } []
            height (layoutGetBox $ head $ boxLayout zeroBox {
                size = Size 100 100
              } child False) `shouldBe` 10
            height (layoutGetBox $ head $ boxLayout zeroBox {
                size = Size 100 100
              } (LayoutFlow () lengthBox [child, child]) False) `shouldBe` 20
        it "Collapses margins" $ do
            let a :: LayoutItem Length Length ()
                a = LayoutFlow () PaddedBox {
                    B.min = Size Auto Auto,
                    size = Size Auto Auto,
                    B.nat = Size 0 0,
                    B.max = Size Auto Auto,
                    padding = Border (Pixels 0) (Pixels 0) (Pixels 0) (Pixels 0),
                    border = Border (Pixels 0) (Pixels 0) (Pixels 0) (Pixels 0),
                    margin = Border (Pixels 5) (Pixels 10) (Pixels 0) (Pixels 0)
                  } []
                b :: LayoutItem Length Length ()
                b = LayoutFlow () PaddedBox {
                    B.min = Size Auto Auto,
                    size = Size Auto Auto,
                    B.nat = Size 0 0,
                    B.max = Size Auto Auto,
                    padding = Border (Pixels 0) (Pixels 0) (Pixels 0) (Pixels 0),
                    border = Border (Pixels 0) (Pixels 0) (Pixels 0) (Pixels 0),
                    margin = Border (Pixels 10) (Pixels 5) (Pixels 0) (Pixels 0)
                  } []
            height (layoutGetBox $ head $ boxLayout zeroBox {
                    size = Size 100 100
                } (LayoutFlow () lengthBox [a, a]) False) `shouldBe` 25
            height (layoutGetBox $ head $ boxLayout zeroBox {
                    size = Size 100 100
                } (LayoutFlow () lengthBox [b, b]) False) `shouldBe` 25
            height (layoutGetBox $ head $ boxLayout zeroBox {
                    size = Size 100 100
                } (LayoutFlow () lengthBox [a, b]) False) `shouldBe` 20
            height (layoutGetBox $ head $ boxLayout zeroBox {
                    size = Size 100 100
                } (LayoutFlow () lengthBox [b, a]) False) `shouldBe` 25

        {-it "computes single-columns widths/heights" $ do
            let zeroCell = LayoutFlow () lengthBox []
            let nonzeroCell = LayoutFlow () lengthBox {
                B.min = Size (Pixels 10) (Pixels 10),
                size = Size (Pixels 20) (Pixels 20)
              } []

            let LayoutGrid (_, _) pxGrid pxCells = boxLayout zeroBox {
                    size = Size 100 100
                  } (LayoutGrid () (buildGrid [Left $ Pixels 10] [Left $ Pixels 10])
                    [(GridItem 0 1 0 1 (Size Start Start) lengthBox, zeroCell)]) True
            let LayoutFlow (pos, _) _ _ = snd $ head pxCells
            containerSize pxGrid `shouldBe` Size 10 10
            pos `shouldBe` (0, 0)
            let LayoutGrid (_, _) pxGrid pxCells = boxLayout zeroBox {
                    size = Size 100 100
                  } (LayoutGrid () (buildGrid [Left $ Percent 0.5] [Left $ Percent 0.5])
                    [(GridItem 0 1 0 1 (Size Start Start) lengthBox, zeroCell)]) True
            let LayoutFlow (pos, _) _ _ = snd $ head pxCells
            containerSize pxGrid `shouldBe` Size 50 50
            pos `shouldBe` (0, 0)
            let LayoutGrid (_, _) pxGrid pxCells = boxLayout zeroBox {
                    size = Size 100 100
                  } (LayoutGrid () (buildGrid [Left Auto] [Left Auto])
                    [(GridItem 0 1 0 1 (Size Start Start) lengthBox, nonzeroCell)]) True
            let LayoutFlow (pos, _) _ _ = snd $ head pxCells
            containerSize pxGrid `shouldBe` Size 20 10 -- FIXME Is the 10 correct?
            pos `shouldBe` (0, 0)
            let LayoutGrid (_, _) pxGrid pxCells = boxLayout zeroBox {
                    size = Size 100 100
                  } (LayoutGrid () (buildGrid [Left Preferred] [Left Preferred])
                    [(GridItem 0 1 0 1 (Size Start Start) lengthBox, nonzeroCell)]) True
            let LayoutFlow (pos, _) _ _ = snd $ head pxCells
            containerSize pxGrid `shouldBe` Size 20 0 -- FIXME Is the 0 correct?
            pos `shouldBe` (0, 0)
            let LayoutGrid (_, _) pxGrid pxCells = boxLayout zeroBox {
                    size = Size 100 100
                  } (LayoutGrid () (buildGrid [Left Min] [Left Min])
                    [(GridItem 0 1 0 1 (Size Start Start) lengthBox, nonzeroCell)]) True
            let LayoutFlow (pos, _) _ _ = snd $ head pxCells
            containerSize pxGrid `shouldBe` Size 10 10
            pos `shouldBe` (0, 0) -}
    describe "Grid templates" $ do
        it "parses successfully" $ do
            let grid = fromJust $ parseASCIIGrid [["head", "head"],
                                                  ["nav", "main"],
                                                  ["foot", "."]] 0 HM.empty
            HM.lookup "head" grid `shouldBe` Just ((0,2), (0, Just 1))
            HM.lookup "nav" grid `shouldBe` Just ((0,1), (1, Just 2))
            HM.lookup "main" grid `shouldBe` Just ((1,2), (1, Just 2))
            HM.lookup "foot" grid `shouldBe` Just ((0,1), (2, Nothing))
            HM.lookup "aside" grid `shouldBe` Nothing
        it "discards invalid non-squares" $ do
            let test grid = parseASCIIGrid grid 0 HM.empty `shouldBe` Nothing
            test [["head", "nav", "head"]]
            test [["head"], ["nav"], ["head"]]
            test [["head", "head"], ["head", "nav"]]
    describe "<table>" $ do
        it "parses to grids" $ do
            -- <table>
            --  <caption>Test table</caption>
            --  <thead><tr><th>A</th><th rowspan="2">B</th><th>C</th></tr></thead>
            --  <tbody><tr><td colspan="2">D</td><td colspan="2">E</td></tr></tbody>
            --  <tfoot><tr><td>F</td><td>G</td><td>H</td></tr></tfoot>
            -- </table>
            let text' txt = StyleTree temp { inlineStyles = plaintext txt } []
            let table :: StyleTree (CSSBox ())
                table = StyleTree temp { display = Table } [
                  StyleTree temp { display = TableHeaderGroup } [
                      StyleTree temp { display = TableRow } [
                          StyleTree temp { display = TableCell } [text' "A"],
                          StyleTree temp {
                              display = TableCell,
                              tableOptions = temp { rowspan = 2 }
                          } [text' "B"],
                          StyleTree temp { display = TableCell } [text' "C"]
                      ]
                  ],
                  StyleTree temp { display = TableRowGroup } [
                      StyleTree temp { display = TableRow } [
                          StyleTree temp {
                              display = TableCell,
                              tableOptions = temp { colspan = 2 }
                          } [text' "D"],
                          StyleTree temp {
                              display = TableCell,
                              tableOptions = temp { colspan = 2 }
                          } [text' "E"]
                      ]
                  ],
                  StyleTree temp { display = TableFooterGroup } [
                      StyleTree temp { display = TableRow } [
                          StyleTree temp { display = TableCell } [text' "F"],
                          StyleTree temp { display = TableCell } [text' "G"],
                          StyleTree temp { display = TableCell } [text' "H"]
                      ]
                  ],
                  StyleTree temp { display = TableCaption } [text' "Test table"]
                 ]
            let defaultPageOptions = PageOptions 0 0 2 2
            let gridItem x y = GridItem {
                cellStart = x, cellEnd = y,
                Grid.alignment = Start,
                minSize = 0, natSize = 0
              }
            let track cells' = Track {
                cells = cells',
                trackMins = [], trackNats = [], gap = Pixels 0
            }
            let inline txt = LayoutInline () (constructParagraph "" (
                        RootBox $ Box [
                            TextSequence ((placeholderFont, 12), zero, ()) txt
                        ] $ defaultTextOptions DirLTR
                    ) "" defaultParagraphOptions) defaultPageOptions
            finalizeCSS placeholderFont placeholderFont table `shouldBe`
                    LayoutFlow () lengthBox [
                        LayoutFlow () lengthBox [inline "Test table"],
                        LayoutGrid () Size {
                           inline = track [Left Auto, Left Auto, Left Auto, Left Auto, Left Auto, Left Auto],
                           block = track [Left Auto, Left Auto, Left Auto]
                        } [
                            gridItem 0 6 `Size` gridItem 0 1,
                            gridItem 0 6 `Size` gridItem 1 2,
                            gridItem 0 6 `Size` gridItem 2 3,
                            gridItem 0 6 `Size` gridItem 3 4,
                            gridItem 0 6 `Size` gridItem 4 5,
                            gridItem 0 6 `Size` gridItem 5 6,
                            gridItem 1 2 `Size` gridItem 0 1,
                            gridItem 3 4 `Size` gridItem 0 2,
                            gridItem 5 6 `Size` gridItem 0 1,
                            gridItem 1 3 `Size` gridItem 1 2,
                            gridItem 4 6 `Size` gridItem 1 2,
                            gridItem 1 2 `Size` gridItem 2 3,
                            gridItem 3 4 `Size` gridItem 2 3,
                            gridItem 5 6 `Size` gridItem 2 3
                        ] [
                            LayoutFlow () lengthBox [],
                            LayoutFlow () lengthBox [],
                            LayoutFlow () lengthBox [],
                            LayoutFlow () lengthBox [],
                            LayoutFlow () lengthBox [],
                            LayoutFlow () lengthBox [],
                            LayoutFlow () lengthBox [inline "A"],
                            LayoutFlow () lengthBox [inline "B"],
                            LayoutFlow () lengthBox [inline "C"],
                            LayoutFlow () lengthBox [inline "D"],
                            LayoutFlow () lengthBox [inline "E"],
                            LayoutFlow () lengthBox [inline "F"],
                            LayoutFlow () lengthBox [inline "G"],
                            LayoutFlow () lengthBox [inline "H"]
                        ]
                    ]
    describe "Flexbox" $ do
        it "wraps properly" $ do
            let child l = FlexChild {
                grow = 0,
                shrink = 0,
                basis = l,
                Flex.alignment = AlStart,
                flexInner = ()
              }
            let baseFlex = FlexParent {
                direction = Row,
                reverseRows = False,
                wrap = NoWrap,
                justify = JStart,
                alignLines = Just JStart,
                baseGap = 2,
                crossGap = 2,
                Flex.children = [[(child 10) { grow = 1 }, (child 20) { shrink = 1 },
                        (child 30) { grow = 2 }, (child 40) { shrink = 2 }]],
                pageWidth = 0
              } :: FlexParent () Double
            -- These test results don't look right...
            flexWrap baseFlex 50 `shouldBe` baseFlex {
                Flex.children = [[(child 10) { grow = 1 },
                        (child 1.3333333333333321) { shrink = 1 }, (child 30) { grow = 2 },
                        (child 2.6666666666666643) { shrink = 2}]]
              }
            flexWrap baseFlex 40 `shouldBe` baseFlex {
                Flex.children = [[(child 10) { grow = 1 },
                        (child $ -2) { shrink = 1 }, (child 30) { grow = 2 },
                        (child $ -4) { shrink = 2}]]
              }
            flexWrap baseFlex 30 `shouldBe` baseFlex {
                Flex.children = [[(child 10) { grow = 1 },
                        (child $ -5.333333333333332) { shrink = 1 }, (child 30) { grow = 2 },
                        (child $ -10.666666666666664) { shrink = 2}]]
              }
            flexWrap baseFlex 20 `shouldBe` baseFlex {
                Flex.children = [[(child 10) { grow = 1 },
                        (child $ -8.666666666666668) { shrink = 1 }, (child 30) { grow = 2 },
                        (child $ -17.333333333333336) { shrink = 2}]]
              }
            flexWrap baseFlex 10 `shouldBe` baseFlex {
                Flex.children = [[(child 10) { grow = 1 },
                        (child $ -12) { shrink = 1 }, (child 30) { grow = 2 },
                        (child $ -24) { shrink = 2}]]
              }
            flexWrap baseFlex { direction = Flex.Column } 50 `shouldBe` baseFlex {
                direction = Flex.Column,
                Flex.children = [[(child 10) { grow = 1 },
                        (child 1.3333333333333321) { shrink = 1 }, (child 30) { grow = 2 },
                        (child 2.6666666666666643) { shrink = 2}]]
              }
            flexWrap baseFlex { direction = Flex.Column } 40 `shouldBe` baseFlex {
                direction = Flex.Column,
                Flex.children = [[(child 10) { grow = 1 },
                        (child $ -2) { shrink = 1 }, (child 30) { grow = 2 },
                        (child $ -4) { shrink = 2}]]
              }
            flexWrap baseFlex { direction = Flex.Column } 30 `shouldBe` baseFlex {
                direction = Flex.Column,
                Flex.children = [[(child 10) { grow = 1 },
                        (child $ -5.333333333333332) { shrink = 1 }, (child 30) { grow = 2 },
                        (child $ -10.666666666666664) { shrink = 2}]]
              }
            flexWrap baseFlex { direction = Flex.Column } 20 `shouldBe` baseFlex {
                direction = Flex.Column,
                Flex.children = [[(child 10) { grow = 1 },
                        (child $ -8.666666666666668) { shrink = 1 }, (child 30) { grow = 2 },
                        (child $ -17.333333333333336) { shrink = 2}]]
              }
            flexWrap baseFlex { direction = Flex.Column } 10 `shouldBe` baseFlex {
                direction = Flex.Column,
                Flex.children = [[(child 10) { grow = 1 },
                        (child $ -12) { shrink = 1 }, (child 30) { grow = 2 },
                        (child $ -24) { shrink = 2}]]
              }
            flexWrap baseFlex { reverseRows = True } 50 `shouldBe` baseFlex {
                reverseRows = True,
                Flex.children = [[
                    FlexChild {
                        grow = 0.0,
                        shrink = 2.0,
                        basis = 2.6666666666666643,
                        Flex.alignment = AlStart,
                        flexInner = ()
                    },
                    FlexChild {
                        grow = 2.0,
                        shrink = 0.0,
                        basis = 30.0,
                        Flex.alignment = AlStart,
                        flexInner = ()
                    },
                    FlexChild {
                        grow = 0.0,
                        shrink = 1.0,
                        basis = 1.3333333333333321,
                        Flex.alignment = AlStart,
                        flexInner = ()
                    },
                    FlexChild {
                       grow = 1.0,
                       shrink = 0.0,
                       basis = 10.0,
                       Flex.alignment = AlStart,
                       flexInner = ()
                    }
                ]]
              }
            flexWrap baseFlex { reverseRows = True } 40 `shouldBe` baseFlex {
                reverseRows = True,
                Flex.children = [[
                    FlexChild {
                        grow = 0.0,
                        shrink = 2.0,
                        basis = -4,
                        Flex.alignment = AlStart,
                        flexInner = ()
                    },
                    FlexChild {
                        grow = 2.0,
                        shrink = 0.0,
                        basis = 30.0,
                        Flex.alignment = AlStart,
                        flexInner = ()
                    },
                    FlexChild {
                        grow = 0.0,
                        shrink = 1.0,
                        basis = -2,
                        Flex.alignment = AlStart,
                        flexInner = ()
                    },
                    FlexChild {
                       grow = 1.0,
                       shrink = 0.0,
                       basis = 10.0,
                       Flex.alignment = AlStart,
                       flexInner = ()
                    }
                ]]
            }
            flexWrap baseFlex { reverseRows = True } 30 `shouldBe` baseFlex {
                reverseRows = True,
                Flex.children = [[
                    FlexChild {
                        grow = 0.0,
                        shrink = 2.0,
                        basis = -10.666666666666664,
                        Flex.alignment = AlStart,
                        flexInner = ()
                    },
                    FlexChild {
                        grow = 2.0,
                        shrink = 0.0,
                        basis = 30.0,
                        Flex.alignment = AlStart,
                        flexInner = ()
                    },
                    FlexChild {
                        grow = 0.0,
                        shrink = 1.0,
                        basis = -5.333333333333332,
                        Flex.alignment = AlStart,
                        flexInner = ()
                    },
                    FlexChild {
                       grow = 1.0,
                       shrink = 0.0,
                       basis = 10.0,
                       Flex.alignment = AlStart,
                       flexInner = ()
                    }
                ]]
            }
            flexWrap baseFlex { reverseRows = True } 20 `shouldBe` baseFlex {
                reverseRows = True,
                Flex.children = [[
                    FlexChild {
                        grow = 0.0,
                        shrink = 2.0,
                        basis = -17.333333333333336,
                        Flex.alignment = AlStart,
                        flexInner = ()
                    },
                    FlexChild {
                        grow = 2.0,
                        shrink = 0.0,
                        basis = 30.0,
                        Flex.alignment = AlStart,
                        flexInner = ()
                    },
                    FlexChild {
                        grow = 0.0,
                        shrink = 1.0,
                        basis = -8.666666666666668,
                        Flex.alignment = AlStart,
                        flexInner = ()
                    },
                    FlexChild {
                       grow = 1.0,
                       shrink = 0.0,
                       basis = 10.0,
                       Flex.alignment = AlStart,
                       flexInner = ()
                    }
                ]]
              }
            flexWrap baseFlex { reverseRows = True } 10 `shouldBe` baseFlex {
                reverseRows = True,
                Flex.children = [[
                    FlexChild {
                        grow = 0.0,
                        shrink = 2.0,
                        basis = -24,
                        Flex.alignment = AlStart,
                        flexInner = ()
                    },
                    FlexChild {
                        grow = 2.0,
                        shrink = 0.0,
                        basis = 30.0,
                        Flex.alignment = AlStart,
                        flexInner = ()
                    },
                    FlexChild {
                        grow = 0.0,
                        shrink = 1.0,
                        basis = -12,
                        Flex.alignment = AlStart,
                        flexInner = ()
                    },
                    FlexChild {
                       grow = 1.0,
                       shrink = 0.0,
                       basis = 10.0,
                       Flex.alignment = AlStart,
                       flexInner = ()
                    }
                ]]
              }
            flexWrap baseFlex { wrap = Wrap } 50 `shouldBe` baseFlex {
                wrap = Wrap,
                Flex.children = [[(child 10) { grow = 1 },
                        (child 6) { shrink = 1 }, (child 30) { grow = 2 }], [
                        (child 40) { shrink = 2}]]
              }
            flexWrap baseFlex { wrap = Wrap } 40 `shouldBe` baseFlex {
                wrap = Wrap,
                Flex.children = [[(child 18) { grow = 1 },
                        (child 20) { shrink = 1 }], [(child 40) { grow = 2 }],
                        [(child 40) { shrink = 2}]]
              }
            flexWrap baseFlex { wrap = Wrap } 30 `shouldBe` baseFlex {
                wrap = Wrap,
                Flex.children = [[(child 10) { grow = 1 },
                        (child 18) { shrink = 1 }], [(child 30) { grow = 2 }],
                        [(child 30) { shrink = 2}]]
              }
            flexWrap baseFlex { wrap = Wrap } 20 `shouldBe` baseFlex {
                wrap = Wrap,
                Flex.children = [[(child 20) { grow = 1 }],
                        [(child 20) { shrink = 1 }], [(child 30) { grow = 2 }],
                        [(child 20) { shrink = 2}]]
              }
            flexWrap baseFlex { wrap = Wrap } 10 `shouldBe` baseFlex {
                wrap = Wrap,
                Flex.children = [[(child 10) { grow = 1 }], [(child 10) { shrink = 1 }],
                        [(child 30) { grow = 2 }], [(child 10) { shrink = 2}]]
              }
            flexWrap baseFlex { wrap = WrapReverse } 50 `shouldBe` baseFlex {
                wrap = WrapReverse,
                Flex.children = [[(child 40) { shrink = 2}], [(child 10) { grow = 1 },
                        (child 6) { shrink = 1 }, (child 30) { grow = 2 }]]
              }
            flexWrap baseFlex { wrap = WrapReverse } 40 `shouldBe` baseFlex {
                wrap = WrapReverse,
                Flex.children = [[(child 40) { shrink = 2}], [(child 40) { grow = 2 }],
                        [(child 18) { grow = 1 }, (child 20) { shrink = 1 }]]
              }
            flexWrap baseFlex { wrap = WrapReverse } 30 `shouldBe` baseFlex {
                wrap = WrapReverse,
                Flex.children = [[(child 30) { shrink = 2}], [(child 30) { grow = 2 }],
                        [(child 10) { grow = 1 }, (child 18) { shrink = 1 }]]
              }
            flexWrap baseFlex { wrap = WrapReverse } 20 `shouldBe` baseFlex {
                wrap = WrapReverse,
                Flex.children = [[(child 20) { shrink = 2}], [(child 30) { grow = 2 }],
                        [(child 20) { shrink = 1 }], [(child 20) { grow = 1 }]]
              }
            flexWrap baseFlex { wrap = WrapReverse } 10 `shouldBe` baseFlex {
                wrap = WrapReverse,
                Flex.children = [[(child 10) { shrink = 2}], [(child 30) { grow = 2 }],
                        [(child 10) { shrink = 1 }], [(child 10) { grow = 1 }]]
              }
        it "Paginates properly" $ do
            let child l align = FlexChild {
                grow = 0,
                shrink = 0,
                basis = l,
                Flex.alignment = align,
                flexInner = l
              }
            let baseFlex = FlexParent {
                direction = Row,
                reverseRows = False,
                wrap = Wrap,
                justify = JStart,
                alignLines = Just JStart,
                baseGap = 2,
                crossGap = 2,
                Flex.children = [[(child 10 AlStart) { grow = 1 },
                        (child 20 AlCenter) { shrink = 1 },
                        (child 30 AlEnd) { grow = 2 },
                        (child 40 AlStretch) { shrink = 2 }]],
                pageWidth = 80
              } :: FlexParent Double Double
            let self = flexWrap baseFlex 50
            self `shouldBe` baseFlex {
                Flex.children = [[(child 10 AlStart) { grow = 1 },
                        (child 6 AlCenter) { shrink = 1, flexInner = 20 },
                        (child 30 AlEnd) { grow = 2 }], [
                        (child 40 AlStretch) { shrink = 2}]]
              }
            flexSplit (\x -> Size x x) 30 50 self `shouldBe` (baseFlex {
                Flex.children = [[(child 10 AlStart) { grow = 1 },
                        (child 6 AlCenter) { shrink = 1, flexInner = 20 },
                        (child 30 AlEnd) { grow = 2 }]]
              }, baseFlex {
                Flex.children = [[(child 40 AlStretch) { shrink = 2}]]
              })
            print self { direction = Flex.Column }
            flexSplit (\x -> Size x x) 40 50 self { direction = Flex.Column } `shouldBe` (baseFlex {
                direction = Flex.Column,
                Flex.children = [[(child 10 AlStart) { grow = 1 },
                        (child (-4) AlCenter) { shrink = 1, flexInner = 20 },
                        (child 30 AlEnd) { grow = 2 }], [(child 40 AlStretch) { shrink = 2}]]
              }, baseFlex {
                direction = Flex.Column,
                Flex.children = []
              })
            flexSplit (\x -> Size x x) 10 50 self { direction = Flex.Column } `shouldBe` (baseFlex {
                direction = Flex.Column,
                Flex.children = []
              }, baseFlex {
                direction = Flex.Column,
                Flex.children = [[(child 10 AlStart) { grow = 1 },
                        (child 6 AlCenter) { shrink = 1, flexInner = 20 },
                        (child 30 AlEnd) { grow = 2 }], [(child 40 AlStretch) { shrink = 2}]]
              })
            flexSplit (\x -> Size x x) 10 50 self { direction = Flex.Column, pageWidth = 40 } `shouldBe` (baseFlex {
                Flex.children = [[(child 10 AlStart) { grow = 1 },
                        (child 6 AlCenter) { shrink = 1, flexInner = 20 },
                        (child 30 AlEnd) { grow = 2 }]],
                pageWidth = 40
              }, baseFlex {
                Flex.children = [[(child 40 AlStretch) { shrink = 2}]],
                pageWidth = 40
              })
        it "positions correctly" $ do
            let child l align = FlexChild {
                grow = 0,
                shrink = 0,
                basis = l,
                Flex.alignment = align,
                flexInner = l
              }
            let child' l align pos = FlexChild {
                grow = 0, shrink = 0, basis = l,
                Flex.alignment = align, flexInner = (pos, l)
              }
            let flex childs = FlexParent {
                direction = Row,
                reverseRows = False,
                wrap = Wrap,
                justify = JStart,
                alignLines = Just JStart,
                baseGap = 2,
                crossGap = 2,
                Flex.children = childs,
                pageWidth = 80
              }
            let baseFlex = flex [[(child 10 AlStart) { grow = 1 },
                    (child 20 AlCenter) { shrink = 1 },
                    (child 30 AlEnd) { grow = 2 },
                    (child 40 AlStretch) { shrink = 2 }]] :: FlexParent Double Double
            flexPosition (,) (\a -> Size a a) (10, 10) (Size 50 50) baseFlex `shouldBe` flex [[
                (child' 10 AlStart (10, 10)) { grow = 1 },
                (child' 20 AlCenter (22, 20)) { shrink = 1 },
                (child' 30 AlEnd (44, 20)) { grow = 2 },
                (child' 40 AlStretch (76, 10)) { shrink = 2 }
              ]]


runMath = flip evalCalc [] . mapCalc fst . flip parseCalc [] . filter (/= Whitespace) . tokenize

instance PropertyParser () where
    temp = ()
    inherit _ = ()
    longhand _ _ _ _ = Nothing
