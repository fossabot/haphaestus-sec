module Main where

import Test.Hspec
import Graphics.Text.Font.Choose

main :: IO ()
main = hspec spec

test query expect = nameParse query `shouldBe` expect

spec :: Spec
spec = do
    describe "Canary" $ do
        it "Test framework works" $ do
            True `shouldBe` True
    describe "Name Parse" $ do
        it "parses as expected" $ do
            "sans\\-serif" `test` [("family", [(Weak, ValueString "sans-serif")])]
            "Foo-10" `test` [("family", [(Weak, ValueString "Foo")]),
                            ("size", [(Weak, ValueDouble 10.0)])]
            "Foo,Bar-10" `test` [
                ("family", [(Weak, ValueString "Foo"), (Weak, ValueString "Bar")]),
                ("size", [(Weak, ValueDouble 10.0)])]
            "Foo:weight=medium" `test` [("family", [(Weak, ValueString "Foo")]),
                                        ("weight", [(Weak, ValueDouble 100.0)])]
            "Foo:weight_medium" `test` [("family", [(Weak, ValueString "Foo")]),
                                        ("weight", [(Weak, ValueDouble 100.0)])]
            ":medium" `test` [("weight", [(Weak, ValueInt 100)])]
            ":normal" `test` [("width", [(Weak, ValueInt 100)])]
            ":weight=[medium bold]" `test` [
                    ("weight", [(Weak, ValueRange $ Range 100.0 200.0)])]
    -- FIXME test more...
