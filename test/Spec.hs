import Library
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "Punto 1: Gimnastas saludables" $ do
        it "funca" $ do
            True `shouldBe` True
        