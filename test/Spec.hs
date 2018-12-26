import           Package.C
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec

main :: IO ()
main = hspec $
    describe "parseTriple" $
        parallel $ it "should work on arm-linux-gnueabihf" $
            parse parseTriple "(none)" "arm-linux-gnueabihf"
                `shouldParse` TargetTriple Arm Nothing Linux (Just GNUeabihf)
