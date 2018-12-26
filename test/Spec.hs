import           Package.C
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec

main :: IO ()
main = hspec $
    describe "parseTriple" $ do
        parallel $ it "should work on arm-linux-gnueabihf" $
            parse parseTriple "(none)" "arm-linux-gnueabihf"
                `shouldParse` TargetTriple Arm Nothing Linux (Just GNUeabihf)
        parallel $ it "should work on mips64el-linux-gnuabi64" $
            parse parseTriple "(none)" "mips64el-linux-gnuabi64"
                `shouldParse` TargetTriple Mips64El Nothing Linux (Just GNUabi64)
