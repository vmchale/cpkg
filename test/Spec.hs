import           Package.C
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec

parseHelper :: String -> TargetTriple -> SpecWith ()
parseHelper str tgt =
    parallel $ it "should work on arm-linux-gnueabihf" $
        parse parseTriple "(none)" str
            `shouldParse` tgt

main :: IO ()
main = hspec $
    describe "parseTriple" $ do
        parseHelper "arm-linux-gnueabihf" $ TargetTriple Arm Nothing Linux (Just GNUeabihf)
        parseHelper "mips64el-linux-gnuabi64" $ TargetTriple Mips64El Nothing Linux (Just GNUabi64)
        parseHelper "mips64-linux-gnu" $ TargetTriple Mips64 Nothing Linux (Just GNU)
