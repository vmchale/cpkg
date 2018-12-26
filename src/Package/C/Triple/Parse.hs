module Package.C.Triple.Parse ( parseTriple
                              , parseTripleIO
                              ) where

import           CPkgPrelude
import           Package.C.Error
import           Package.C.Triple.Type
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Parser = Parsec Void String

parseTripleIO :: MonadIO m => String -> m TargetTriple
parseTripleIO = parseIO parseTriple

parseIO :: MonadIO m => Parser a -> String -> m a
parseIO p str =
    case parse p "(none)" str of
        Right x  -> pure x
        Left err -> parseErr (errorBundlePretty err)

parseTriple :: Parser TargetTriple
parseTriple = TargetTriple
    <$> parseArch
    <*> optional (try ((between .$ char '-') parseManufacturer))
    <*> (char '-' *> parseOS)
    <*> optional (char '-' *> parseABI)

tryString :: String -> Parser String
tryString = try . string

parseArch :: Parser Arch
parseArch =
        (tryString "x86_64" $> X64)
    <|> (tryString "armv7l" $> Arm)
    <|> (tryString "arm" $> Arm)
    <|> (tryString "aarch64" $> AArch)
    <|> (tryString "riscv64" $> RISCV64)
    <|> (tryString "powerpc64le" $> PowerPC64le)
    <|> (tryString "powerpc64" $> PowerPC64)
    <|> (tryString "powerpc" $> PowerPC)
    <|> (tryString "sparc64" $> Sparc64)
    <|> (tryString "s390x" $> S390x)
    <|> (tryString "alpha" $> Alpha)
    <|> (tryString "m68k" $> M68k)
    <|> (tryString "mipsel" $> MipsEl)
    <|> (tryString "mips" $> Mips)
    <|> (tryString "mips64el" $> Mips64El)
    <|> (tryString "mips64" $> Mips64)
    <|> (tryString "i686" $> X86)
    <|> (tryString "sh4" $> SH4)
    <|> (tryString "hppa64" $> HPPA64)
    <|> (tryString "hppa" $> HPPA)

parseManufacturer :: Parser Manufacturer
parseManufacturer =
        (tryString "unknown" $> Unknown)
    <|> (tryString "apple" $> Apple)
    <|> (tryString "ibm" $> IBM)
    <|> (tryString "pc" $> PC)

parseOS :: Parser OS
parseOS =
        (tryString "darwin" $> Darwin)
    <|> (tryString "dragonfly" $> Dragonfly)
    <|> (tryString "freebsd" $> FreeBSD)
    <|> (tryString "linux" $> Linux)
    <|> (tryString "openbsd" $> OpenBSD)
    <|> (tryString "netbsd" $> NetBSD)
    <|> (tryString "solaris" $> Solaris)
    <|> (tryString "w64" $> Windows)
    <|> (tryString "redox" $> Redox)
    <|> (tryString "haiku" $> Haiku)
    <|> (tryString "ios" $> IOS)
    <|> (tryString "aix" $> AIX)
    <|> (tryString "hurd" $> Hurd)
    <|> (tryString "android" $> Android)
    <|> (tryString "none" $> NoOs)

parseABI :: Parser ABI
parseABI =
        (tryString "gnueabihf" $> GNUeabihf)
    <|> (tryString "gnueabi" $> GNUeabi)
    <|> (tryString "gnuabi64" $> GNUabi64)
    <|> (tryString "gnuspe" $> GNUspe)
    <|> (tryString "gnu" $> GNU)
    <|> (tryString "mingw32" $> MinGw)
