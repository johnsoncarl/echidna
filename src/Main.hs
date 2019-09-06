module Main where

import Control.Monad.Reader (runReaderT)
import Control.Monad.Random (getRandom)
import Data.Text (pack)
import Options.Applicative
import System.Exit (exitWith, exitSuccess, ExitCode(..))

import Echidna.ABI
import Echidna.Config
import Echidna.Solidity
import Echidna.Campaign
import Echidna.UI

data Options = Options
  { filePath         :: FilePath
  , filePath2        :: FilePath
  , selectedContract :: Maybe String
  , configFilepath   :: Maybe FilePath
  }

 -- * This is a input Parser of type Options
options :: Parser Options
options = Options <$> argument str (metavar "FILE"
                        <> help "Solidity file to analyze")
                  <*> argument str (metavar "FILE"
                        <> help "Solidity file2 to analyze")
                  <*> optional (argument str $ metavar "CONTRACT"
                        <> help "Contract to analyze")
                  <*> optional (option str $ long "config"
                        <> help "Config file")

-- * Setting the Header and Program Description. Can be viewed with echidna-test --help
opts :: ParserInfo Options
opts = info (options <**> helper) $ fullDesc
  <> progDesc "EVM property-based testing framework"
  <> header "Echidna"

main :: IO ()

main = do Options f f2 c conf <- execParser opts
          g   <- getRandom
          cfg <- maybe (pure defaultConfig) parseConfig conf
          cpg <- flip runReaderT cfg $ do
                                          cs       <- contracts f
                                          (v,w,ts) <- loadSpecified (pack . (f ++) . (':' :) <$> c) cs >>= prepareForTest
                                          ui v w ts (Just $ mkGenDict 0.15 (extractConstants cs) [] g)
          cpg2 <- flip runReaderT cfg $ do
                                          cs       <- contracts f2
                                          (v,w,ts) <- loadSpecified (pack . (f2 ++) . (':' :) <$> c) cs >>= prepareForTest
                                          ui v w ts (Just $ mkGenDict 0.10 (extractConstants cs) [] g)
          if not . isSuccess $ cpg then exitWith $ ExitFailure 1 else (if not . isSuccess $ cpg then exitWith $ ExitFailure 1 else exitSuccess) 
