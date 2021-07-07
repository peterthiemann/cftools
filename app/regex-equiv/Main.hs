import qualified Data.Text as T
import           System.Environment (getArgs)

import           Equivalence
import           Regexp.Parser

main :: IO ()
main = do
    [sigma, in1, in2] <- getArgs
    either handleParseError id $ do
      r1 <- parseRegexp sigma (T.pack in1)
      r2 <- parseRegexp sigma (T.pack in2)
      return . print $ equiv sigma r1 r2
  where
    handleParseError err = putStrLn $ "Parse error: " ++ err
