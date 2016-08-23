import           Data.Maybe
import           System.Environment
import qualified YesodServantExample

main :: IO ()
main = do
    p <- fromMaybe 3000 . (fmap read) <$> lookupEnv "PORT"
    YesodServantExample.run p
