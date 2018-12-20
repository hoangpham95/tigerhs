import TigerLexer
import System.IO
import Control.Monad

main = do
  contents <- readFile "samples/test1.tig"
  let toks = runAlex contents
  mapM_ (\s -> print(fst s)) toks