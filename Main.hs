import Lexer
import System.IO
import Control.Monad

main = do
  contents <- readFile "samples/test1.tig"
  let toks = alexMonadScan contents
  map (\s -> print(fst s)) toks