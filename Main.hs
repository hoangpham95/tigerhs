import TigerLexer
import System.IO
import Control.Monad

main = do
  contents <- readFile "samples/test1.tig"
  let m = getTok contents
  case m of 
    Left s -> print("Lexer error: " ++ s)
    Right toks -> mapM_ (\s@(LexResult _ c _) -> print(c)) toks