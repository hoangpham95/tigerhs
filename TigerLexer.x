{
module TigerLexer ( getTok, runAlex, alexMonadScan, Token(..), LexResult(..) ) where

import Prelude hiding ( GT, LT, EQ )
import Data.Maybe
import Control.Monad
import Numeric ( readDec )
}

%wrapper "monadUserState"

$digit = 0-9			      -- digits
$alpha = [a-zA-Z]		    -- alphabetic characters
$white = [\ \t\b]  -- white space

@id = $alpha($alpha | _ | $digit)*
@number = $digit+

state:-

<0> "type" { mkTok TYPE }
<0> "var" { mkTok VAR }
<0> "function" { mkTok FUNCTION }
<0> "break" { mkTok BREAK }
<0> "of" { mkTok OF }
<0> "end" { mkTok END }
<0> "in" { mkTok IN }
<0> "nil" { mkTok NIL }
<0> "let" { mkTok LET }
<0> "do" { mkTok DO }
<0> "to" { mkTok TO }
<0> "for" { mkTok FOR }
<0> "while" { mkTok WHILE }
<0> "else" { mkTok ELSE }
<0> "then" { mkTok THEN }
<0> "if" { mkTok IF }
<0> "array" { mkTok ARRAY }
<0> \n ;
<0> $white+ { skip }

<0> :\= { mkTok ASSIGN }
<0> \| { mkTok OR }
<0> & { mkTok AND }
<0> > { mkTok GT }
<0> \>\= { mkTok GE }
<0> \< { mkTok LT }
<0> \<\= { mkTok LE }
<0> \= { mkTok EQ }
<0> \<\> { mkTok NE }
<0> \+ { mkTok PLUS }
<0> \- { mkTok MINUS }
<0> \* { mkTok TIMES }
<0> \/ { mkTok DIVIDE }
<0> \. { mkTok DOT }
<0> \[ { mkTok LBRACK }
<0> \] { mkTok RBRACK }
<0> \( { mkTok LPAREN }
<0> \) { mkTok RPAREN }
<0> \; { mkTok SEMICOLON }
<0> : { mkTok COLON }
<0> "," { mkTok COMMA }
<0> "/*" { enterNewComment `andBegin` comment }
<comment> "/*" { embedComment }
<comment> "*/" { unembedComment }
<comment> . ;
<comment> "\n" { skip }
<0> \" { enterStringState `andBegin` string }
<string> \\n { addToString '\n' }
<string> \\t { addToString '\t' }

<0> @id { mkId }
<0> @number { mkNumber } 

{
-- The token type:
data Token =
    EOF
  | INT     Int
  | ID      String
  | STRING  String
  | TYPE
  | VAR
  | FUNCTION
  | BREAK
  | OF
  | END
  | IN
  | NIL
  | LET
  | DO
  | TO
  | FOR
  | WHILE
  | ELSE
  | THEN
  | IF
  | ARRAY
  | ASSIGN
  | OR
  | AND
  | GE
  | LE
  | GT
  | LT
  | NE
  | EQ
  | PLUS
  | MINUS
  | DIVIDE
  | TIMES
  | DOT
  | LBRACE
  | RBRACE
  | LBRACK
  | RBRACK
  | LPAREN
  | RPAREN
  | SEMICOLON
  | COLON
  | COMMA
  deriving (Eq, Show)

-- Wrapper from monad

-- definition of EOF needed by alex
alexEOF :: Alex LexResult
alexEOF = return (LexResult undefined EOF Nothing)

-- Action

data LexResult = LexResult AlexPosn Token (Maybe String) deriving (Show)
type Action = AlexInput -> Int -> Alex LexResult

mkTok :: Token -> Action
mkTok c (p, _, _, input) len = 
  return (LexResult p c (Just s)) where 
    s = take len input

mkId (p, _, _, input) len = return (LexResult p (ID s) (Just s)) where
  s = (take len input)

mkNumber (p, _, _, input) len = 
  if (length r) == 1 then 
    return (LexResult p (INT (fst (head r))) (Just s))
  else alexError ("Lexer error: Cannot parse number")
     
  where 
    s = take len input
    r = readDec s

enterNewComment, embedComment, unembedComment, enterStringState :: Action
enterNewComment input len =
  do 
    setCommentDepth 1
    skip input len

embedComment input len =
  do 
    cd <- getCommentDepth
    setCommentDepth (cd + 1)
    skip input len

unembedComment input len =
  do
    cd <- getCommentDepth
    setCommentDepth (cd - 1)
    when (cd == 1) (alexSetStartCode 0)
    skip input len

enterStringState input len =
  do 
    setStringState True
    skip input len

-- newtype Alex a = Alex { unAlex :: AlexState -> Either String (AlexState, a) }

data AlexUserState = AlexUserState {
  commentDepth :: Int,
  stringVal  :: String,
  inStringState :: Bool
}

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState {
  commentDepth = 0,
  stringVal = "",
  inStringState = False
}

getCommentDepth :: Alex Int
getCommentDepth =  Alex $ \s@AlexState{alex_ust=ust} -> 
  Right (s, commentDepth ust) 

setCommentDepth :: Int -> Alex ()
setCommentDepth cd =  Alex $ \s@AlexState{alex_ust=ust} -> 
  Right (s{alex_ust=ust{commentDepth=cd}}, ())

setStringState :: Bool -> Alex ()
setStringState st = Alex $ \s@AlexState{alex_ust=ust} ->
  Right (s{alex_ust=ust{inStringState=st}}, ())

getStringVal :: Alex String
getStringVal = Alex $ \s@AlexState{alex_ust=ust} ->
  Right(s, stringVal ust)

addToLexerString :: Char -> Alex ()
addToLexerString c = Alex $ \s@AlexState{alex_ust=ust} -> 
  Right(s{alex_ust=ust{stringVal=c:(stringVal ust)}}, ())

addToString :: Char -> Action
addToString c _ _ =
  do 
    addToLexerString c
    alexMonadScan

getTok :: String -> Either String [LexResult]
getTok input = let loop = do tok <- alexMonadScan
                             let t@(LexResult _ c _) = tok
                             if (c == EOF)
                               then return [t]
                             else do
                               toks <- loop
                               return (tok : toks)
                          in runAlex input loop
}