{
  -- Top level: declare module name and imports

  module Lexer (
    Token(..), 
    AlexPosn(..), 
    alexScanTokens, 
    token_posn
  ) where

  import Prelude hiding ( GT, LT, EQ )
  import Data.Maybe
}

%wrapper "monadUserState"

$digit = 0-9			      -- digits
$alpha = [a-zA-Z]		    -- alphabetic characters
$white = [ \t\n\r\f\v]  -- white space

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

<0> :\= { mkTok ASSIGN }
<0> \| { mkTok OR }
<0> & { mkTok AND }
<0> > { mkTok GT }
<0> \>\= { mkTok GE }
<0> \< { mkTok LT }
<0> \<\= { mkTok LE }
<0> \= { mkTok EQ }
<0> \<\> { mkTok NEQ }
<0> \+ { mkTok PLUS }
<0> \- { mkTok MINUS }
<0> \* { mkTok TIMES }
<0> \/ { mkTok DIVIDE }
<0> \. { mkTok DOT }
<0> \{ { mkTok LBRACK }
<0> \} { mkTok RBRACK }
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
<0> \" { enterNewString `andBegin` string }
<string> \\n { addToString '\n' }
<string> \\t { addToString '\t' }

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
  deriving (Eq,Show)

-- Wrapper from monad

-- Action

data LexResult = LexResult AlexPosn Token (Maybe String)
type Action = AlexInput -> Int -> Alex LexResult

mkTok :: Token -> AlexInput -> Int -> Alex LexResult
mkTok c (p, _, _, str) len = return (LexResult p c (Just (take len str)))

enterNewComment, embedComment, unembedComment, enterNewString :: Action
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
    when (cd == 1) (alexStartCode 0)
    skip input len

enterNewString input len =
  do 
    skip input len

-- newtype Alex a = Alex { unAlex :: AlexState -> Either String (AlexState, a) }

data AlexUserState = AlexUserState {
  commentDepth :: Int,
  stringVal  :: String
}

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState {
  commentDepth = 0,
  stringVal = ""
}

getCommentDepth :: Alex Int
getCommentDepth =  Alex $ \s@AlexState{alex_ust=ust} -> 
  Right (s, commentDepth ust) 

setCommentDepth :: Int -> Alex ()
setCommentDepth ss =  Alex $ \s@AlexState{alex_ust=ust} -> 
  Right (s{alex_ust=ust{commentDepth=ss}}, ())

getStringVal :: Alex String
getStringVal = Alex $ \s@AlexState{alex_ust=ust} ->
  Right(s, stringVal ust)

addToString :: Char -> Alex ()
addToString c = Alex $ \s@AlexState{alex_ust=ust} ->
  Right (s{alex_ust=ust{stringVal=c:(stringVal ust)}}, ())

}