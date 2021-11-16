module Main

import Control.Monad.State
import Data.List
import System.File.ReadWrite
import Text.Lexer
import Text.Lexer.Tokenizer
import Text.Parser
import Text.Parser.Core

namespace Input
  data Token = TokenNewline
             | TokenChars (List Char)

  tokenizer : Tokenizer Token
  tokenizer = match newline (const TokenNewline)
          <|> match alphas (TokenChars . unpack)

  public export
  InputType : Type
  InputType = List1 $ List Char

  grammarNewline : Grammar () Token True ()
  grammarNewline = terminal "Newline" $ \ x =>
                   case x of
                        TokenNewline => Just ()
                        TokenChars xs => Nothing

  grammarChars : Grammar () Token True (List Char)
  grammarChars = terminal "Chars" $ \ x =>
                 case x of
                      TokenNewline => Nothing
                      TokenChars xs => Just xs

  grammar : Grammar () Token True InputType
  grammar = someTill eof (grammarChars <* grammarNewline)

  export
  input : IO InputType
  input = do
    readResult <- readFile "./input"
    inputString <- case readResult of
                Left err => do
                  printLn err
                  ?handleFileError
                Right x => pure x
    tokens <- case lex tokenizer inputString of
                   (x, (EndInput, _)) => pure x
                   (_, stopReason) => do
                     printLn stopReason
                     ?handleLexError
    case parse grammar tokens of
         Left err => do
           --printLn err
           ?handleParseError
         Right (result, rest) => do
           --printLn rest
           pure result

||| Expects a sorted list.
equals : Eq a => List a ->
         List a ->
         List (List a)
equals [] ys = [ys]
equals (x :: xs) [] = equals xs [x]
equals (x :: xs) (y :: ys) = if x == y
                                then equals xs (x :: y :: ys)
                                else (y :: ys) :: equals xs [x]

||| Expects a sorted list.
countEquals : Eq a => List a ->
              List Nat
countEquals xs = length <$> equals xs []

part1 : Input.InputType -> IO Nat
part1 input = do
  let counts = countEquals . sort <$> forget input
  let containsDoubles = filter (2 `elem`) counts
  let containsTriples = filter (3 `elem`) counts
  let result = length containsDoubles * length containsTriples
  printLn counts
  pure result

main : IO ()
main = do
  input <- Input.input
  --printLn input

  printLn =<< part1 input

  pure ()
