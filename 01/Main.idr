module Main

import System.File.ReadWrite
import Text.Bounded
import Text.Lexer
import Text.Lexer.Tokenizer
import Text.Parser
import Text.Parser.Core

namespace Input
  data Token = TokenNewline
             | TokenPlus
             | TokenMinus
             | TokenNatural Nat

  tokenizer : Tokenizer Token
  tokenizer = match newline (const TokenNewline)
          <|> match (is '+') (const TokenPlus)
          <|> match (is '-') (const TokenMinus)
          <|> match digits (TokenNatural . cast)

  grammarSign : Grammar () Token True (Nat -> Integer)
  grammarSign = terminal "Sign" $ \ x =>
                case x of
                     TokenPlus => Just natToInteger
                     TokenMinus => Just $ negate . natToInteger
                     _ => Nothing

  grammarNatural : Grammar () Token True Nat
  grammarNatural = terminal "Natural" $ \ x =>
                   case x of
                        TokenNatural x => Just x
                        _ => Nothing

  grammarNewline : Grammar () Token True ()
  grammarNewline = terminal "Newline" $ \ x =>
                   case x of
                        TokenNewline => Just ()
                        _ => Nothing

  grammarLine : Grammar () Token True Integer
  grammarLine = grammarSign <*> grammarNatural <* grammarNewline
  
  grammar : Grammar () Token False (List Integer)
  grammar = manyTill eof grammarLine

  export
  input : IO (List Integer)
  input = do
    readResult <- readFile "./input"
    inputString <- case readResult of
                (Left err) => do
                  putStrLn $ show err
                  ?handleFileError
                (Right x) => pure x
    tokens <- case lex tokenizer inputString of
                   (x, (EndInput, _)) => pure x
                   (_, stopReason) => do
                     putStrLn $ show stopReason
                     ?handleLexError
    case parse grammar tokens of
         (Left err) => do
           --putStrLn $ show err
           ?handleParseError
         (Right (result, rest)) => do
           --putStrLn $ show rest
           pure result

main : IO ()
main = do
  frequencyChanges <- Input.input
  --putStrLn $ show frequencyChanges
  putStrLn $ show $ sum frequencyChanges
  pure ()
