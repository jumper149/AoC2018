module Main

import System.File.ReadWrite
import Text.Lexer
import Text.Lexer.Tokenizer
import Text.Parser
import Text.Parser.Core

namespace Input

  tokenizer : Tokenizer ?tokenType1
  
  grammar : Grammar () ?tokenType2 True ?inputType1

  export
  input : IO ?inputType2
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

main : IO ()
main = do
  --input <- Input.input
  --printLn input

  pure ()
