module Main

import Control.Monad.State
import Data.List
import Data.List1
import Data.Maybe
import Data.Stream
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
  
  grammar : Grammar () Token True (List1 Integer)
  grammar = someTill eof grammarLine

  export
  input : IO (List1 Integer)
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

findDuplicateSumStep : HasIO m => MonadState (List1 Integer) m => Integer -> m (Maybe Integer)
findDuplicateSumStep x = do
  oldList <- get
  let lastSum = head oldList
  let newSum = lastSum + x
  liftIO $ putStrLn $ show newSum
  if newSum `elem` forget oldList
     then pure $ Just newSum
     else do
       put $ newSum `cons` oldList
       pure $ Nothing

findDuplicateSum : HasIO m => MonadState (List1 Integer) m => Stream Integer -> m ()
findDuplicateSum xs = do
  let x = head xs
  result <- findDuplicateSumStep x
  case result of
       Nothing => findDuplicateSum $ tail xs
       (Just duplicateSum) => pure ()

main : IO ()
main = do
  frequencyChanges <- Input.input
  --putStrLn $ show frequencyChanges
  --putStrLn $ show $ sum frequencyChanges
  case frequencyChanges of
       (x ::: xs) => do
         let stateComputation : StateT (List1 Integer) IO ()
             stateComputation = findDuplicateSum $ cycle $ x :: xs
         evalStateT (0 ::: []) stateComputation
  pure ()
