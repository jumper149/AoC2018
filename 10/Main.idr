module Main

import Control.Monad.State
import Data.String
import Data.Vect
import System.File.ReadWrite
import Text.Lexer
import Text.Lexer.Tokenizer
import Text.Parser
import Text.Parser.Core

namespace Input
  data Token = MkTokenNewline
             | MkTokenWhitespace
             | MkTokenEqualSign
             | MkTokenMinus
             | MkTokenComma
             | MkTokenBracketLeft
             | MkTokenBracketRight
             | MkTokenLabel String
             | MkTokenInteger Integer

  tokenizer : Tokenizer Token
  tokenizer = match newline (const MkTokenNewline)
          <|> match spaces (const MkTokenWhitespace)
          <|> match (is '=') (const MkTokenEqualSign)
          <|> match (is '-') (const MkTokenMinus)
          <|> match (is ',') (const MkTokenComma)
          <|> match (is '<') (const MkTokenBracketLeft)
          <|> match (is '>') (const MkTokenBracketRight)
          <|> match alphas MkTokenLabel
          <|> match digits (MkTokenInteger . cast)

  public export
  InputType : Type
  InputType = List ((Integer, Integer), (Integer, Integer))

  grammarNewline : Grammar () Token True ()
  grammarNewline = terminal "Newline" $ \ x =>
                   case x of
                        MkTokenNewline => Just ()
                        _ => Nothing

  grammarWhitespace : Grammar () Token True ()
  grammarWhitespace = terminal "Whitespace" $ \ x =>
                      case x of
                           MkTokenWhitespace => Just ()
                           _ => Nothing

  grammarLabel : String -> Grammar () Token True ()
  grammarLabel label = terminal "Label" $ \ x =>
                         case x of
                              MkTokenLabel x => if x == label
                                                   then Just ()
                                                   else Nothing
                              _ => Nothing

  grammarEqualSign : Grammar () Token True ()
  grammarEqualSign = terminal "EqualSign" $ \ x =>
                     case x of
                          MkTokenEqualSign => Just ()
                          _ => Nothing

  grammarComma : Grammar () Token True ()
  grammarComma = terminal "Comma" $ \ x =>
                     case x of
                          MkTokenComma => Just ()
                          _ => Nothing

  grammarBracketLeft : Grammar () Token True ()
  grammarBracketLeft = terminal "BracketLeft" $ \ x =>
                       case x of
                            MkTokenBracketLeft => Just ()
                            _ => Nothing

  grammarBracketRight : Grammar () Token True ()
  grammarBracketRight = terminal "BracketRight" $ \ x =>
                        case x of
                             MkTokenBracketRight => Just ()
                             _ => Nothing

  grammarMinus : Grammar () Token True (Integer -> Integer)
  grammarMinus = terminal "BracketRight" $ \ x =>
                 case x of
                      MkTokenMinus => Just negate
                      _ => Nothing

  grammarInteger : Grammar () Token True Integer
  grammarInteger = terminal "BracketRight" $ \ x =>
                   case x of
                        MkTokenInteger y => Just y
                        _ => Nothing

  grammarPair : Grammar () Token True (Integer, Integer)
  grammarPair = grammarBracketLeft
             *> optional grammarWhitespace
             *> pure (,)
             <*> (option id grammarMinus <*> grammarInteger)
             <* grammarComma
             <* optional grammarWhitespace
             <*> (option id grammarMinus <*> grammarInteger)
             <* grammarBracketRight

  grammarPosition : Grammar () Token True (Integer, Integer)
  grammarPosition = grammarLabel "position" *> grammarEqualSign *> grammarPair

  grammarVelocity : Grammar () Token True (Integer, Integer)
  grammarVelocity = grammarLabel "velocity" *> grammarEqualSign *> grammarPair
  
  grammarLine : Grammar () Token True ((Integer, Integer), (Integer, Integer))
  grammarLine = (,) <$> (grammarPosition <* grammarWhitespace) <*> grammarVelocity <* grammarNewline

  grammar : Grammar () Token False InputType
  grammar = manyTill eof grammarLine

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

record Position where
  constructor MkPosition
  x, y : Integer

record Velocity where
  constructor MkVelocity
  x, y : Integer

record Point where
  constructor MkPoint
  position : Position
  velocity : Velocity

toPoints : InputType -> List Point
toPoints = map $ \ (pos, vel) => MkPoint (uncurry MkPosition pos) (uncurry MkVelocity vel)

Resolution : Nat
Resolution = 100

data Sky = MkSky (Vect Resolution $ Vect Resolution Bool)

initialSky : Sky
initialSky = MkSky $ replicate Resolution $ replicate Resolution False

drawPoint : Point -> Sky -> Sky
drawPoint p (MkSky ys) = fromMaybe (MkSky ys) $ do
  xPos <- integerToFin (p.position.x + (natToInteger Resolution `div` 2)) Resolution
  yPos <- integerToFin (p.position.y + (natToInteger Resolution `div` 2)) Resolution
  pure $ MkSky $ updateAt yPos (replaceAt xPos True) ys

renderSky : Sky -> String
renderSky (MkSky ys) = pack $ unlines' $ toList $ map (toList . map renderPoint) ys
  where
    renderPoint : Bool -> Char
    renderPoint False = ' '
    renderPoint True = 'X'

movePoint : Point -> Point
movePoint p = record { position = MkPosition (p.position.x + p.velocity.x) (p.position.y + p.velocity.y) } p

watchSky : StateT (List Point) IO String
watchSky = do
  points <- get
  let sky = foldr drawPoint initialSky points
  let newPoints = movePoint <$> points
  put newPoints
  let str = renderSky sky
  pure str

main : IO ()
main = do
  input <- Input.input
  printLn input
  let points = toPoints input

  evalStateT points watchSky

  pure ()
