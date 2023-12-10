{-# LANGUAGE OverloadedStrings #-}
-- | (Unused) Parses & evaluates calc() expressions.
-- Implemented using The Shunting Yard Algorithm.
module Graphics.Layout.Arithmetic(Opcode(..), parseCalc, verifyCalc,
        evalCalc, mapCalc) where

import Data.CSS.Syntax.Tokens (Token(..), NumericValue(..))
import Data.Scientific (toRealFloat)
import GHC.Real (infinity)
import Data.Text (unpack, Text)
import qualified Data.Text as Txt
import Debug.Trace (trace) -- For error reporting.

-- | Parsed calc() expression. As a postfix arithmatic expression.
data Opcode n = Seq | Add | Subtract | Multiply | Divide | Func Text | Num n deriving Show
-- | Parse a calc() expression.
parseCalc :: [Token] -> [Opcode (Float, String)] -> [Opcode (Float, String)]
parseCalc (Number _ n:toks) stack = Num (val2float n, ""):parseCalc toks stack
parseCalc (Percentage _ n:toks) stack = Num (val2float n, "%"):parseCalc toks stack
parseCalc (Dimension _ n unit:toks) stack =
    Num (val2float n, unpack unit):parseCalc toks stack
parseCalc (Ident "e":toks) stack = Num (exp 1, ""):parseCalc toks stack
parseCalc (Ident "pi":toks) stack = Num (pi, ""):parseCalc toks stack
parseCalc (Ident "infinity":toks) stack = Num (f infinity, ""):parseCalc toks stack
parseCalc (Ident "-infinity":toks) stack =
    Num (negate $ f infinity, ""):parseCalc toks stack
parseCalc (Ident "NaN":toks) stack = Num (0/0, ""):parseCalc toks stack

parseCalc (Function x:toks) stack = parseCalc toks (Func x:stack)
parseCalc (LeftParen:toks) stack = parseCalc toks (Func "calc":stack)
parseCalc toks'@(Delim c:toks) (stack:stacks)
    | prec stack >= prec (op c) = stack:parseCalc toks' stacks
    | otherwise = parseCalc toks (op c:stack:stacks)
  where
    prec :: Opcode n -> Int
    prec Seq = 1
    prec Add = 2
    prec Subtract = 2
    prec Multiply = 3
    prec Divide = 3
    prec (Func _) = 0
    prec (Num _) = error "Unexpected number on operand stack!"
parseCalc (Delim c:toks) [] = parseCalc toks [op c]
parseCalc (Comma:toks) stack = parseCalc (Delim ',':toks) stack
parseCalc (RightParen:toks) (Func "calc":stack) = parseCalc toks stack
parseCalc (RightParen:toks) (op'@(Func _):stack) = op':parseCalc toks stack
parseCalc toks@(RightParen:_) (op':stack) = op':parseCalc toks stack
parseCalc (RightParen:toks) [] = parseCalc toks []
parseCalc [] [] = []
parseCalc [] stack = parseCalc [RightParen] stack
parseCalc _ _ = [Func "invalid"]

-- | Parse an operator char.
op :: Char -> Opcode n
op '+' = Add
op '-' = Subtract
op '*' = Multiply
op '/' = Divide
op ',' = Seq -- For function-calls.
op _ = Func "invalid"

-- Do operands counts line up? Are we dividing by 0?
-- Also I see concerns about whether units line up. Not bothering verifying that.
-- | Verify that a parsed math expression can be properly evaluated.
verifyCalc :: [Opcode (Float, String)] -> [Bool] -> Bool
verifyCalc (Seq:expr) stack = verifyCalc expr stack
verifyCalc (Add:expr) (_:_:stack) = verifyCalc expr (True:stack)
verifyCalc (Subtract:expr) (_:_:stack) = verifyCalc expr (True:stack)
verifyCalc (Multiply:expr) (_:_:stack) = verifyCalc expr (True:stack)
verifyCalc (Divide:_) (False:_) = False
verifyCalc (Divide:expr) (_:_:stack) = verifyCalc expr (True:stack)
verifyCalc (Num (n, _):expr) stack = verifyCalc expr ((n == 0):stack)
verifyCalc (Func x:expr) (_:stack)
    | x `elem` Txt.words "abs acos asin atan cos exp log sign sin sqrt tan" =
        verifyCalc expr (True:stack)
verifyCalc (Func x:expr) (_:_:stack)
    | x `elem` Txt.words "atan2 max min mod pow rem" = verifyCalc expr (True:stack)
verifyCalc (Func "clamp":expr) (_:_:_:stack) = verifyCalc expr (True:stack)
verifyCalc [] [_] = True
verifyCalc _ _ = False

-- | Evaluate a parsed calc() expression.
evalCalc :: [Opcode Float] -> [Float] -> Float
evalCalc (Seq:expr) stack = evalCalc expr stack -- The function args off
evalCalc (Add:expr) (y:x:stack) = evalCalc expr ((x + y):stack)
evalCalc (Subtract:expr) (y:x:stack) = evalCalc expr ((x - y):stack)
evalCalc (Multiply:expr) (y:x:stack) = evalCalc expr ((x*y):stack)
evalCalc (Divide:expr) (y:x:stack) = evalCalc expr ((x/y):stack)
evalCalc (Num n:expr) stack = evalCalc expr (n:stack)

evalCalc (Func "abs":expr) (x:stack) = evalCalc expr (abs x:stack)
evalCalc (Func "acos":expr) (x:stack) = evalCalc expr (acos x:stack)
evalCalc (Func "asin":expr) (x:stack) = evalCalc expr (asin x:stack)
evalCalc (Func "atan":expr) (x:stack) = evalCalc expr (atan x:stack)
evalCalc (Func "atan2":expr) (y:x:stack) = evalCalc expr (atan2 x y:stack)
evalCalc (Func "clamp":expr) (high:x:low:stack) =
    evalCalc expr (min high (max low x):stack)
evalCalc (Func "cos":expr) (x:stack) = evalCalc expr (cos x:stack)
evalCalc (Func "exp":expr) (x:stack) = evalCalc expr (exp x:stack)
evalCalc (Func "log":expr) (x:stack) = evalCalc expr (log x:stack)
evalCalc (Func "max":expr) (y:x:stack) = evalCalc expr (max x y:stack)
evalCalc (Func "min":expr) (y:x:stack) = evalCalc expr (min x y:stack)
evalCalc (Func "mod":expr) (y:x:stack) =
    evalCalc expr (toEnum (round x `mod` round y):stack)
evalCalc (Func "pow":expr) (y:x:stack) = evalCalc expr (x ** y:stack)
evalCalc (Func "rem":expr) (y:x:stack) =
    evalCalc expr (toEnum (round x `rem` round y):stack)
evalCalc (Func "sign":expr) (x:stack) = evalCalc expr (signum x:stack)
evalCalc (Func "sin":expr) (x:stack) = evalCalc expr (sin x:stack)
evalCalc (Func "sqrt":expr) (x:stack) = evalCalc expr (sqrt x:stack)
evalCalc (Func "tan":expr) (x:stack) = evalCalc expr (tan x:stack)

evalCalc [] [ret] = ret
evalCalc [] stack@(ret:_) =
    trace ("Verification should have caught this error! " ++ show stack) ret
evalCalc [] [] = trace "Verification should have caught this error! Stack underflow!" 0
evalCalc (op:_) (ret:_) =
    trace ("Verification should have caught this error! Unsupported op " ++ show op) ret
evalCalc (op:_) [] =
    trace ("Verification should have caught this error! Unsupported op " ++ show op) 0

-- | Convert all numbers in an expression via the given callback.
mapCalc :: (a -> b) -> [Opcode a] -> [Opcode b]
mapCalc cb (Num x:toks) = Num (cb x):mapCalc cb toks
-- GHC demanded more verbosity...
mapCalc cb (Seq:toks) = mapCalc cb toks -- we can drop these while we're at it...
mapCalc cb (Add:toks) = Add:mapCalc cb toks
mapCalc cb (Subtract:toks) = Subtract:mapCalc cb toks
mapCalc cb (Multiply:toks) = Multiply:mapCalc cb toks
mapCalc cb (Divide:toks) = Divide:mapCalc cb toks
mapCalc cb (Func f':toks) = Func f':mapCalc cb toks
mapCalc _ [] = []

-- | Convert from a tokenized NumericValue to a Float.
val2float :: NumericValue -> Float
val2float (NVInteger n) = fromIntegral n
val2float (NVNumber n) = toRealFloat n

-- | Convert from a rational value to a float.
f :: Rational -> Float
f = fromRational
