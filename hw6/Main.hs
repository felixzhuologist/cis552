{-# OPTIONS_GHC -fwarn-tabs -fwarn-incomplete-patterns -fdefer-type-errors #-}
module Main where

import Data.Map (Map)
import qualified Data.Map as Map

import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as PP

import Control.Applicative (Alternative(..))
import Control.Monad ()

import State (State)
import qualified State as S

import qualified Parser as P
import qualified ParserCombinators as P

main :: IO ()
main = return ()

-------------------------------------------------------------------------

type Variable = String

newtype Block =
    Block [ Statement ]                 -- { s1; ... sn; }
  deriving (Eq, Show)

data Statement =
    Assign Variable Expression          -- x = e
  | If Expression Block Block           -- if e then s1 else s2
  | While Expression Block              -- while e do s
  deriving (Eq, Show)



data Expression =
    Var Variable                        -- x
  | Val Value                           -- v
  | Op Expression Bop Expression
  deriving (Eq, Show)

data Bop =
    Plus     -- +  :: Int -> Int -> Int
  | Minus    -- -  :: Int -> Int -> Int
  | Times    -- *  :: Int -> Int -> Int
  | Divide   -- /  :: Int -> Int -> Int
  | Gt       -- >  :: Int -> Int -> Bool
  | Ge       -- >= :: Int -> Int -> Bool
  | Lt       -- <  :: Int -> Int -> Bool
  | Le       -- <= :: Int -> Int -> Bool
  deriving (Eq, Show, Enum)

data Value =
    IntVal Int
  | BoolVal Bool
  deriving (Eq, Show)

type Store = Map Variable Value

-------------------------------------------------------------------------
-- Here are some test programs. You can ignore the 80-column limit for this part
-- of the file.

-- test.imp
wTest :: Block
wTest = Block [Assign "x" (Op (Op (Op (Val (IntVal 1)) Plus (Val (IntVal 2))) Minus (Val (IntVal 3))) Plus (Op (Val (IntVal 1)) Plus (Val (IntVal 3)))),
              Assign "y" (Val (IntVal 0)),
              While (Op (Var "x") Gt (Val (IntVal 0)))
                (Block [Assign "y" (Op (Var "y") Plus (Var "x")),
                        Assign "x" (Op (Var "x") Minus (Val (IntVal 1)))])]

-- fact.imp
wFact :: Block
wFact =  Block [ Assign "n" (Val (IntVal 5)),
                Assign "f" (Val (IntVal 1)),
                While (Op (Var "n") Gt (Val (IntVal 0)))
                 (Block [Assign "x" (Var "n"),
                         Assign "z" (Var "f"),
                         While (Op (Var "x") Gt (Val (IntVal 1))) (Block [Assign "f" (Op (Var "z") Plus (Var "f")),
                                                                          Assign "x" (Op (Var "x") Minus (Val (IntVal 1)))]),
                         Assign "n" (Op (Var "n") Minus (Val (IntVal 1)))]) ]

-- abs.imp
wAbs :: Block
wAbs = Block [Assign "x" (Op (Val (IntVal 0)) Minus (Val (IntVal 3))),
             If (Op (Var "x") Lt (Val (IntVal 0)))
                (Block [Assign "x" (Op (Val (IntVal 0)) Minus (Var "x"))]) (Block [])]

-- times.imp
wTimes :: Block
wTimes = Block [Assign "x" (Val (IntVal 10)),
                Assign "y" (Val (IntVal 3)),
                Assign "z" (Val (IntVal 0)),
                While (Op (Var "x") Gt (Val (IntVal 0))) (Block [Assign "z" (Op (Var "z") Plus (Var "y")),
                                                                 Assign "x" (Op (Var "x") Minus (Val (IntVal 1)))])]

-------------------------------------------------------------------------

evalE :: Expression -> State Store Value

evalE (Var varname) = fmap getVar S.get where
  getVar store = case Map.lookup varname store of
                  (Just val) -> val
                  Nothing -> IntVal 0
evalE (Val v)    = return v
evalE (Op e1 bop e2) = do
  e1' <- evalE e1
  e2' <- evalE e2
  return $ applyBop bop e1' e2'

applyBop :: Bop -> Value -> Value -> Value
applyBop Plus (IntVal v1) (IntVal v2) = IntVal $ v1 + v2
applyBop Minus (IntVal v1) (IntVal v2) = IntVal $ v1 - v2
applyBop Times (IntVal v1) (IntVal v2) = IntVal $ v1 * v2
applyBop Divide (IntVal v1) (IntVal v2) = IntVal $ v1 `quot` v2
applyBop Gt (IntVal v1) (IntVal v2) = BoolVal $ v1 > v2
applyBop Ge (IntVal v1) (IntVal v2) = BoolVal $ v1 >= v2
applyBop Lt (IntVal v1) (IntVal v2) = BoolVal $ v1 < v2
applyBop Le (IntVal v1) (IntVal v2) = BoolVal $ v1 <= v2
applyBop _ _ _ = IntVal 0

evalS :: Statement -> State Store ()
evalS loop@(While cond (Block stmts)) = evalS (If cond loopOnce (Block [])) where
  loopOnce = Block (stmts ++ [loop]) -- eval body once + the loop again
evalS (Assign varname exp) = do
  val <- evalE exp
  store <- S.get
  S.put (Map.insert varname val store)
evalS (If cond iftrue iffalse) = do
  condVal <- evalE cond
  case condVal of
    (BoolVal True) -> eval iftrue
    (BoolVal False) -> eval iffalse
    _ -> return ()

eval :: Block -> State Store ()
eval (Block []) = return ()
eval (Block (stmt:stmts)) = do
  evalS stmt
  eval $ Block stmts

exec :: Block -> Store -> Store
exec code store = S.execState (eval code) store

run :: Block -> IO ()
run block = do putStrLn "Output Store:"
               print (exec block Map.empty)

---------------------------------------------

class PP a where
  pp :: a -> Doc

instance PP Bop where
  pp Plus   = PP.char '+'
  pp Minus  = PP.char '-'
  pp Times  = PP.char '*'
  pp Divide = PP.char '/'
  pp Gt     = PP.char '>'
  pp Ge     = PP.text ">="
  pp Lt     = PP.char '<'
  pp Le     = PP.text "<="

oneLine :: PP a => a -> String
oneLine = PP.renderStyle (PP.style {PP.mode=PP.OneLineMode}) . pp

indented :: PP a => a -> String
indented = PP.render . pp

spacedBraces :: Doc -> Doc
spacedBraces d = PP.vcat [PP.lbrace, d, PP.rbrace]

instance PP Value where
  pp (IntVal a) = PP.int a
  pp (BoolVal True) = PP.text "true"
  pp (BoolVal False) = PP.text "false"

instance PP Expression where
  pp (Var v) = PP.text v
  pp (Val v) = pp v
  pp (Op e1 bop e2) = pp e1 PP.<+> pp bop PP.<+> pp e2
  -- TODO: figure out parentheses
  -- pp (Op e1@(Var _) bop e2@(Var _)) = PP.hsep [pp e1, pp bop, pp e2]
  -- pp (Op e1@(Var _) bop e2) = PP.hsep [pp e1, pp bop, PP.parens $ pp e2]
  -- pp (Op e1@(Val _) bop e2@(Val _)) = PP.hsep [pp e1, pp bop, pp e2]
  -- pp (Op e1@(Val _) bop e2) = PP.hsep [pp e1, pp bop, PP.parens $ pp e2]
  -- pp (Op e1 bop e2) = PP.hsep [PP.parens $ pp e1, pp bop, PP.parens $ pp e2]

instance PP Block where
  pp (Block stmts) = PP.vcat $ map pp stmts

instance PP Statement where
  pp (Assign var val) = PP.text var PP.<+> PP.char '=' PP.<+> pp val PP.<> PP.semi
  pp (If cond e1 e2) = PP.text "if" PP.<+>
    (PP.parens $ pp cond) PP.<+>
    (spacedBraces $ pp e1) PP.<+>
    PP.text "else" PP.<+>
    (spacedBraces $ pp e2)
  pp (While cond body) = PP.text "while" PP.<+>
    (PP.parens $ pp cond) PP.<+>
    (spacedBraces $ pp body)

-- use the C++ precendence level table
level :: Bop -> Int
level Times  = 7
level Divide = 7
level Plus   = 5
level Minus  = 5
level _      = 3    -- comparison operators

------------------------------------------------------------------------

step :: Block -> State Store Block
step (Block (stmt:stmts)) = do
  evalS stmt
  return $ Block stmts
step emptyBlock = return emptyBlock

-- | Is this block completely evaluated?
final :: Block -> Bool
final (Block []) = True
final _          = False

-- | Evaluate this block to completion
execStep :: Block -> Store -> Store
execStep block store
  | final block = store
  | otherwise = S.execState (step block) store

-- | Evaluate this block for a specified number of steps
boundedStep :: Int -> Block -> State Store Block
boundedStep 0 b = return b
boundedStep n b = do
  rest <- step b
  boundedStep (n - 1) rest


stepper :: Block -> IO ()
stepper b = go b where
  go block  = do
    putBlock block
    putStr "imp> "
    str <- getLine
    case str of
      "x" -> return ()    -- quit the stepper
      -- how to pass store from one step to the next?
      -- "n" -> stepper $ evalState (step block)
      _   -> putStrLn "?" >> go block -- unknown command
  putBlock :: Block -> IO ()
  putBlock (Block [])    = putStrLn "done"
  putBlock (Block (s:_)) = putStr   "-->"   >> putStrLn (PP.render (pp s))

------------------------------------------------------------------------

valueP :: P.Parser Value
valueP = intP <|> boolP

intP :: P.Parser Value
intP = fmap IntVal P.int

constP :: String -> a -> P.Parser a
constP s x = fmap (const x) (P.string s)

boolP :: P.Parser Value
boolP = P.choice [(constP "true" (BoolVal True)), (constP "false" (BoolVal False))]

opP :: P.Parser Bop
opP = P.choice [(constP "+" Plus),
                (constP "-" Minus),
                (constP "*" Times),
                (constP "/" Divide),
                (constP ">" Gt),
                (constP ">=" Ge),
                (constP "<" Lt),
                (constP "<=" Le)]

varP :: P.Parser Variable
varP = some P.lower

-- return a parser that runs the input parser and then skips over any whitespace
wsP :: P.Parser a -> P.Parser a
wsP p = p <* some P.space

-- todo: handle precendence, whitespace.
-- also currently parsing "1 + 2" will just parse "1" and return
exprP :: P.Parser Expression
exprP = P.choice [fmap Val valueP,
                  fmap Var varP,
                  pure Op <*> exprP <*> opP <*> exprP]

statementP :: P.Parser Statement
statementP = P.choice [pure Assign <*> varP <*> exprP,
                       pure If <*> exprP <*> toplevelP <*> toplevelP,
                       pure While <*> exprP <*> toplevelP]

toplevelP :: P.Parser Block
toplevelP = fmap Block $ some statementP

