-- CS 456 Homework 4
-- Due date: 11/05/2021 by 6:00PM

-- Name: Eliel Vipata
-- Email: vkilembo@purdue.edu

-- ASSIGNMENT INSTRUCTIONS:
-- ========================================================================
-- All questions have to be completed. Before submitting, make sure
-- that running 'stack build; stack exec hw4-exe' in the shell
-- completes without errors.  If your homework does not compile, it
-- will not be graded! If an incomplete answer to a question is
-- causing ghc to fail, comment it out.

-- SUBMISSION INSTRUCTIONS:
-- =========================================================================
-- The completed homework (i.e. everything in the top-level directory)
-- should be submitted via Brightspace by 6PM on 11/05/2021.

-- Part 1: Monads [25pts]

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}

module Main where


import Lib

import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except
import Data.List
import Data.Maybe
import Control.Applicative
import Test.QuickCheck


-- Question 1 [1pt]: Define a function that increments a counter
-- implemented using the State monad. Do not use 'do' notation, use
-- the bind operation instead (>>=).


incCount :: MonadState Int m => m ()
incCount =  get >>= \x -> put(x+1)


-- -- -- [execState :: State s a -> s -> s] is a function that evaluates a
-- -- -- monadic value in some initial state and returns the resulting final
-- -- -- state. Here are some examples of incCount:
-- -- -- * execState incCount 0 = 1
-- -- -- * execState incCount 10 = 11
-- -- -- * execState incCount 41 = 42


-- -- Question 2 [1pt]: Define a function that checks if the value stored
-- -- in the current state is less than 42, incrementing it if so, and
-- -- throwing an error otherwise. Do not use 'do' notation, use
-- -- the bind operation instead (>>=).


safeInc :: (MonadState Int m, MonadError () m) => m ()
safeInc =  get >>= \x->if x < 42 then put(x+1) else throwError ()
-- safeInc = error "Fill In Here"

-- -- [execStateT :: Monad m => State s a -> s -> m s] is a function that
-- -- evaluates a monadic value in some initial state and returns the
-- -- resulting final state, wrapped with any remaining monadic bits.
-- -- [runExcept :: Except e a -> Either e a] is a function that lifts a
-- -- value living in the Exception Monad to a Either value.
-- -- Here are some examples of safeCount:
-- -- * runExcept (execStateT safeInc 0) = Right 1
-- -- * runExcept (execStateT safeInct 10) = Right 11
-- -- * runExcept (execStateT safeInc 42) = Left



safeIncLog :: (MonadState Int m, MonadError () m, MonadWriter String m) => m ()
safeIncLog = get >>= \x->if x < 42 then tell "Safely Updated Counter" >> safeInc else throwError ()

-- -- [runExcept :: Except e a -> Either e a] is a function that lifts a
-- -- value living in the Exception Monad to a Either value.
-- -- Here are some examples of safeCount:
-- -- * runWriter (runExceptT (execStateT safeIncLog 0)) = (Right 1,"Safely Updated Counter!")
-- -- * runWriter (runExceptT (execStateT (safeIncLog >> safeIncLog >> safeIncLog) 10)) = (Right 13, "Safely Updated Counter! Safely Updated Counter! Safely Updated Counter! ")
-- -- * runWriter (runExceptT (execStateT (safeIncLog) 42)) = (Left (),"")
-- -- * runWriter (runExceptT (execStateT (safeIncLog >> safeIncLog) 41)) = (Left (),"Safely Updated Counter!")

-- -- Question 4 [2pts]: Write variants of the above functions using the
-- -- monadic 'do' notation.

incCount' :: MonadState Int m => m ()
incCount' = do
    n <- get
    put(n+1)


safeInc' :: (MonadState Int m, MonadError () m) => m ()
safeInc' =  do
        n <- get 
        if | n < 42 -> put(n+1)
           | otherwise -> throwError ()

safeIncLog' :: (MonadState Int m, MonadError () m, MonadWriter String m) => m ()
safeIncLog' = do
            n <- get
            if | n < 42 -> tell "Safely updated counter" >> safeInc'
                | otherwise -> throwError()

-- In the first part of the homework, you will use monads to write an
-- interactive two-player game of Connect Four with users. Per
-- Wikipedia: "Connect Four is a two-player connection board game, in
-- which the players choose a color and then take turns dropping
-- colored discs into a seven-column, six-row vertically suspended
-- grid. The pieces fall straight down, occupying the lowest available
-- space within the column. The objective of the game is to be the
-- first to form a horizontal, vertical, or diagonal line of four of
-- one's own discs."

-- Note: While the questions will give you the broad strokes of an
-- implementation, you will almost certainly want to define some
-- helper functions as part of your solution!

-- === The Board ===
-- For flexibility, we define constants for the row and column size of
-- the board, length of a winning sequence, and search depth for the
-- game tree:

rows :: Int
rows = 6

cols :: Int
cols = 7

-- The board is represented as a list of rows, where each row is a
-- list of player values, subject to the above row and column sizes:
type Board = [Row]
type Row = [Entry]

-- Each entry value in the board is either empty [E], a yellow checker
-- [Y], or a red checker [R]:
data Entry = E | Y | R deriving (Ord, Eq)

-- For example, here is a typical board:
exBoard :: Board
exBoard = [[E,E,E,E,E,E,E], 
           [E,E,E,E,E,E,E],
           [E,E,E,E,E,E,E],
           [E,E,E,R,R,E,E],
           [E,E,Y,Y,R,E,E],
           [E,Y,Y,R,R,R,Y]]



exWinBoard :: Board
exWinBoard = [[E,E,E,E,E,E,E],
              [E,E,E,E,E,E,E],
              [E,E,E,E,R,E,E],
              [E,E,E,R,R,E,E],
              [E,E,Y,Y,R,E,E],
              [E,Y,Y,R,R,R,Y]]

initialBoard :: Board
initialBoard = replicate rows (replicate cols E)

---
instance Show Entry where
  show Y = "Y"
  show E = "."
  show R = "R"

-- The following code displays a board on the screen:
showBoard :: Board -> IO ()
showBoard b =
   putStrLn (unlines (map showRow b ++ line ++ nums))
     where
       showRow  = (foldl (++) []) . (map show)
       line     = [replicate cols '-']
       nums     = [take cols ['0'..]]




isMem::(Eq a) => a -> [a]->Bool
isMem y ys = foldl(\acc x -> if x == y then True else acc) False ys

totalVal::(Eq a) => a ->[a]->Int
totalVal y ys = foldl(\acc x -> if x == y then acc+1 else acc) 0 ys

winningRow::[Entry]->Bool
winningRow xs = totalVal Y xs == 4 || totalVal R xs == 4

getNthElement::[Entry]->Int->Entry
getNthElement xs y = xs !! y

winningRow'::Row->Int->Bool
winningRow' a b = getNthElement a b == getNthElement a (b+1) &&
                  getNthElement a b == getNthElement a (b+2) &&
                  getNthElement a b == getNthElement a (b+3)

getNthRow::Board->Int->Row
getNthRow xs y = xs !! y

winningCol'::Board->Int->Int->Bool
winningCol' a b c = (getNthElement(getNthRow a b) c == getNthElement(getNthRow a (b+1)) c)&&
                    (getNthElement(getNthRow a b) c == getNthElement(getNthRow a (b+2)) c) &&
                    (getNthElement(getNthRow a b) c == getNthElement(getNthRow a (b+3)) c)

totalColVal::Board->Entry->Int->Int
totalColVal xs a y = foldl(\acc x -> if getNthElement x y  == a then acc+1 else acc) 0 xs


winningCol::Board->Int->Bool
winningCol xs c = totalColVal xs Y c == 4 || totalColVal xs R c ==4 

getNthDiagonal::Board->Int->Int->Entry
getNthDiagonal xs a b = (getNthRow xs a) !! b

winningRightDiagonal::Board->Entry->Int->Int->Bool
winningRightDiagonal xs a y z = rc1 == a && rc2 == a && rc3 == a && rc4 == a
                              where rc1 = getNthDiagonal xs y z
                                    rc2 = getNthDiagonal xs (y+1) (z+1)
                                    rc3 = getNthDiagonal xs (y+2) (z+2)
                                    rc4 = getNthDiagonal xs (y+3) (z+3)

winningLeftDiagonal::Board->Entry->Int->Int->Bool
winningLeftDiagonal xs a y z = rc1 == a && rc2 == a && rc3 == a && rc4 == a
                              where rc1 = getNthDiagonal xs y z
                                    rc2 = getNthDiagonal xs (y+1) (z-1)
                                    rc3 = getNthDiagonal xs (y+1) (z-2)
                                    rc4 = getNthDiagonal xs (y+3) (z-3)

checkColumns::Board->Bool
checkColumns b = winningCol b 0 || winningCol b 1 || winningCol b 2 || winningCol b 3 || winningCol b 4 || winningCol b 5 || winningCol b 6

checkRows::Board->Bool
checkRows b = winningRow(getNthRow b 0) ||winningRow(getNthRow b 1) ||winningRow(getNthRow b 2) ||winningRow(getNthRow b 3) ||winningRow(getNthRow b 4) ||winningRow(getNthRow b 5)

checkRightDiagonals::Board->Entry->Bool
checkRightDiagonals b c = winningRightDiagonal b c 0 0 || winningRightDiagonal b c 0 1 || winningRightDiagonal b c 0 2 || winningRightDiagonal b c 0 3 ||
                  winningRightDiagonal b c 1 0 || winningRightDiagonal b c 1 1 || winningRightDiagonal b c 1 2 || winningRightDiagonal b c 1 3 ||
                   winningRightDiagonal b c 2 0 || winningRightDiagonal b c 2 1 || winningRightDiagonal b c 2 2 || winningRightDiagonal b c 2 3 

checkLeftDiagonals::Board->Entry->Bool
checkLeftDiagonals b c = winningRightDiagonal b c 0 6 || winningRightDiagonal b c 0 5 || winningRightDiagonal b c 0 4 || winningRightDiagonal b c 0 3 ||
                  winningRightDiagonal b c 1 6 || winningRightDiagonal b c 1 5 || winningRightDiagonal b c 1 4 || winningRightDiagonal b c 1 3 ||
                   winningRightDiagonal b c 2 6 || winningRightDiagonal b c 2 5 || winningRightDiagonal b c 2 4 || winningRightDiagonal b c 2 3 


checkDiagonals::Board->Bool
checkDiagonals b = checkLeftDiagonals b Y || checkLeftDiagonals b R || checkRightDiagonals b Y || checkRightDiagonals b R

win::Board->Bool
win b  = checkColumns b == True || checkRows b == True ||  checkDiagonals b == True

winningBoard :: (MonadError () m, MonadState (m Entry) m) => Board -> m Entry
winningBoard b = get >>= \x -> if win b == True then x else throwError ()


-- -- Question 7 [2pts]: Write a monadic [playMove] function that applies [makeMove] to
-- -- update the current game state, which consists of the current board
-- -- and an [Entry] value representing the player who will make the next
-- -- move.
-- playMove :: (MonadState (Board, Entry) m, MonadError () m) => Int -> m ()
-- playMove i = error "Fill In Here"

-- -- The playMove function uses the [runExcept] and [execStateT]
-- -- functions from the first three questions to turn the monadic game
-- -- state into a pure value:
-- playMove' :: Board -> Entry -> Int -> Either () (Board, Entry)
-- playMove' b e i = runExcept $ execStateT (playMove i) (b, e)

-- -- Question 8 [5pts]:
-- -- Define a [playGame] function that:
-- -- 1) displays the current board,
-- -- 2) queries the user for the next move, It may be helpful to use the [Read] typeclass can be used to convert the input string to an [Int].
-- -- 3) Uses [playMove] to update the game state:
-- -- 3a) If the resulting board is a winner, the identity of the winner should be printed
-- -- 3b) If there is no winner on the current board, [playGame] should recursive, allowing the game to continue
-- -- 3c) In the case that an invalid move was supplied, the game state should remain unchanged, and the game should continue as in 3b

-- playGame' :: Board -> Entry -> IO ()
-- playGame' b e = error "Fill In Here"

-- -- [playGame] starts a game of Connect Four with the expected initial values.
-- playGame :: IO ()
-- playGame = playGame' initialBoard R
              


-- PART 2
data SimpleType =
       TBool
     | TyArrow SimpleType SimpleType
     | TyVar Int
     | Empty
      deriving (Eq)

instance Show SimpleType where
     show (TyVar n) = "X" ++ show n
     show TBool = "Bool"
     show (TyArrow t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"

-- The full syntax of this language is thus:
--        t ::= x                      variable
--           | \x:T. t2                abstraction
--           | t1 t2                   application
--           | true                    constant true
--           | false                   constant false
--           | if t1 then t2 else t3   conditional

data STLCExp =                        -- t ::=
     STVar String                     --       x
   | Lambda String SimpleType STLCExp --       \x : T. t
   | App STLCExp STLCExp              --       t1 t2
   | BoolExp Bool                     --       true | false
   | IfExp STLCExp STLCExp STLCExp    --       if tc then tt else te
   deriving (Eq)

instance Show STLCExp where
  show (STVar x) = x
  show (Lambda x ty e) = "\\" ++ x ++ " : " ++ show ty ++ ". " ++ show e
  show (App e1 e2) = "(" ++ show e1 ++ ") (" ++ show e2 ++ ")"
  show (BoolExp True) = "true"
  show (BoolExp False) = "false"
  show (IfExp ec et ee) = "if " ++ show ec ++ " then " ++ show et ++ " else " ++ show ee

-- Environments map variable names to types
type LamTypCtxt = [(String, SimpleType)]

-- Extending an Environment with a new variable typing
extendLTC :: String -> SimpleType -> LamTypCtxt -> LamTypCtxt
extendLTC x ty env = (x, ty) : env

-- Type Constraints are equations between types: T1 = T2
data TypeConstraint = TypeEq SimpleType SimpleType
instance Show TypeConstraint where
         show (TypeEq t1 t2) = show t1 ++ " = " ++ show t2
type TypeConstraints = [TypeConstraint]

-- Our checker only fails if there is an occurence of an unbound variable
data SimpleTypeError
  = UnBoundVar String deriving Show

-- The constaint-based type-checker has monads in order to:
-- Throw an error if there is an unbound type variable (ExceptT SimpleTypeError)
-- Keep track of the current typing context (ReaderT LamTypCtxt)
-- and Keep a list of fresh type variables (State Int)
type InferM = ExceptT SimpleTypeError (ReaderT LamTypCtxt (State Int))

-- inLamTypCtxt x ty e evaluates e in the current context extended with [x --> ty]
inLamTypCtxt :: String -> SimpleType -> InferM a -> InferM a
inLamTypCtxt x ty = local (extendLTC x ty)

-- getFresh gets a fresh variable in the current context
getFresh :: InferM Int
getFresh = do
           x <- get
           y <- put (x + 1)
           return x

-- lookupSTVar gets the type of a variable in the current typing context
lookupSTVar :: String -> InferM SimpleType
lookupSTVar x = do
  env <- ask -- Get the current environment
  case lookup x env of
    Just e  -> return e
    Nothing -> Control.Monad.Except.throwError $ UnBoundVar x


processBoolean::STLCExp->Bool
processBoolean (BoolExp True) = True
processBoolean (BoolExp False) = False

getSimpleType::STLCExp->SimpleType
getSimpleType (Lambda a b c) = b
getSimpleType (App a b) = TyArrow (getSimpleType a) (getSimpleType b) 
getSimpleType (BoolExp _) = TBool
getSimpleType (IfExp a b c) = if processBoolean a then getSimpleType b else getSimpleType b
getSimpleType (STVar a) = (TyVar (read a :: Int)) 


-- Question 9 [5pts]: Write a constraint-based typechecker for the
-- simply typed lambda calculus with booleans, following the typing
-- rules included in the comments above each case. Make sure to return
-- an appropriate type error in the case that a rule does not apply.
inferConstraints :: STLCExp -> InferM (SimpleType, TypeConstraints)
--  ----------------------  CT-True
--    Γ ⊢ true : Bool | [ ]

--  ----------------------  CT-False
--    Γ ⊢ false : Bool | [ ]
inferConstraints (BoolExp _) =  return (TBool, [])                            

-- --   Γ ⊢ ec : Tc | Cc     Γ ⊢ et : Tt | Ct     Γ ⊢ ee : Te | Ce
-- --  ----------------------  CT-If
-- --    Γ ⊢ if ec then et else ee : Tt | [Tc = Bool, Tt = Te] ++ Cc ++ Ct ++ Ce
-- inferConstraints (IfExp ec et ee) = return (fst(inferConstraints(et)),[TypeEq fst(inferConstraints(cc) TBool, fst(inferConstraints(et) fst(inferConstraints(ee))))] ++ snd(inferConstraints(et)) ++ snd(inferConstraints(ec)) ++ snd(inferConstraints(ee))) 

inferConstraints (IfExp ec et ee) = do
                  (tc, cc) <- inferConstraints (ec)
                  (tt, ct) <- inferConstraints (et)
                  (te, ce) <- inferConstraints (ee)
                  return (tt, [TypeEq tc TBool, TypeEq tt te] ++ cc ++ ct ++ ce)
-- --   Γ [x --> ty] ⊢ e : ty2 | C
-- --  ------------------------------  CT-Abs
-- --    Γ ⊢ \x:ty. e : ty -> ty2 | C
-- inferConstraints (Lambda x ty e) = return (TyArrow ty fst(inLamTypCtxt x ty (inferConstraints e)), snd(inLamTypCtxt x ty (inferConstraints e)))
inferConstraints (Lambda x ty e) = do
  (ty2, c) <- inLamTypCtxt x ty (inferConstraints e)
  return (TyArrow ty ty2, c)

-- --   Γ ⊢ e1 : ty1 | C1          Γ ⊢ e2 : ty2 | C2            fresh X
-- --  -----------------------------------------------------------------  CT-Abs
-- --    Γ ⊢ e1 e2 : X | [ty1 = ty2 -> X] ++ C1 ++ C2
-- inferConstraints (App e1 e2) = do
inferConstraints (App e1 e2) = do
                  (ty1, c1) <- inferConstraints e1
                  (ty2, c2) <- inferConstraints e2
                  n <- getFresh
                  let ty = TyVar n in return(ty,[TypeEq ty1 (TyArrow ty2 ty)] ++ c1 ++ c2 )
--    Γ(x) = T
--  -------------- CT-Var
--    Γ ⊢ x : T
inferConstraints (STVar x) = do
                            n <- lookupSTVar x
                            return (n , [])


-- runInfer takes the starting value of fresh variables, a typing
-- context, and an expression to do constraint-based typing on
runInferConstraints :: Int -> LamTypCtxt -> STLCExp -> Either SimpleTypeError (SimpleType, TypeConstraints)
runInferConstraints freshestVar gamma e = fst (flip runState freshestVar (flip runReaderT gamma (runExceptT (inferConstraints e))))

lamExp1 = Lambda "x" (TyVar 0) (Lambda "y" (TyVar 1) (Lambda "z" (TyVar 2) (App (App (STVar "x") (STVar "z")) (App (STVar "y") (STVar "z")))))
-- print $ runInferConstraints 3 [ ] lamExp1 should equal ((X0 -> (X1 -> (X2 -> X5))),[X3 = (X4 -> X5),X0 = (X2 -> X3),X1 = (X2 -> X4)])

lamExp2 = Lambda "x" (TyVar 0) (App (STVar "x") (STVar "x"))
-- print $ runInferConstraints 3 [ ] lamExp2 should equal ((X0 -> X3),[X0 = (X0 -> X3)])

-- Substitutions are maps from Type Variables to Types:
type TypeSubst = [(Int, SimpleType)]

-- Questions 10-11 [2 x 2pts]: Implement the type substitution application and type
-- substitution functions discussed in class.

getIntType::SimpleType->Int
getIntType (TyVar a) = a


getType::SimpleType->Int
getType (TyArrow a _) = getIntType(a)
getType (TyArrow _ b) = getIntType(b)
getType (TyVar a) = a

getSub::TypeSubst->Int->SimpleType
getSub [] _ = Empty
getSub ((b,c):xs) a = if a == b then c else getSub xs a

applyTypeSubst :: TypeSubst -> SimpleType -> SimpleType
applyTypeSubst sigma  ty = if (getSub sigma (getType ty)) == Empty then TyVar (getType ty) else (getSub sigma (getType ty))
applyTypeSubst sigma (TyArrow a b) = (TyArrow (applyTypeSubst sigma (a)) (applyTypeSubst sigma (b)))
applyTypeSubst sigma  TBool = TBool

-- composeTypeSubst :: TypeSubst -> TypeSubst -> TypeSubst
-- composeTypeSubst sigma1 sigma2 = let t = getSub sigma1 in 


-- Question 14 [2pts]: Write a generator that generates lists of at
-- most 100 random elements:

genBoundedList :: (Arbitrary a) => Gen [a]
genBoundedList =  
  sized $
    \n -> do
      k <- choose (0, n)
      sequence [ arbitrary | _ <- [1..k] ]

-- Question 15 [1pts]: Write a predicate that encodes the fact that
-- the reverse is involutive, i.e. reversining a list twice should
-- produce the original list.
propRevInvolutive :: [Int] -> Bool
propRevInvolutive xs = xs == (reverse(reverse xs))

-- Question 16 [1pts]: Write a predicate that encodes the fact that
-- the length of the list built by appending two lists together is the
-- same as the sum of the lengths of the individual lists.
propLengthApp :: [Int] -> [Int] -> Bool
propLengthApp xs ys = (length xs) + (length ys) == (length (xs++ys))

-- Question 17 [1pts]: Write a predicate that encodes the fact that an
-- element is a member of the concatenation of two lists if and only
-- if it is an element of one of the individual lists.

propMemApp :: Int -> [Int] -> [Int] -> Bool
propMemApp x xs ys =  ((isMem x xs) || (isMem x ys)) == isMem x (xs++ys)

-- Question 18 [2pts]: Write a generator that produces a random board
-- that could result from a sequence of the function [playMove].
-- genConnectFourBoard :: Gen Board
-- genConnectFourBoard = error "Fill In Here"

getTotalEntry::Board->Entry->Int
getTotalEntry b a = foldr(\x acc -> foldr(\y val -> if y == a then val+1 else val) acc x ) 0 b
-- -- Question 19 [2pts]: A well-formed Connect Four board should either
-- -- have the same number of red and yellow checkers, or at most one
-- -- more red checker than yellow checkers. Write a predicate encoding
-- -- this property.

propWellFormedBoard :: Board -> Bool
propWellFormedBoard b = getTotalEntry b R == getTotalEntry b Y


main :: IO ()
main = print "HI"
