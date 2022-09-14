-- CS 456 Homework 4
-- Due date: 11/05/2021 by 6:00PM

-- Name: YOUR NAME HERE
-- Email: YOUR EMAIL HERE

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
{-# LANGUAGE TypeSynonymInstances #-}


import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except
import Data.List
import Data.Maybe
-- import Control.Monad.Maybe
import Control.Applicative
import Test.QuickCheck

-- Monad warm ups:

-- Question 1 [1pt]: Define a function that increments a counter
-- implemented using the State monad. Do not use 'do' notation, use
-- the bind operation instead (>>=).

incCount :: MonadState Int m => m ()
incCount = get >>= \s -> put (s + 1)

-- [execState :: State s a -> s -> s] is a function that evaluates a
-- monadic value in some initial state and returns the resulting final
-- state. Here are some examples of incCount:
-- * execState incCount 0 = 1
-- * execState incCount 10 = 11
-- * execState incCount 41 = 42

-- Question 2 [1pt]: Define a function that checks if the value stored
-- in the current state is less than 42, incrementing it if so, and
-- throwing an error otherwise. Do not use 'do' notation, use
-- the bind operation instead (>>=).

-- My Version that explains the error
-- safeInc :: (MonadState Int m, MonadError String m) => m ()
-- safeInc = get >>= \s -> if s < 42 then put (s+1) else throwError "state overflowed"

safeInc :: (MonadState Int m, MonadError () m) => m ()
safeInc = get >>= \s -> if s < 42 then put (s+1) else throwError ()

-- [execStateT :: Monad m => State s a -> s -> m s] is a function that
-- evaluates a monadic value in some initial state and returns the
-- resulting final state, wrapped with any remaining monadic bits.
-- [runExcept :: Except e a -> Either e a] is a function that lifts a
-- value living in the Exception Monad to a Either value.
-- Here are some examples of safeCount:
-- * runExcept (execStateT safeInc 0) = Right 1
-- * runExcept (execStateT safeInct 10) = Right 11
-- * runExcept (execStateT safeInc 42) = Left


-- Question 3 [1pt]: Define a function that, in addition to performing
-- a safe incrememnt operation, logs the event by adding the string
-- "Safely Updated Counter! " to a log implemented using the Writer
-- monad. Do not use 'do' notation, use the bind operation instead
-- (>>=).

safeIncLog :: (MonadState Int m, MonadError () m, MonadWriter String m) => m ()
safeIncLog = tell "Safely Updated Counter" >> safeInc

-- [runExcept :: Except e a -> Either e a] is a function that lifts a
-- value living in the Exception Monad to a Either value.
-- Here are some examples of safeCount:
-- * runWriter (runExceptT (execStateT safeIncLog 0)) = (Right 1,"Safely Updated Counter!")
-- * runWriter (runExceptT (execStateT (safeIncLog >> safeIncLog >> safeIncLog) 10)) = (Right 13, "Safely Updated Counter! Safely Updated Counter! Safely Updated Counter! ")
-- * runWriter (runExceptT (execStateT (safeIncLog) 42)) = (Left (),"")
-- * runWriter (runExceptT (execStateT (safeIncLog >> safeIncLog) 41)) = (Left (),"Safely Updated Counter!")

-- Question 4 [2pts]: Write variants of the above functions using the
-- monadic 'do' notation.

incCount' :: MonadState Int m => m ()
incCount' = do
  s <- get
  put (s + 1)

safeInc' :: (MonadState Int m, MonadError () m) => m ()
safeInc' = do
  s <- get
  if s < 42
    then put (s+1)
    else throwError ()


safeIncLog' :: (MonadState Int m, MonadError () m, MonadWriter String m) => m ()
safeIncLog' = do
  tell "Safely Updated Counter"
  safeInc

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

revv b = reverse [ reverse b' | b' <- b]

getSecondDiag b = filter (\ l -> length l > 3) $ [
  [ b !! row !! col | (row, col) <- zip ([0 .. rows - 1]) ([p .. cols - 1]) ]
  | p <- reverse [0 .. cols - 1]]
  ++ [
  [ revv b !! row !! col | (row, col) <- zip ([0 .. rows - 1]) ([p .. cols - 1]) ]
  | p <- reverse [0 .. cols - 1]]

getFirstDiag b = filter (\ l -> length l > 3) $ [
  [ b !! row !! col | (row, col) <- zip [0 .. rows - 1] (reverse [0 .. p]) ]
  | p <- [0 .. cols - 1]]
  ++ [
  [ revv b !! row !! col | (row, col) <- zip [0 .. rows - 1] (reverse [0 .. p]) ]
  | p <- [0 .. cols - 1]]

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

-- For example, [showBoard exBoard] gives the following output:
--    .......
--    .......
--    .......
--    ...XX..
--    ..OOX..
--    .OOXXXO
--    -------
--    0123456

-- Question 5 [10pts]: Write a [winningBoard] function that checks
-- whether a player has won the game, i.e. whether there is a
-- horizontal, vertical, or diagonal line of four of the same color
-- checker. This function should throw an error when there is not a
-- winner, and return the [Entry] of the winning player otherwise.
bisect :: Int -> [a] -> [[a]]
bisect 0 _ = []
bisect n l
  | length l < n = []
  | otherwise = take n l : bisect n (drop 1 l)

checkAllSame :: [Entry] -> Maybe Entry
checkAllSame l =
  if all (\ b -> b == R) l
  then return R
        else if all (\b -> b == Y) l
             then return Y
             else Nothing

winningDiagonal ::  Board -> Maybe Entry
winningDiagonal b =
  let bissectFstDiag = flatten $ map (bisect 4) (getFirstDiag b) in
  let bissectSndDiag = flatten $ map (bisect 4) (getSecondDiag b) in
    (afromList $ map checkAllSame bissectFstDiag)
    <|>
    (afromList $ map checkAllSame bissectSndDiag)

flatten :: [[a]] -> [a]
flatten [] = []
flatten (x : xs) = x ++ flatten xs

afromList :: [Maybe a] -> Maybe a
afromList [] = Nothing
afromList (x : xs) = x <|> afromList xs

winningHorizontal :: Board -> Maybe Entry
winningHorizontal b =
  let bissectedBoard = flatten $ map (bisect 4) b in
    afromList $ map checkAllSame bissectedBoard

winningVertical :: Board -> Maybe Entry
winningVertical b = winningHorizontal $ transpose b

winningBoard :: MonadError () m => Board -> m Entry
winningBoard b = case winningVertical b <|> winningHorizontal b <|> winningDiagonal b of
  Nothing -> throwError ()
  Just x -> return x

-- Question 6 [3pts]: Write a [makeMove] function to take a move by dropping a checker of
-- the input Entry into the column specified by the integer
-- argument. The inserted piece should fall straight down, occupying
-- the lowest available space within the column. In the case that the
-- column is completely full, the function should throw an error.

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth (n-1) newVal xs

-- First Int is which row
-- Second Int is which col
updateBoard :: Board -> Entry -> Int -> Int -> Board
updateBoard b e row col = replaceNth row (replaceNth col e (b !! row)) b

getFirstE :: [(Entry, Int)] -> Maybe Int
getFirstE [] = Nothing
getFirstE ((x, i) : xs) = if x == E then Just i else getFirstE xs


updateFirst :: [Entry] -> Entry -> Maybe [Entry]
updateFirst l en =
  let revl = reverse l in
  case getFirstE (zip revl [0..]) of
    Nothing -> Nothing
    Just pos' -> Just $ reverse $ replaceNth pos' en revl


makeMove :: MonadError () m => Board -> Entry -> Int -> m Board
makeMove b e col =
  if col > cols - 1
  then throwError ()
  else
    let transb = transpose b in
    let updt = transb !! col in
    case updateFirst updt e of
         Nothing -> throwError ()
         Just b' -> return $ transpose $ replaceNth col b' transb

-- Question 7 [2pts]: Write a monadic [playMove] function that applies [makeMove] to
-- update the current game state, which consists of the current board
-- and an [Entry] value representing the player who will make the next
-- move.
playMove :: (MonadState (Board, Entry) m, MonadError () m) => Int -> m ()
playMove i = do
  (b, e) <- get
  b' <- makeMove b e i
  let nextMove = if e == R then Y else R
  put (b', nextMove)

-- The playMove function uses the [runExcept] and [execStateT]
-- functions from the first three questions to turn the monadic game
-- state into a pure value:
playMove' :: Board -> Entry -> Int -> Either () (Board, Entry)
playMove' b e i = runExcept $ execStateT (playMove i) (b, e)

-- Question 8 [5pts]:
-- Define a [playGame] function that:
-- 1) displays the current board,
-- 2) queries the user for the next move, It may be helpful to use the [Read] typeclass can be used to convert the input string to an [Int].
-- 3) Uses [playMove] to update the game state:
-- 3a) If the resulting board is a winner, the identity of the winner should be printed
-- 3b) If there is no winner on the current board, [playGame] should recursive, allowing the game to continue
-- 3c) In the case that an invalid move was supplied, the game state should remain unchanged, and the game should continue as in 3b

playGame' :: Board -> Entry -> IO ()
playGame' b e = do
  showBoard b
  putStr "Insert action: "
  x <- readLn
  case playMove' b e x of
    Left () ->
      putStrLn "Ilegal Move!"
    Right (board, entry) ->
      case runExcept $ winningBoard board of
        Left () -> playGame' board entry
        Right e -> do
          showBoard b
          putStrLn $ show e ++ " Won!"

-- [playGame] starts a game of Connect Four with the expected initial values.
playGame :: IO ()
playGame = playGame' initialBoard R

-- Part 2: Type Inference for the Lambda Calculus [20pts]
data SimpleType =
       TBool
     | TyArrow SimpleType SimpleType
     | TyVar Int
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

-- Extend ing an Environment with a new variable typing
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
    Nothing -> throwError $ UnBoundVar x

-- Question 9 [5pts]: Write a constraint-based typechecker for the
-- simply typed lambda calculus with booleans, following the typing
-- rules included in the comments above each case. Make sure to return
-- an appropriate type error in the case that a rule does not apply.
inferConstraints :: STLCExp -> InferM (SimpleType, TypeConstraints)


--  ----------------------  CT-True
--    Γ ⊢ true : Bool | [ ]

--  ----------------------  CT-False
--    Γ ⊢ false : Bool | [ ]
inferConstraints (BoolExp True) = return (TBool, [])
inferConstraints (BoolExp False) = return (TBool, [])

--   Γ ⊢ ec : Tc | Cc     Γ ⊢ et : Tt | Ct     Γ ⊢ ee : Te | Ce
--  ----------------------  CT-If
--    Γ ⊢ if ec then et else ee : Tt | [Tc = Bool, Tt = Te] ++ Cc ++ Ct ++ Ce
inferConstraints (IfExp ec et ee) = do
  (tc, cc) <- inferConstraints (ec)
  (tt, ct) <- inferConstraints (et)
  (te, ce) <- inferConstraints (ee)
  return (tt, [TypeEq tc TBool, TypeEq tt te] ++ cc ++ ct ++ ce)

--   Γ [x --> ty] ⊢ e : ty2 | C
--  ------------------------------  CT-Abs
--    Γ ⊢ \x:ty. e : ty -> ty2 | C
inferConstraints (Lambda x ty e) = do
  (ty2, c) <- inLamTypCtxt x ty (inferConstraints e)
  return (TyArrow ty ty2, c)

--   Γ ⊢ e1 : ty1 | C1          Γ ⊢ e2 : ty2 | C2            fresh X
--  -----------------------------------------------------------------  CT-Abs
--    Γ ⊢ e1 e2 : X | [ty1 = ty2 -> X] ++ C1 ++ C2
inferConstraints (App e1 e2) = do
  (ty1, c1) <- inferConstraints e1
  (ty2, c2) <- inferConstraints e2
  n <- getFresh
  let ty = TyVar n in
    return (ty, [TypeEq ty1 (TyArrow ty2 ty)] ++ c1 ++ c2)
--    Γ(x) = T
--  -------------- CT-Var
--    Γ ⊢ x : T
inferConstraints (STVar x) = do
  ty <- lookupSTVar x
  return (ty, [])

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

findSubst :: Int -> TypeSubst -> Maybe SimpleType
findSubst _ [] = Nothing
findSubst i ((i', ty) : xs) =
  if i == i' then Just ty
  else findSubst i xs

applyTypeSubst :: TypeSubst -> SimpleType -> SimpleType
applyTypeSubst sigma (TyVar i) =
  case findSubst i sigma of
    Nothing -> TyVar i
    Just ty -> ty
applyTypeSubst sigma (TyArrow t1 t2) =
  let t1' = applyTypeSubst sigma t1 in
  let t2' = applyTypeSubst sigma t2 in
  TyArrow t1' t2'
applyTypeSubst _ TBool = TBool

composeTypeSubst :: TypeSubst -> TypeSubst -> TypeSubst
composeTypeSubst sigma1 [] = sigma1
composeTypeSubst sigma1 ((i, ty) : xs) =
  case ty of
    TyVar n ->
      case findSubst n sigma1 of
        Just ty' -> composeTypeSubst ((i, ty') : sigma1) xs
        Nothing -> composeTypeSubst ((i, ty) : sigma1) xs
    _ -> composeTypeSubst ((i, ty) : sigma1) xs

-- Question 12 [6pts]: Implement the unification algorithm from class. It
-- should take a set of TypeConstraints as input, and return the most
-- general TypeSubst satisfying those constraints, if one
-- exists. (This question is worth the equivalent of 10 "normal"
-- questions.)

fv :: SimpleType -> [Int]
fv TBool = []
fv (TyVar i) = [i]
fv (TyArrow t1 t2) = fv t1 ++ fv t2


unify :: TypeConstraints -> Except () TypeSubst
unify [] = return []
unify ((TypeEq s t) : cs)
  | s == t = unify cs
  | TyVar x <- s, not (x `elem` fv t) =
      let x_to_t = [(x, t)] in
      let cs' = map (\ (TypeEq t1 t2) ->
                       let t1' = applyTypeSubst x_to_t t1 in
                       let t2' = applyTypeSubst x_to_t t2 in
                         TypeEq t1 t2'
                    ) cs in
        do
          sigma1 <- unify cs'
          return $ composeTypeSubst sigma1 x_to_t
  | TyVar x <- t, not (x `elem` fv s) =
      let subst = [(x, s)] in
      let cs' = map (\ (TypeEq t1 t2) ->
                       let t1' = applyTypeSubst subst t1 in
                       let t2' = applyTypeSubst subst t2 in
                         TypeEq t1' t2
                    ) cs in
        do
          sigma1 <- unify cs'
          return $ composeTypeSubst sigma1 subst
  | TyArrow s1 s2 <- s, TyArrow t1 t2 <- t =
      unify (cs ++ [TypeEq s1 t1, TypeEq s2 t2])
unify _ = throwError ()


-- Question 13 [5pts]: Combine your answers to Question 13 with
-- runInferConstraints to build a type inference function that takes a
-- simply-typed lambda term, e, and returns the principal type of e in
-- the empty typing context.  Note: you'll need to give
-- runInferConstraints an appropriately large value for fresh
-- variables. (This question is worth the equivalent of 3 "normal"
-- questions.)

maxVar (STVar _) = 0
maxVar (Lambda _ t e) = max (maximum (fv t)) (maxVar e)
maxVar (App t1 t2) = max (maxVar t1) (maxVar t2)
maxVar (BoolExp _) = 0
maxVar (IfExp t1 t2 t3) =
  let n1 = maxVar t1 in
  let n2 = maxVar t2 in
  let n3 = maxVar t3 in
    maximum [n1, n2, n3]

inferType :: STLCExp -> Maybe SimpleType
inferType exp =
  let n = maxVar exp + 1in
    case runInferConstraints n [] exp of
      Left _ -> Nothing
      Right (ty, tc) ->
        case runExcept $ unify tc of
          Left _ -> Nothing
          Right subst -> Just $ applyTypeSubst subst ty

-- inferType lamExp1 should evaluate to: Just ((X2 -> (X11 -> X12)) -> ((X2 -> X11) -> (X2 -> X12)))
-- inferType lamExp2 should evaluate to: Nothing

-- Part 3: Property-Based Testing [10pts]

-- Question 14 [2pts]: Write a generator that generates lists of at
-- most 100 random elements:

genBoundedList :: (Arbitrary a) => Gen [a]
genBoundedList =
  sized $ \ n -> do
  k <- choose (0, n)
  sequence [ arbitrary | _ <- [1..k] ]

-- Question 15 [1pts]: Write a predicate that encodes the fact that
-- the reverse is involutive, i.e. reversining a list twice should
-- produce the original list.
propRevInvolutive :: [Int] -> Bool
propRevInvolutive l = reverse (reverse l) == l

-- Question 16 [1pts]: Write a predicate that encodes the fact that
-- the length of the list built by appending two lists together is the
-- same as the sum of the lengths of the individual lists.
propLengthApp :: [Int] -> [Int] -> Bool
propLengthApp l1 l2 = length (l1 ++ l2) == length l1 + length l2

-- Question 17 [1pts]: Write a predicate that encodes the fact that an
-- element is a member of the concatenation of two lists if and only
-- if it is an element of one of the individual lists.
propMemApp :: Int -> [Int] -> [Int] -> Bool
propMemApp x l1 l2 = (x `elem` (l1 ++ l2)) == (x `elem` l1 || x `elem` l2)

-- Question 18 [2pts]: Write a generator that produces a random board
-- that could result from a sequence of the function [playMove].

-- playMove :: (MonadState (Board, Entry) m, MonadError () m) => Int -> m ()
-- playMove' :: Board -> Entry -> Int -> Either () (Board, Entry)
rep :: Int -> Board -> Entry -> [Int] -> Board
rep 0 board en [] = board
rep 0 board en (i:_) =
  case playMove' board en i of
    Left _ -> board
    Right (b, _) -> b
rep n board en (i:is) =
  let board' = (playMove' board en i) in
    case board' of
      Left _ -> board
      Right (b, e) -> rep (n-1) b e is

genConnectFourBoard :: Gen Board
genConnectFourBoard =
  sized $ \ n ->
  do
    l <- sequence [ choose (0, 6) | _ <- [1 .. n] ]
    return $ rep n initialBoard R l


-- Question 19 [2pts]: A well-formed Connect Four board should either
-- have the same number of red and yellow checkers, or at most one
-- more red checker than yellow checkers. Write a predicate encoding
-- this property.

-- Left holds R count and right holds Y count
redYellowCount :: Board -> (Int, Int)
redYellowCount board = foldr (\ b acc ->
                                foldr (\ e (r, y) ->
                                         if e == R
                                         then (r+1, y)
                                         else
                                           if e == Y then (r, y+1) else (r, y)) acc b ) (0, 0) board

propWellFormedBoard :: Board -> Bool
propWellFormedBoard board =
  let (r, y) = redYellowCount board in
  (r == y) || (r == y + 1)

-- instance Arbitrary Board where
--   arbitrary b = genConnectFourBoard

-- Question 20 [1pts]: Use your answers to questions 18+19 to test
-- your implementation of [playMove] from part two.
testPlayMove :: IO ()
testPlayMove =
  quickCheck $ forAll genConnectFourBoard propWellFormedBoard

main = print $ "Does anyone read this line?"
