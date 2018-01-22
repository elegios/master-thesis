{-# LANGUAGE ViewPatterns, FlexibleContexts #-}

-- TODO: better error reporting, source location should be easy to add, even though maybe it'll only give the location in the "language" file

module Interpreter (removeEFuns, interpret, showCoreProgram) where

-- This is here to print the current value of a ref
-- It is only used in a show, and the value is never truly depended on, it is only for debugging
-- May still not be a good idea, but eh
import System.IO.Unsafe (unsafePerformIO)

import Control.Monad.Cont (ContT, runContT, callCC)
import Control.Monad.State (StateT, evalStateT, get, put, modify, gets)
import Control.Monad.Trans (liftIO)
import Control.Arrow (first)
import Data.IORef (IORef, newIORef, writeIORef, readIORef)
import Data.List (partition)
import Data.Maybe (catMaybes)

import Text.PrettyPrint ((<+>), (<>), ($+$))
import qualified Text.PrettyPrint as P

import qualified Data.Map as M
import qualified Data.Set as S

import Types.Lexer (Token(IntegerTok, StringTok, FloatTok))
import Types.Construction (NoSplice)
import Types.Ast

type Interpreter sym r a = ContT r (StateT (M.Map sym (Value sym r)) IO) a

data Value sym r = IntV Integer
                 | FloatV Double
                 | StringV String
                 | FuncV ((Value sym r) -> Interpreter sym r (Value sym r))
                 | RefV (IORef (Value sym r))
                 | NilV
                 | ConsV (Value sym r) (Value sym r)
                 | UnitV
                 | TrueV
                 | FalseV

instance Show sym => Show (Value sym r) where
  show (IntV i) = show i
  show (FloatV f) = show f
  show (StringV s) = show s
  show FuncV{} = "<func>"
  show (RefV ref) = "ref(" ++ show (unsafePerformIO $ readIORef ref) ++ ")"
  show NilV = "nil"
  show (ConsV h t) = show h ++ ":" ++ show t
  show UnitV = "unit"
  show TrueV = "true"
  show FalseV = "false"

instance Show sym => Eq (Value sym r) where
  (IntV a) == (IntV b) = a == b
  (FloatV a) == (FloatV b) = a == b
  (StringV a) == (StringV b) = a == b
  FuncV{} == FuncV{} = False
  (RefV a) == (RefV b) = a == b
  NilV == NilV = True
  (ConsV a as) == (ConsV b bs) = a == b && as == bs
  UnitV == UnitV = True
  TrueV == TrueV = True
  FalseV == FalseV = True
  _ == _ = False

type Node sym = NodeI (NoSplice (FixNode NoSplice sym)) sym

interpret :: (Ord sym, Show sym) => FixNode NoSplice sym -> IO String
interpret = fmap show . runInterpreter . eval . view . evalEFuns M.empty . unSplice

removeEFuns :: (Ord sym, Show sym) => FixNode NoSplice sym -> FixNode NoSplice sym
removeEFuns (FixNode n) = FixNode $ evalEFuns M.empty n

runInterpreter :: (Ord sym, Show sym, Show a) => Interpreter sym String a -> IO String
runInterpreter m = evalStateT (runContT m (return . show)) M.empty

evalEFuns :: Ord sym => M.Map sym (Node sym) -> Node sym -> Node sym
evalEFuns _ SyntaxSplice{} = error $ "Compiler error: evalEFuns encountered a syntax splice"
evalEFuns m n@Node{name, children} = case (constrName name, children) of
  ("appli", [MidNode (toExprFunc -> Just (i, body)), MidNode a]) ->
    M.insert i (evalEFuns m a) m `evalEFuns` body
  ("appli", [MidNode f, MidNode a]) -> case evalEFuns m f of
    (toExprFunc -> Just (i, body)) ->
      M.insert i (evalEFuns m a) m `evalEFuns` body
    f' -> n { children = [MidNode f', MidNode $ evalEFuns m a] }
  ("var", [MidIdentifier _ ((`M.lookup` m) -> Just a)]) -> a
  _ -> n { children = evalEFunsM m <$> children }
  where
    toExprFunc Node{ name = (constrName -> "exprfun")
                   , children = [_, MidIdentifier _ i, _, MidNode body]
                   } = Just (i, body)
    toExprFunc _ = Nothing
    evalEFunsM m (MidNode n) = MidNode $ evalEFuns m n
    evalEFunsM m (Repeated rep children) = Repeated rep $ evalEFunsM m <$> children
    evalEFunsM m (Sequenced r children) = Sequenced r $ evalEFunsM m <$> children
    evalEFunsM _ m = m

eval :: (Ord sym, Show sym) => NodeView sym -> Interpreter sym r (Value sym r)
eval = \case
  N "top" [n] -> evalWithArounds n

  N "defAfter" [_, Id i, _, e] ->
    evalWithArounds e >>= modify . M.insert i >> return UnitV
  N "defAround" _ -> return UnitV

  N "seqComp" [e1, _, e2] -> eval e1 >> eval e2
  N "appli" [f, e] -> eval f >>= \case
    FuncV f -> eval e >>= f
    value -> errorSource f $ "Attempted to apply non-function value: " ++ show value
  N "fun" [_, Id i, _, e] -> get >>= \closure -> return . FuncV $ \v -> scope $ do
    put closure
    modify $ M.insert i v
    evalWithArounds e

  N "var" [Id i] -> gets (M.lookup i) >>= \case
    Just v -> return v
    Nothing -> error $ "Identifier " ++ show i ++ " is not defined"

  N "builtin" [_, N builtinName _] -> return $ builtin builtinName

  N "int" [I i] -> return . IntV $ toInteger i
  N "float" [F f] -> return $ FloatV f
  N "string" [S s] -> return $ StringV s

  n -> error $ "Malformed / unsupported program node: " ++ show n

evalWithArounds :: (Show sym, Ord sym) => NodeView sym -> Interpreter sym r (Value sym r)
evalWithArounds n = scope $ do
  defArounds <- mapM defAround (getUnscopedNodes n)
  evalDefArounds $ catMaybes defArounds
  eval n
  where
    defAround (N "defAround" [_, Id i, _, e]) =
      Just . (, (i, evalWithArounds e)) <$> freeVariables e
    defAround _ = return Nothing
    evalDefArounds [] = return ()
    evalDefArounds defArounds = case partition (S.null . fst) defArounds of
      ([], notReady) -> error $ "Could not evaluate defArounds in a scope, these remain: " ++ show (fst . snd <$> notReady)
      (ready, notReady) -> do
        newM <- sequence . M.fromList $ snd <$> ready
        modify $ M.union newM
        evalDefArounds $ removeBound (M.keysSet newM) <$> notReady
    removeBound bound = first (`S.difference` bound)

freeVariables :: Ord sym => NodeView sym -> Interpreter sym r (S.Set sym)
freeVariables = scope . recur
  where
    recur = \case
      N "defAfter" [_, Id i, _, e] -> scope (recur e) <* modify (M.insert i UnitV)
      N "defAround" [_, _, _, e] -> scope $ recur e
      N "seqComp" [e1, _, e2] -> S.union <$> recur e1 <*> recur e2
      N "appli" [f, a] -> S.union <$> recur f <*> recur a
      N "fun" [_, Id i, _, e] -> scope $ do
        modify $ M.insert i UnitV
        recur e
      N "var" [Id i] ->
        boolean S.empty (S.singleton i) <$> gets (M.member i)
      _ -> return S.empty

getUnscopedNodes :: Show sym => NodeView sym -> [NodeView sym]
getUnscopedNodes n = (n :) $ case n of
  N "defAfter" _ -> []
  N "defAround" _ -> []
  N "seqComp" [e1, _, e2] -> getUnscopedNodes e1 ++ getUnscopedNodes e2
  N "appli" [f, a] -> getUnscopedNodes f ++ getUnscopedNodes a
  N "fun" _ -> []
  N "var" _ -> []
  N "int" _ -> []
  N "float" _ -> []
  N "string" _ -> []
  N "builtin" _ -> []
  _ -> error $ "getUnscopedNodes got unknown / unsupported syntax construction: " ++ show n

builtin :: (Ord sym, Show sym) => String -> Value sym r
builtin "callcc" = FuncV $ \case
  FuncV f -> callCC $ f . FuncV
  value -> error $ "Callcc called with non-function argument: " ++ show value
builtin "fix" = FuncV $ \case
  valueF@(FuncV f) -> let fixF = FuncV $ apply (f fixF) in return fixF
  value -> error $ "Fix called with non-function argument: " ++ show value
  where
    apply mf v = mf >>= \case
      FuncV f' -> f' v
      value -> error $ "Function given to fix returned a non-function result: " ++ show value
builtin "ref" = FuncV $ fmap RefV . liftIO . newIORef
builtin "deref" = FuncV $ \case
  RefV ref -> liftIO $ readIORef ref
  v -> return v
builtin "assign" = FuncV $ \case
  RefV ref -> return . FuncV $ unit . liftIO . writeIORef ref
  value -> error $ "Assign called with non-ref argument: " ++ show value
builtin "plus" = arithmetic (+) (+)
builtin "minus" = arithmetic (-) (-)
builtin "multiply" = arithmetic (*) (*)
builtin "divide" = arithmetic div (/)
builtin "equal" = FuncV $ \a -> return . FuncV $ \b -> boolToValue $ a == b
builtin "true" = TrueV
builtin "false" = FalseV
builtin "unit" = UnitV
builtin "cons" = FuncV $ \a -> return . FuncV $ \as -> return $ ConsV a as
builtin "head" = FuncV $ \case
  ConsV a _ -> return a
  value -> error $ "Called head with non-list argument: " ++ show value
builtin "tail" = FuncV $ \case
  ConsV _ as -> return as
  value -> error $ "Called tail with non-list argument: " ++ show value
builtin "nil" = NilV
builtin "if" = FuncV $ \case
  TrueV -> return . FuncV $ \th -> return . FuncV $ \_ -> run th
  FalseV -> return . FuncV $ \_ -> return . FuncV $ \el -> run el
  value -> error $ "Called if with non-boolean condition: " ++ show value
  where
    run (FuncV f) = f UnitV
    run value = error $ "Called if with non function then or else: " ++ show value
builtin "crash" = error $ "Evaluated #crash"  -- TODO: the interpreter might not be strict enough to actually evaluate this?
builtin "print" = FuncV $ liftIO . (UnitV <$) . putStrLn . show
builtin name = error $ "Unknown builtin: " ++ show name

boolToValue :: Bool -> Interpreter sym r (Value sym r)
boolToValue True = return TrueV
boolToValue False = return FalseV

scope :: Interpreter sym r a -> Interpreter sym r a
scope m = get >>= \s -> m <* put s

arithmetic :: Show sym => (Integer -> Integer -> Integer) -> (Double -> Double -> Double) -> Value sym r
arithmetic iop fop =
  FuncV $ \a -> return .
  FuncV $ \b -> return $ case (a, b) of
    (IntV a, IntV b) -> IntV $ a `iop` b
    (IntV a, FloatV b) -> FloatV $ fromInteger a `fop` b
    (FloatV a, IntV b) -> FloatV $ a `fop` fromInteger b
    (FloatV a, FloatV b) -> FloatV $ a `fop` b
    _ -> error $ "Arithmetic called with non-numeric arguments: " ++ show a ++ " and " ++ show b

view :: Node sym -> NodeView sym
view SyntaxSplice{} = error $ "Compiler error: view encountered a syntax splice"
view Node{name, children} = N (constrName name) $ viewM <$> children
  where
    viewM (MidNode n) = view n
    viewM (MidIdentifier _ i) = Id i
    viewM (Basic (IntegerTok _ i)) = I i
    viewM (Basic (FloatTok _ f)) = F f
    viewM (Basic (StringTok _ s)) = S s
    viewM _ = O

data NodeView sym = N String [NodeView sym] | Id sym | S String | I Int | F Double | O deriving (Show)

constrName :: String -> String
constrName = tail . dropWhile (/= '#')

unit :: Interpreter sym r a -> Interpreter sym r (Value sym r)
unit = (UnitV <$)

boolean :: a -> a -> Bool -> a
boolean a _ True = a
boolean _ a False = a

errorSource :: Show sym => NodeView sym -> String -> a
errorSource n message = error $ message ++ "\nProduced by:\n" ++ showCoreProgramInner n

showCoreProgram :: Show sym => FixNode NoSplice sym -> String
showCoreProgram (FixNode n) = showCoreProgramInner $ view n

showCoreProgramInner :: Show sym => NodeView sym -> String
showCoreProgramInner = P.render . fst . recur
  where
    recur = \case
      N "top" [e] -> recur e

      N "defAfter" [_, Id i, _, e] -> (P.parens . P.hang (P.text "defAfter" <+> P.text (show i) <+> P.equals) 1 $ noSurround (recur e), AtomicShow)
      N "defAround" [_, Id i, _, e] -> (P.parens . P.hang (P.text "defAround" <+> P.text (show i) <+> P.equals) 1 $ noSurround (recur e), AtomicShow)

      N "seqComp" [e1, _, e2] -> (noSurround (recur e1) <+> P.semi $+$ noSurround (recur e2), SeqCompShow)

      N "appli" [f, a] -> (fSurround (recur f) <+> appliSurround (recur a), AppliShow)
      N "fun" [_, Id i, _, e] -> (P.parens . P.hang (P.text "fun " <+> P.text (show i) <> P.text ".") 1 $ noSurround (recur e), AtomicShow)
      N "exprfun" [_, Id i, _, e] -> (P.parens . P.hang (P.text "efun " <+> P.text (show i) <> P.text ".") 1 $ noSurround (recur e), AtomicShow)

      N "var" [Id i] -> (P.text $ show i, AtomicShow)
      N "int" [I i] -> (P.int i, AtomicShow)
      N "float" [F f] -> (P.double f, AtomicShow)
      N "string" [S s] -> (P.text $ show s, AtomicShow)

      N "builtin" [_, N name _] -> (P.text "#" <> P.text name, AtomicShow)

      n -> error $ "showCoreProgram encountered malformed / unsupported node: " ++ show n
    appliSurround (s, AtomicShow) = s
    appliSurround (s, _) = P.parens s
    fSurround (s, SeqCompShow) = P.parens s
    fSurround (s, _) = s
    noSurround (s, _) = s

data ShowKind = AtomicShow | AppliShow | SeqCompShow deriving (Eq)
