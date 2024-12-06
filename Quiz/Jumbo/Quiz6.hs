{-# LANGUAGE ScopedTypeVariables #-} 
{-# LANGUAGE FlexibleContexts #-}

module Quiz6 where

import Data.Function ((&))
import Control.Monad.Identity 
     ( Identity(runIdentity) )
import Control.Monad.State
     ( StateT(runStateT), MonadState(put, get) )
import Control.Monad.Except
     ( ExceptT, MonadError(throwError), runExceptT )

data Expr = Val Int
          | Div Expr Expr
          deriving (Show)

eval (Val i) = i
eval (Div e1 e2) = eval e1 `div` eval e2

ok1 :: Expr 
ok1 = Div (Div (Val 1800) (Val 2)) (Val 21)

ok2 :: Expr
ok2 = Div (Val 2) (Div (Val 1) (Div (Val 3) (Val 3)))

err :: Expr
err = Div (Val 2) (Div (Val 1) (Div (Val 2) (Val 3)))

errorS :: Show a => a -> a -> String
errorS x y = "Error dividing " ++ show x ++ " by " ++ show y

evalJumbo :: (MonadError String m, MonadState Int m) => Expr -> m Int
evalJumbo (Val n) = return n
evalJumbo (Div x y) = do rx <- evalJumbo x
                         ry <- evalJumbo y
                         if ry == 0 
                            then throwError $ errorS rx ry 
                            else do 
                                    (s :: Int) <- get 
                                    put (s + 1)
                                    return (rx `div` ry)

evalJumbo' :: (MonadError String m, MonadState String m) => Expr -> m Int
evalJumbo' (Val n) = do
  put (show n)
  return n
evalJumbo' (Div x y) = do
  rx <- evalJumbo' x
  sx <- get

  ry <- evalJumbo' y
  sy <- get

  let rightStr = case y of
                   Val _ -> sy
                   Div _ _ -> "(" ++ sy ++ ")"
  put (sx ++ " / " ++ rightStr)

  if ry == 0
    then throwError $ errorS rx ry
    else return (rx `div` ry)

goExSt :: Expr -> Identity (Either String (Int, String))
goExSt e = do
  (res, s) <- runStateT (runExceptT (evalJumbo' e)) ""
  return $ case res of
    Left err -> Left err
    Right i  -> Right (i, s)

goStEx :: Expr -> Identity (Either String Int, String)
goStEx e = runStateT (runExceptT (evalJumbo' e)) ""
