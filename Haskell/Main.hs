module Main where

import Control.Monad

newtype StateL s a = StateL (s -> [(a, s)])

runStateL :: StateL s a -> s -> [(a, s)]
runStateL (StateL g) s = g s

evalStateL :: StateL s a -> s -> [a]
evalStateL (StateL g) s = fmap fst (g s)

instance Functor (StateL s) where
    fmap f (StateL g) = StateL (\s -> fmap (\(a, s')-> (f a, s')) (g s))

instance Monad (StateL s) where
    return x = StateL (\s -> [(x, s)])
    (StateL g) >>= k = StateL (\s -> concat $ fmap (\(a, s') -> runStateL (k a) s') (g s))

instance MonadPlus (StateL s) where
    mzero = StateL (\s -> [])
    mplus = undefined

select ::[a] ->[(a, [a])]
select[] = []
select(x:xs) = (x, xs) : [(y, x:ys) | (y, ys) <-select xs]

asNumber :: [Int] -> Int
asNumber = foldl (\t o -> t*10 + o) 0

--     S E N D
-- +   M O R E
-- -----------
--   M O N E Y

solve = do
    s <- sel
    e <- sel
    n <- sel
    d <- sel
    m <- sel
    o <- sel
    r <- sel
    y <- sel
    guard (s /= 0 && m /= 0)
    let send  = asNumber [s,e,n,d]
        more  = asNumber [m,o,r,e]
        money = asNumber [m,o,n,e,y]
    guard (send + more == money)
    return (send, more, money)
  where sel = StateL select

main :: IO ()
main = print $ evalStateL solve [0..9] 

test = StateL select >>= (\x -> StateL select >>= \y -> return (x, y))

