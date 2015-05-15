module Advanced where

import Control.Monad
import Control.Applicative
import Control.Arrow (first)
import Data.Maybe (fromJust)
import Data.List (nub)
import qualified Data.Map.Strict as M

newtype StateL s a = StateL (s -> [(a, s)])

runStateL :: StateL s a -> s -> [(a, s)]
runStateL (StateL g) = g

evalStateL :: StateL s a -> s -> [a]
evalStateL (StateL g) s = fmap fst (g s)

instance Functor (StateL s) where
    fmap f (StateL g) = StateL $ fmap (first f) . g

instance Applicative (StateL s) where
    pure x = StateL $ \s -> [(x, s)]
    fs <*> xs = StateL $ \s -> [(f a, s'') | (f, s' ) <- runStateL fs s
                                           , (a, s'') <- runStateL xs s]

instance Alternative (StateL s) where
    empty = StateL $ const []
    as <|> bs = StateL $ \s -> runStateL as s ++ runStateL bs s

instance Monad (StateL s) where
    return = pure
    --(StateL g) >>= k = StateL $ concat . fmap (\(a, s) -> runStateL (k a) s) . g
    (StateL g) >>= k = StateL $ \s -> [(b, s'') | (a, s' ) <- g s
                                                , (b, s'') <- runStateL (k a) s']

instance MonadPlus (StateL s) where
    mzero = empty
    mplus = (<|>)

select ::[a] ->[(a, [a])]
select [] = []
select(x:xs) = (x, xs) : [(y, x:ys) | (y, ys) <-select xs]

--     S E N D
-- +   M O R E
-- -----------
--   M O N E Y

solve :: StateL [Int] (Int, Int, Int)
solve = StateL select >>= go (nub "sendmoremoney") M.empty
  where
    go [c] subst i = prune (M.insert c i subst)
    go (c:cs) subst i = StateL select >>= go cs (M.insert c i subst)
    prune subst = do
        guard (get 's' /= 0 && get 'm' /= 0)
        let send  = toNumber "send"
            more  = toNumber "more"
            money = toNumber "money"
        guard $ send + more == money
        return (send, more, money)
      where
        get c = fromJust (M.lookup c subst)
        toNumber str = asNumber (map get str)
        asNumber = foldl (\t o -> t*10 + o) 0

main = print $ evalStateL solve [0..9]
