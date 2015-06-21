module Solver where

import Control.Monad (guard, liftM, join)
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import qualified Data.Map.Strict as M

--     S E N D
-- +   M O R E
-- -----------
--   M O N E Y

-- PartialMap contains a list of digits,
-- which don't have a mapping yet,
-- and a map from Char to Int
type PartialMap = ([Int], M.Map Char Int)
type StateL = StateT PartialMap []

toNumber :: [Char] -> StateL Int
toNumber str = do
    digits <- mapM mapC str
    return (asNumber digits)

asNumber :: [Int] -> Int
asNumber = foldl (\t o -> t * 10 + o) 0

-- Look up the character in the map
-- If not found, create all possible mappings for it using remaining digits
-- Otherwise, return the digit mapped to it
selC :: Char -> StateL Int
selC c = do
    (digits, subst) <- get
    case M.lookup c subst of
        Nothing -> do
            i <- lift digits
            put (filter (/= i) digits, M.insert c i subst)
            return i
        Just i  -> return i

-- Look up a character assuming it's already been mapped
mapC :: Char -> StateL Int
mapC c = do
    (digits, subst) <- get
    case M.lookup c subst of
        Nothing -> error ("No mapping for " ++ [c])
        Just i  -> return i

-- Find all substitutions for which addends add up to result
-- No substitution may result in a number that starts with zero

solve :: [String] -> String -> StateL ([Int], Int)
solve addends result =  go (map reverse addends) (reverse result) 0
  where
    -- Perform long addition, right to left (arguments are reversed)
    -- For each character, look it up in the partial map
    -- or generate all possible substitutions for it
    go :: [String] -> String -> Int -> StateL ([Int], Int)
    go [] result carry = finish result carry
    go addends result carry = do
        is <- mapM (selC . head) addends
        r <- selC (head result)
        let s = sum is + carry
        guard (s `mod` 10 == r)
        let tails = filter (not . null) $ map tail addends
        go tails (tail result) (s `div` 10)
    -- We have reached the left end of all addends
    finish :: String -> Int -> StateL ([Int], Int)
    finish [] carry = guard (carry == 0) >> prune
    finish result carry = do
        r <- selC (head result)
        guard (r == carry && null (tail result))
        prune
    -- Make sure no number starts with zero
    prune :: StateL ([Int], Int)
    prune = do
        is <- mapM (mapC . head) addends
        k <- mapC (head result)
        guard (all (/= 0) (k : is))
        ns <- mapM toNumber addends
        r <- toNumber result
        return (ns, r)

-- ["send", "more"] "money"
-- ["pink", "brown"] "ivory"
-- ["IO", "TITAN", "URANUS"] "CHARON"
-- ["ONE", "TWO", "FIVE", "NINE", "ELEVEN", "TWELVE", "FIFTY"] "NINETY"
main = do
    let lst = evalStateT (solve ["ONE", "TWO", "FIVE", "NINE", "ELEVEN", "TWELVE", "FIFTY"] "NINETY") ([0..9], M.empty)
        zero = sum $ fmap (\(as, r) -> sum as - r) lst
    print zero
    print lst
