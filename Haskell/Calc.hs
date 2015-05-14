module Calc where
import Control.Monad.State

data Item = Plus | Minus | Number Int
    deriving (Show, Eq)

type Plan = State [Item]

pop :: Plan Item
pop = do
    s <- get
    put (tail s)
    return (head s)

plus :: Plan Int
plus = do
    n <- calc
    m <- calc
    return (n + m)
    

minus :: Plan Int
minus = do
    n <- calc
    m <- calc
    return (n - m)
    

calc :: Plan Int
calc = do
    it <- pop
    case it of
        Plus     -> plus
        Minus    -> minus
        Number n -> return n
        
main :: IO ()
main = print $ runState calc [Plus, Minus, Number 2, Number 4, Number 1]
