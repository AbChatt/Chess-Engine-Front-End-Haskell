module Knight where

import KnightDef

-- Argument order: board size n, initial position, destination
knightServer :: MonadKnight m => Int -> (Int, Int) -> (Int, Int) -> m Integer
knightServer = error "TODO"


instance Functor KnightTrace where
    fmap f ks = ks >>= \a -> return (f a)

instance Applicative KnightTrace where
    pure a = return a
    fs <*> xs = fs >>= \f -> xs >>= \x -> return (f x)

instance Monad KnightTrace where


instance MonadKnight KnightTrace where


simpleCheck :: KnightTrace Integer -> Bool
simpleCheck = error "TODO"
