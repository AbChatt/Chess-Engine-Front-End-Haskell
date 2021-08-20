module Knight where

import KnightDef

-- Argument order: board size n, initial position, destination
knightServer :: MonadKnight m => Int -> (Int, Int) -> (Int, Int) -> m Integer
knightServer n (a, b) (c, d) = recurseHelper 0 n (a, b) (c, d)

isValidMove :: [(Int, Int)] -> (Int, Int) -> Bool
isValidMove [] move = False
isValidMove (x:xs) move
    | move == x = True
    | otherwise = isValidMove xs move

-- parameters: move count, all the ones in order from knightServer
recurseHelper :: MonadKnight m => Integer -> Int -> (Int, Int) -> (Int, Int) -> m Integer
recurseHelper numMoves n (a, b) (c, d)
    | (a, b) == (c, d) = (return) numMoves
    | otherwise = do
        validPos <- (return) (knightNext n (a, b))
        nextPos <- tellPosAskNext (MkPos n (c, d) (a, b) validPos)
        result <- (return) (isValidMove validPos nextPos)
        case result of
            True -> recurseHelper (numMoves + 1) n nextPos (c, d)
            False -> buzz >> recurseHelper numMoves n (a, b) (c, d)

instance Functor KnightTrace where
    fmap f ks = ks >>= \a -> return (f a)

instance Applicative KnightTrace where
    pure a = return a
    fs <*> xs = fs >>= \f -> xs >>= \x -> return (f x)

instance Monad KnightTrace where
    return a = Answer a
    Answer a >>= b = b a
    Buzz a >>= b = Buzz (a >>= \x -> b x)
    Step pos func >>= b = Step pos  (\x -> func x >>= \y -> b y)

instance MonadKnight KnightTrace where
    -- buzz is equivalent to buzzing and that's it - no next step so return empty as invalid move
    buzz = Buzz (Answer ()) 

    -- tellPosAskNext pos is equivalent to calling tellPosAskNext pos and then no function given
    -- based on KnightServer instructions, it checks the user given position so return that position
    tellPosAskNext pos = Step pos (\(x, y) -> Answer (x, y))


simpleCheck :: KnightTrace Integer -> Bool
simpleCheck (Answer a) = False
simpleCheck (Buzz _) = False
simpleCheck (Step (MkPos n (a, b) (c, d) nextMoves) func) = do
    if (c, d) /= (1, 1) then False
    else case func (3, 2) of        -- cannot check equality using == as KnightTrace does not derive Eq
        Answer 1 -> True
        otherwise -> False