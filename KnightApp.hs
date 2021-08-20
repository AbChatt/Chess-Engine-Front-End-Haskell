{-# LANGUAGE RankNTypes #-}

module KnightApp where

import System.IO
import Text.Read

import KnightDef
import Knight

doBuzz = putStrLn "Sorry, that was an illegal move."

doAsk (MkPos n dest current choices) =
    putStrLn (show n ++ "x" ++ show n ++ " board, "
              ++ "destination=" ++ show dest ++ ", "
              ++ "current=" ++ show current ++ "\n"
              ++ "your next position can be one of: " ++ show choices)
    >> readXY
    >>= \newpos -> pure newpos

doAnswer i = putStrLn ("You have used " ++ show i ++ " moves.")

readXY =
    putStr "Please enter the next position: "
    >> hFlush stdout
    >> getLine
    >>= \line -> case readMaybe line of
    Nothing -> putStrLn "Sorry, need (x,y)" >> readXY
    Just pos -> pure pos

instance MonadKnight IO where
    buzz = doBuzz
    tellPosAskNext pos = doAsk pos

playIO :: (forall m. MonadKnight m => m Integer) -> IO ()
playIO server = server >>= doAnswer

playTrace :: KnightTrace Integer -> IO ()
playTrace (Answer i) = doAnswer i
playTrace (Step pos f) = doAsk pos >>= \newpos -> playTrace (f newpos)
playTrace (Buzz k) = doBuzz >> playTrace k
