module MyLib (run) where

import Control.Monad.Cont (Cont, ContT (ContT, runContT), cont, runCont)
import Control.Monad.Cont.Class (MonadCont (callCC))
import Data.Functor.Identity (Identity (Identity, runIdentity))

run :: IO ()
run = do
    pure ()
    c1
        ( \act1 ->
            c2 $ \act2 -> do
                act1
                act2
        )

c1 :: (IO () -> IO ()) -> IO ()
c1 k = do
    putStrLn "A-1"
    k (pure ())
    putStrLn "A-2"

c2 :: (IO () -> IO ()) -> IO ()
c2 k = do
    putStrLn "B-1"
    k (pure ())
    putStrLn "B-2"

mCallCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
mCallCC f =
    ContT $ \cc -> runContT (f (\x -> ContT (\_ -> cc x))) cc

-- or
-- ContT $ \cc ->
-- let ContT g = f (\x -> ContT (\_ -> cc x))
-- in g cc

-- >>> runCont (bar True "fff") id
-- "Flag is set, exiting early"

-- >>> runCont (bar False "fff") id
-- "Result: Hello, fff"

bar :: Bool -> String -> Cont r String
bar flag name =
    mCallCC
        ( \cc -> do
            msg <- addGreeting flag name cc
            pure ("Result: " <> msg)
        )

addGreeting ::
    Bool ->
    String ->
    (String -> Cont r String) ->
    Cont r String
addGreeting flag name k = do
    if flag
        then k "Flag is set, exiting early"
        else pure ("Hello, " <> name)
