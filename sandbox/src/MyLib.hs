{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingVia #-}

module MyLib where

import Control.Monad.Cont (
  Cont,
  ContT (ContT, runContT),
  MonadIO (liftIO),
  cont,
  runCont,
 )
import Control.Monad.Cont.Class (MonadCont (callCC))
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.Reader.Class
import Control.Monad.Trans (lift)
import Control.Monad.Writer
import Control.Monad.Writer.Class (tell)
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Debug.Trace (trace)
import Control.Monad.Trans.Class

run :: IO ()
run = do
  runContT (pa >> pb) pure

type Env m = CoroutineT m ()
newtype CoroutineT m a = CoroutineT
  { runCoroutineT :: ContT () (ReaderT (Env m) m) a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader (Env m)
    
    ) 
    -- deriving MonadTrans via (ContT )

instance MonadTrans CoroutineT where
    lift = CoroutineT . lift . lift
    {-# INLINE lift #-}

-- runC :: ContT a1 (ReaderT r m) a2 -> (a2 -> ReaderT r m a1) -> r -> m a1
runIC m c r = runReaderT (runContT  (runCoroutineT m) c) r

instance MonadWriter w m => MonadWriter w (CoroutineT m) where
  tell w = CoroutineT $ lift (tell w)
  writer = error "TODO: writer"
  listen = error "TODO: listen"
  pass = error "TODO: pass"

-- yield = undefined

coroutine3, coroutine4 :: CoroutineT (Writer String) ()
coroutine3 = do
  tell "1"
  yield
  yield
  tell "2"
coroutine4 = do
  tell "a"
  yield
  tell "b"
  yield
  tell "c"
  yield
  tell "d"
  yield

pa :: ContT () IO ()
pa = do
  lift $ putStrLn "pa-1"
  lift $ putStrLn "pa-2"

pb :: ContT () IO ()
pb = do
  lift $ putStrLn "pb-1"
  lift $ putStrLn "pb-2"

-- c :: ContT () IO Int
-- c :: ContT w IO ()

c :: ContT () IO ()
c =
  -- do
  lift (putStrLn "0" >> pure 1)
    -- >>= \a -> ContT (\wcc ->  runContT (callCC (\cc -> cc ())) wcc)
    >>= \a ->
      (trace ("a = " <> show a) ContT (\wcc -> runContT (otherThread) wcc))
        >>= \b ->
          (trace ("b = " <> show b) lift $ pure ())
            >>= \c -> trace ("c = " <> show c) lift $ pure ()
 where
  -- >>= \() -> trace ("c = " <> show ()) lift $ pure ()

  otherThread :: ContT () IO ()
  otherThread = do
    lift $ print "other-thread"


testC0 =
  runContT c $ \c1 -> putStrLn $ "end: " <> show c1

a :: CoroutineT IO ()
a = do
  lpsLnC "a1-"
  yield
  lpsLnC "a2-"
  yield
  lpsLnC "a3-"

b :: CoroutineT IO ()
b = do
  lpsLnC "-b1"
  yield
  -- lpsLnC "-b2"


yield :: Monad m => CoroutineT m ()
yield = CoroutineT $ do
    otherCoro <- ask
    ContT $ \cc -> let 
        r = runContT (runCoroutineT otherCoro)
        self = CoroutineT $ lift $ cc ()
        cur = r pure
      in 
        local (const self) cur

runCoroutines :: Monad m => CoroutineT m () -> CoroutineT m () -> m ()
runCoroutines c1 c2 = do
  lastC <- runReaderT (do 
          runContT (runCoroutineT c1) pure
          l <- ask
          pure l
            ) c2
  runReaderT (runContT (runCoroutineT lastC) pure) (forever yield)
  -- >> runReaderT (runContT (runCoroutineT c2) pure) c1

testAb = runCoroutines b a

testStep = runCoroutines coroutine3 coroutine4

lpsLnC :: String -> CoroutineT IO ()
lpsLnC = lift . putStrLn

lpsLn :: String -> ContT r IO ()
lpsLn = lift . putStrLn

-- zer = const $ pure ()

-- testAB = runContT a pure

-- *** Experiments

mCallCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
mCallCC f =
  ContT $ \cc -> runContT (f (\x -> ContT (\_ -> cc x))) cc

-- how the hell this works?

{- | https://stackoverflow.com/questions/20536700/understanding-haskell-callcc-examples
 | endless print of "A" and "B"
-}
endlessLoop :: ContT () IO ()
endlessLoop = do
  -- Store the continuation for later use
  saved_cc <- callCC $ \k ->
    let f = k f
     in return f

  lift $ putStrLn "A"
  lift $ putStrLn "B"
  saved_cc
  lift $ putStrLn "C" -- never printed

data TagA
data TagB
data TagC

class T a b
instance T TagB TagA

--

data Foo a where
  FooA :: Int -> Foo TagA
  FooB :: Int -> Foo TagB
  FooC :: Int -> Foo TagC

-- just to save some typing and not have to destructure all possibilities in the instances
project :: Foo a -> Int
project (FooA i) = i
project (FooB i) = i
project (FooC i) = i

--

class ListOf a b x | a b -> x where
  listOf :: Foo a -> Foo b -> [Foo x]

instance {-# OVERLAPPING #-} T a b => ListOf a b a where
  listOf i@(FooA _) j = [i, FooA (project j)]
  listOf i@(FooB _) j = [i, FooB (project j)]
  listOf i@(FooC _) j = [i, FooC (project j)]

instance {-# OVERLAPPABLE #-} ListOf a b b where
  listOf i j@(FooA _) = [FooA (project i), j]
  listOf i j@(FooB _) = [FooB (project i), j]
  listOf i j@(FooC _) = [FooC (project i), j]

--

-- TagB and TagA are related by T
ex1 :: [Foo TagB]
ex1 = listOf (FooB 1) (FooA 2)

-- TagB and TagC are not
ex2 :: [Foo TagC]
ex2 = listOf (FooB 1) (FooC 2)