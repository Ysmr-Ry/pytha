{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

module Lib (
  module Control.Monad.State.Strict,
  module Lens.Micro,
  module Lens.Micro.GHC,
  module Lens.Micro.Mtl,
  module Haste,
  module Haste.Concurrent,
  Z, R,
  screenSize, start, io,
  onFrame, timer, withMVar, handle,
  morph
) where

import Haste
import Haste.Prim
import Haste.DOM
import Haste.Events
import Haste.Foreign
import Haste.Graphics.AnimationFrame
import Haste.Graphics.Canvas
import Haste.Concurrent
import Control.Monad
import "mtl" Control.Monad.State.Strict
import Data.Complex
import Lens.Micro
import Lens.Micro.GHC
import Lens.Micro.Mtl

type Z = Integer
type R = Double

fi :: (Num b, Integral a) => a -> b
fi = fromIntegral

screenSize :: IO (R, R)
screenSize = (,) <$> w () <*> h () where
  w = ffi "(function(){return Util.width;})"
  h = ffi "(function(){return Util.height;})"

start :: IO ()
start = f () where
  f = ffi "(Util.onload)"
  
io :: MonadIO m => IO a -> m a
io = liftIO

onFrame :: MVar s -> (s -> IO ()) -> CIO ()
onFrame s h = io $ let
    proc _ = void $ do
      s' <- peekMVar s
      maybe (return ()) h s'
      requestAnimationFrame proc
  in proc undefined

withMVar :: MonadConc m => MVar a -> StateT a m b -> m b
withMVar v act = do
  x <- liftConc $ takeMVar v
  (res,x') <- runStateT act x
  liftConc $ putMVar v x'
  return res

timer :: Int -> t -> Outbox t -> Inbox a -> CIO ()
timer n t box _ = forever $ do
  box ! t
  wait n

handle :: Event evt => evt -> (EventData evt -> t) -> Outbox t -> Inbox a -> CIO ()
handle e h box _ = void $ onEvent document e ((box!) . h)

class Morphable a where
  morph :: a -> a -> Double -> a

instance Morphable Double where
  morph from to d = from+(to-from)/d
instance Morphable (Complex Double) where
  morph (fx:+fy) (tx:+ty) d = (fx+(tx-fx)/d):+(fy+(ty-fy)/d)
{-instance (Morphable a, Morphable b) => (a, b) where
  morph (f1,f2) (t1,t2) d = (morph f1 t1 d, morph f2 t2 d)
instance (Morphable a, Morphable b, Morphable c) => (a, b, c) where
  morph (f1,f2,f3) (t1,t2,t3) d = (morph f1 t1 d, morph f2 t2 d, morph f3 t3 d)-}