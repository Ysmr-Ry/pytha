{-# LANGUAGE PackageImports #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Lib hiding (State)
import Haste.Events
import Haste.Graphics.Canvas hiding (Point)
import Data.Complex
import Lens.Micro
import Lens.Micro.GHC
import Lens.Micro.Mtl

type Point = Complex Double
type Time = Int
type Flipped = Bool
type Area = Double

type State = ((Time, Flipped, Angle, Angle), (Area, (Point, Point, Point)))

mag :: Num a => a
mag = 50

timL :: Lens' State Time
timL = _1._1
flipL :: Lens' State Flipped
flipL = _1._2
angL :: Lens' State Angle
angL = _1._3
toAngL :: Lens' State Angle
toAngL = _1._4
areaL :: Lens' State Area
areaL = _2._1
triL :: Lens' State (Point, Point, Point)
triL = _2._2

a, b, c :: Double
a = 3
b = 4
c = 5

main :: IO ()
main = do
  start
  getCanvasById "canvas" >>= \case
    Nothing -> error "Canvas not found!"
    Just cvs -> concurrent $ launch cvs

launch :: Canvas -> CIO ()
launch cvs = do
  let
    p1 = (mag:+0)*((-b*b/c):+(-a*b/c/2))
    p2 = (mag:+0)*(a*a/c:+(-a*b/c/2))
    p3 = (mag:+0)*(0:+a*b/c/2)
    d = ((p1+p2)/(2:+0))+mag*(0:+a*b/c/2)
  state <- newMVar ((0,False,0,0),(c*c,(p1-d,p2-d,p3-d)))
  onFrame state $ renderer cvs
  stepR <- spawn $ process state
  spawn $ timer 16 () stepR
  return ()

process :: MVar State -> Inbox () -> CIO ()
process v tick = forever $ receive tick >>= \_ -> withMVar v $ do
  timL += 1

  ang <- use angL
  toAng <- use toAngL

  when (abs (toAng-ang) < 0.01) $ do
    areaL -= 0.1

  flipped <- use flipL
  angL %= (\f -> morph f toAng 10) 

  area <- use areaL

  when (area <= 0) $ do
    flipL %= not
    areaL .= c*c
    toAngL += pi

tup :: Point -> (Double, Double)
tup (x:+y) = (x,y)

cross :: Point -> Point -> Double
cross p q = imagPart $ p*conjugate q

closePath :: [a] -> [a]
closePath xs@(x:_) = xs ++ [x]

renderer :: Canvas -> State -> IO ()
renderer cvs state = do
  (w,h) <- screenSize

  let 
    (p1,p2,p3) = state ^. triL
    flipped = state ^. flipL
    area = state ^. areaL
    rest = c*c-area
    ang = state ^. angL
    pts = [p1,p2,p3]
    mkRect :: Point -> Point -> Point -> [Point]
    mkRect v q1 q2 = [q1, q1+v, q2+v, q2]
    angB = acos (a/c)
    rect1 = mkRect (0:+(-mag*c)) p1 p2
    rect2 = mkRect (mkPolar (mag*a) (0.5*pi-angB)) p2 p3
    rect3 = mkRect (mkPolar (mag*b) (pi-angB)) p3 p1
    ptsR = map (*cis ang) pts
    org = (w/2,h/2)
    gray = RGB 40 40 40
    ocean = RGB 0 128 255
    calcPoly pa@(_:+paY') v@(x:+y) s
      | s <= s1 = [pc,pc+w h1,pc+vw h1]
      | s <= s2 = [pc, pc-v, pc-v+w (h2-pdY), pc+w h2]
      | otherwise = [pc, pb, pa-vw (paY-h3), pa-w (paY-h3), pd]
      where
        u = (-y):+x
        pb@(_:+pbY') = pa+v
        pc@(_:+pcY') = pb+u
        pd@(_:+pdY') = pc-v
        paY = pcY'-paY'
        pbY = pcY'-pbY'
        pdY = pcY'-pdY'
        minY = pbY `min` pdY
        maxY = pbY `max` pdY
        w height = (-signum u)*(height/abs (sin $ phase $ -u):+0)
        vw height = (-signum v)*(height/abs (sin $ phase $ -v):+0)
        s1 = abs ((-v)`cross`w minY)/2
        h1 = minY*sqrt (s/s1)
        s2 = s1+magnitude (v+w minY)*(maxY-minY)
        h2 = (s-s1)/magnitude (v+w minY)+minY
        h3 = paY-sqrt (abs (v`cross`w (paY-pbY))/2-(s-s2))

  render cvs $ translate org $ do
    let
      waterH = area/c
      col = if flipped then RGB 255 255 255 else ocean
    
    when flipped $ do
      color ocean $ fill $ path $ map (tup . (*cis ang)) rect1
      color ocean $ fill $ path $ map (tup . (*cis ang)) rect2
      color ocean $ fill $ path $ map (tup . (*cis ang)) rect3  

    color col $ fill $ path $ map (tup . (*cis ang)) $ mkRect (0:+(-mag*waterH)) p1 p2
    color col $ fill $ path $ map (tup . (*cis ang)) $ calcPoly p2 (mkPolar (mag*a) (0.5*pi-angB)) (mag*mag*rest*(a/c)**2)  
    color col $ fill $ path $ map (tup . (*cis ang)) $ calcPoly p1 (mkPolar (mag*b) (0.5*pi-angB)) (mag*mag*rest*(b/c)**2)

    color gray $ lineWidth 2 $ stroke $ path $ closePath $ map tup ptsR
    color gray $ lineWidth 2 $ stroke $ path $ map (tup . (*cis ang)) rect1
    color gray $ lineWidth 2 $ stroke $ path $ map (tup . (*cis ang)) rect2
    color gray $ lineWidth 2 $ stroke $ path $ map (tup . (*cis ang)) rect3

  return ()