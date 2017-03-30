module Lib where

import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle

data Pendulum = Pendulum { mass   :: Float
                         , length :: Float
                         , theta  :: Float
                         , omega  :: Float } deriving (Show, Eq)

data DoublePendulumSystem = DPS { pendulum1 :: Pendulum
                                , pendulum2 :: Pendulum
                                , g         :: Float
                                , t         :: Float
                                , dt        :: Float } deriving (Show, Eq)

derivatives :: DoublePendulumSystem -> [Float]
derivatives (DPS (Pendulum m1 l1 theta1 omega1) (Pendulum m2 l2 theta2 omega2) g _ _) = [theta1', omega1', theta2', omega2']
    where 
        theta1' = omega1
        theta2' = omega2
        delta = theta2 - theta1
        totalMass = m2 + m1
        den1 = totalMass * l1 - m2 * l1 * cos delta * cos delta
        den2 = den1 * l2 / l1
        omega1' = (m2 * l1 * omega1 * omega1 * sin delta * cos delta 
                + m2 * g * sin theta2 * cos delta
                + m2 * l2 * omega2 * omega2 * sin delta
                - totalMass * g * sin theta1) 
                / den1
        omega2' = (- m2 * l2 * omega2 * omega2 * sin delta * cos delta
                + totalMass * g * sin theta1 * cos delta 
                - totalMass * l1 * omega1 * omega1 * sin delta 
                - totalMass * g * sin theta2) 
                / den2

rk4PendulumSolver :: DoublePendulumSystem -> DoublePendulumSystem
rk4PendulumSolver ps = mkps yo
    where
        (DPS (Pendulum m1 l1 theta1 omega1) (Pendulum m2 l2 theta2 omega2) g t dt) = ps
        mkps [a,b,c,d] = DPS (Pendulum m1 l1 (normalizeAngle a) b) (Pendulum m2 l2 (normalizeAngle c) d) g (t + dt) dt
        yi = [theta1, omega1, theta2, omega2]
        k1 = map (* dt) (derivatives ps)
        k2 = map (* dt) (derivatives (mkps $ zipWith (+) yi (map (* 0.5) k1)))
        k3 = map (* dt) (derivatives (mkps $ zipWith (+) yi (map (* 0.5) k2)))
        k4 = map (* dt) (derivatives (mkps $ zipWith (+) yi k3))
        yo = zipWith (+) yi 
            (zipWith (+) (map (/ 6) k1) 
            (zipWith (+) (map (/ 3) k2) 
            (zipWith (+) (map (/ 3) k3) (map (/ 6) k4))))             

dpsdrawer :: DoublePendulumSystem -> Picture
dpsdrawer (DPS (Pendulum m1 l1 t1 _) (Pendulum m2 l2 t2 _) _ _ _) = 
    pictures [ line [(0, 0), (a, b)]
             , line [(a, b), (c, d)]
             , color red $ circleSolid 10
             , translate a b $ color blue $ circleSolid (5*m1)
             , translate c d $ color blue $ circleSolid (5*m2) ]
    where
        n = (+) (3*pi/2)
        a = cos (n t1) * l1 * 100
        b = sin (n t1) * l1 * 100
        c = a + (cos (n t2) * l2) * 100
        d = b + (sin (n t2) * l2) * 100

fps :: Int
fps = 200

anim [m1, l1, t1, o1, m2, l2, t2, o2] = 
    simulate (InWindow "DPS" (ceiling $ (l1 + l2) * 220, ceiling $ (l1 + l2) * 220) (10, 10)) 
             white 
             fps 
             (DPS (Pendulum m1 l1 t1 o1) (Pendulum m2 l2 t2 o2) 9.81 0 0.005) 
             dpsdrawer 
             (\_ _ -> rk4PendulumSolver)
