module Lib where

import Graphics.Gloss

data Pendulum = Pendulum { mass   :: Float
                         , length :: Float
                         , theta  :: Float
                         , omega  :: Float
                         , oldPos :: (Float, Float)
                         , path   :: [Picture] } deriving (Show, Eq)

data DoublePendulumSystem = DPS { pendulum1 :: Pendulum
                                , pendulum2 :: Pendulum
                                , g         :: Float
                                , dt        :: Float } deriving (Show, Eq)

derivatives :: DoublePendulumSystem -> [Float]
derivatives (DPS (Pendulum m1 l1 theta1 omega1 _ _) (Pendulum m2 l2 theta2 omega2 _ _) g _) = [theta1', omega1', theta2', omega2']
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
        (DPS (Pendulum m1 l1 theta1 omega1 (ox1, oy1) p1) (Pendulum m2 l2 theta2 omega2 (ox2, oy2) p2) g dt) = ps
        mkps [a,b,c,d] = DPS (Pendulum m1 l1 a b (nx1, ny1) np1) (Pendulum m2 l2 c d (nx2, ny2) np2) g dt
        yi = [theta1, omega1, theta2, omega2]
        k1 = map (* dt) (derivatives ps)
        k2 = map (* dt) (derivatives (mkps $ zipWith (+) yi (map (* 0.5) k1)))
        k3 = map (* dt) (derivatives (mkps $ zipWith (+) yi (map (* 0.5) k2)))
        k4 = map (* dt) (derivatives (mkps $ zipWith (+) yi k3))
        yo = zipWith (+) yi 
            (zipWith (+) (map (/ 6) k1) 
            (zipWith (+) (map (/ 3) k2) 
            (zipWith (+) (map (/ 3) k3) (map (/ 6) k4))))             
        n = (+) (3*pi/2)
        nx1 = cos (n theta1) * l1 * 100
        ny1 = sin (n theta1) * l1 * 100
        nx2 = nx1 + (cos (n theta2) * l2) * 100
        ny2 = ny1 + (sin (n theta2) * l2) * 100
        np1 = color cyan (line [(ox1, oy1), (nx1, ny1)]) : take 100 p1
        np2 = color magenta (line [(ox2, oy2), (nx2, ny2)]) : take 100 p2

dpsdrawer :: DoublePendulumSystem -> Picture
dpsdrawer (DPS (Pendulum m1 l1 t1 _ (a, b) p1) (Pendulum m2 l2 t2 _ (c, d) p2) _ _) = 
    pictures (p1 ++ p2 ++ [ color white $ line [(0, 0), (a, b)]
                          , color white $ line [(a, b), (c, d)]
                          , color yellow $ circleSolid 3
                          , translate a b $ color orange $ circleSolid (5*m1)
                          , translate c d $ color orange $ circleSolid (5*m2) ])

anim [m1, l1, t1, o1, m2, l2, t2, o2, g] = 
    simulate (InWindow "DPS" (ceiling $ (l1 + l2) * 220, ceiling $ (l1 + l2) * 220) (10, 10)) 
             black 
             200 
             (DPS (Pendulum m1 l1 t1 o1 (nx1,ny1) []) (Pendulum m2 l2 t2 o2 (nx2,ny2) []) g (1/200)) 
             dpsdrawer 
             (\_ _ -> rk4PendulumSolver)
    where
        n = (+) (3*pi/2)
        nx1 = cos (n t1) * l1 * 100
        ny1 = sin (n t1) * l1 * 100
        nx2 = nx1 + (cos (n t2) * l2) * 100
        ny2 = ny1 + (sin (n t2) * l2) * 100      