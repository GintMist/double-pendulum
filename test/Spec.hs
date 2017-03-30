import Lib
import Test.QuickCheck

instance Arbitrary DoublePendulumSystem where
    arbitrary = do
        m1 <- fmap ((+0.1).abs) arb
        m2 <- fmap ((+0.1).abs) arb
        l1 <- fmap ((+0.1).abs) arb
        l2 <- fmap ((+0.1).abs) arb
        g  <- arb
        t1 <- arb
        t2 <- arb
        o1 <- arb
        o2 <- arb        
        return (DPS (Pendulum m1 l1 t1 o1 (0,0) []) (Pendulum m2 l2 t2 o2 (0,0) []) g 0 0.005)
        where
            arb :: Gen Float
            arb = do
                x <- arbitrary
                if abs x > 5
                then arb
                else return x

mechEnergy :: DoublePendulumSystem -> Float
mechEnergy (DPS (Pendulum m1 l1 t1 o1 _ _) (Pendulum m2 l2 t2 o2 _ _) g _ _) = - (m1 + m2) * g * l1 * cos t1 - m2 * g * l2 * cos t2  
                                                                     + (1/2) * m1 * l1 * l1 * o1 * o1
                                                                     + (1/2) * m2 * (l1 * l1 * o1 * o1 + l2 * l2 * o2 * o2 + 2 * l1 * l2 * o1 * o2 * cos(t1 - t2))

conservationOfMechEnergy :: DoublePendulumSystem -> Bool
conservationOfMechEnergy dps = perc < 0.01
    where
        diff = mechEnergy dps - mechEnergy (rk4PendulumSolver dps)
        perc = abs (diff * 100 / (if mechEnergy (rk4PendulumSolver dps) == 0 then 1 else mechEnergy (rk4PendulumSolver dps)))

main :: IO ()
main = quickCheck conservationOfMechEnergy