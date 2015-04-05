module Breakfast where


type OatmealTemp = Int
-- interp. possible oatmeal temperature

minTemp, maxTemp, prefTemp :: OatmealTemp
minTemp = 0
maxTemp = 20
prefTemp = 10

data Adjustment = TurnLeft | TurnRight | LeftAsIs deriving (Show, Eq, Ord)
-- interp. representation of the stove adjustment

oatmeal_temp_to_adjustment :: OatmealTemp -> Adjustment
oatmeal_temp_to_adjustment temp = if temp < prefTemp
									then TurnRight
									else if temp > prefTemp
											then TurnLeft
											else LeftAsIs
