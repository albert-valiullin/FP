module Dinner where


data DinnerOrder = Chicken | Pasta | False
-- interp. possible passanger's dinner order

msgTemp = "The passenger ordered "

dinner_order_to_msg :: DinnerOrder -> String
dinner_order_to_msg order = case order of
								Chicken		  -> msgTemp ++ "chicken."
								Pasta		  -> msgTemp ++ "pasta."
								Dinner.False  -> msgTemp ++ "nothing."