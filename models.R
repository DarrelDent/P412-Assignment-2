# 1st model tried: price by carat + store (strongest correlations, pos & neg)
model.carat.store <- lm(tmx$price ~ tmx$carat + tmx$store_num)

# Take a step back and model all variables to see what it looks like
model.all.variables <- lm(tmx$price ~ tmx$carat + tmx$color + tmx$clarity + 
                            tmx$cut_num + tmx$cha_num + tmx$store_num)