build.lm.model <- lm(price ~ carat + color + clarity + 
                              cut + channel + store, 
                            data = train.tmsalary)