build.lm.model.test <- lm(price ~ carat + color + clarity + 
                              cut + channel + store, 
                            data = test.tmsalary)