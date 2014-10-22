build.lm.model.inter <- lm(price ~ carat + color + clarity + 
                              cut + channel + store + store:clarity, 
                            data = train.tmsalary)