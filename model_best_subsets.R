model.best.subsets <- regsubsets(price ~ carat + color + clarity + cut + channel + store, 
                                 data = train.tmsalary, 
                                 nbest = 1, 
                                 nvmax = 10, 
                                 method = "exhaustive")