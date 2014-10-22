model.ran.forest <- randomForest(price ~ carat + color + clarity +
                                   cut + channel + store,
                                 data = test.tmsalary)
print(model.ran.forest)
importance(model.ran.forest)