tree.fit.test <- rpart(price ~ carat + color + clarity + cut + channel + store,
                  data = test.tmsalary)
plot(tree.fit.test)
text(tree.fit.test)