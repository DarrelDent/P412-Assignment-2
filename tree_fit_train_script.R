tree.fit.train <- rpart(price ~ carat + color + clarity + cut + channel + store,
                  data = train.tmsalary)
plot(tree.fit.train)
text(tree.fit.train)