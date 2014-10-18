tree.fit <- rpart(price ~ carat + color + clarity + cut + channel + store,
                  data = tmsalary)
plot(tree.fit)
text(tree.fit)