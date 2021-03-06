plot(fitted(model1.train) ~ fitted(model1fb.train), 
     xlab = "Model with All Variables",
     ylab = "Model with forward/backward-selected Variables")

abline(0, 1)

cor(fitted(model.orig.variables), fitted(model1b.naive))