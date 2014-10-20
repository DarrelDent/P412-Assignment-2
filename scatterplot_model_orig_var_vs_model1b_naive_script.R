plot(fitted(model.orig.variables) ~ fitted(model1b.naive))

abline(0, 1)

cor(fitted(model.orig.variables), fitted(model1b.naive))