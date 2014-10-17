with(tmsalary, {
  hist(carat, freq=TRUE, ylab="Frequency", col=28)
  lines(density(carat), lwd=2)
  rug(carat)
  box()
})