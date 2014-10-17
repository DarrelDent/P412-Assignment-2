with(tmsalary, {
  hist(price, freq=TRUE, ylab="Frequency", col=28)
  lines(density(price), lwd=2)
  rug(price)
  box()
})