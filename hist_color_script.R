with(tmsalary, {
  hist(color, freq=TRUE, ylab="Frequency", col=28)
  lines(density(color), lwd=2)
  rug(color)
  box()
})