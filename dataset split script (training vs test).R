# Sample Indexes
indexes <- sample(1:nrow(tmsalary), size = 0.3 * nrow(tmsalary))

test.tmsalary <- tmsalary[indexes,]
train.tmsalary <- tmsalary[-indexes,]