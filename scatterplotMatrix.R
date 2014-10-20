scatterplotMatrix(~ price + carat + color + clarity + store, span = 0.7, 
                  data = tmsalary, lwd = 2, col = palette()[c(4,2,1)])