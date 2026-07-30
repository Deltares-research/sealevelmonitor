
# TS-diagram

# install.packages("marelac")
# install.packages("plot3D")

library(marelac)
library(plot3D)

plotST <- function(fun, title)
  {
    Sal <- seq(0, 40, by = 0.5)
    Temp <- seq(-5, 40, by = 0.5)
    
      Val <- outer(X = Sal, Y = Temp, FUN = function(X, Y) fun(S = X, t = Y))
      contour(Sal, Temp, Val, xlab = "Salinity", ylab = "temperature",
                main = title, nlevel = 20)
      }

plotST(sw_dens, "Density")
points(x = 35, y = 20)
lines(x = c(10,30,30,10,10), y = c(10,10,20,20,10), col = 4, lty = 3, lwd = 4)

par (mfrow = c(1, 1))

# or:

Sal <- seq(0, 40, by = 0.5)
Temp <- seq(-5, 40, by = 0.5)

Val <- outer(X = Sal, Y = Temp, FUN = function(X, Y) sw_dens(S = X, t = Y))

ggplot(x = )

