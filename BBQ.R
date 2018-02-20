'The algoritnm is implemented in 3 stages
-----------------------------------------------------------------------------
1. Identify a set of potential peaks and troughs using turning point (K rule)

2. Enforce alternating points by selecting highest (resp. lowest)
   consecutive peaks (resp. troughs).

3. Implement censoring rules

-----------------------------------------------------------------------------
'

source(paste0("C:\\Users\\Misha\\Documents\\BBQ-Algorithm\\",
              "BBQ-Auxilary functions.R"))

library(xts)

library(PerformanceAnalytics)

gdp = import.gdp.data(paste0("C:\\Users\\Misha\\Documents\\",
                             "BBQ-Algorithm\\GDP.csv"))

# Identify a set of potential peaks and troughs

x = cbind.xts(gdp,rollapply(gdp, width = 5, FUN = identify.turning.point,
                        align = "center"))

names(x) = c("GDP","TP")

plot.xts(x$GDP, grid.col = "white", main = "GDP")

points(x$GDP[x$TP == 1], col = "green", pch = 20, cex = 2)

points(x$GDP[x$TP == -1], col = "red", pch = 20, cex = 2)

x[x$GDP == max(x$GDP["::2002"])]
