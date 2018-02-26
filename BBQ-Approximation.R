source(paste0("C:\\Users\\Misha\\Documents\\BBQ-Algorithm\\",
              "BBQ-Auxilary functions.R"))

library(PerformanceAnalytics)

gdp = import.gdp.data(paste0("C:\\Users\\Misha\\Documents\\",
                             "BBQ-Algorithm\\GDP.csv"))

peaks = rollapply(gdp, width = 5, FUN = get.peaks.bbq.approx,
                            align = "center")

troughs = rollapply(gdp, width = 5, FUN = get.troughs.bbq.approx,
                  align = "center") * (-1)

df = merge.xts(peaks,troughs)

df = df[complete.cases(df),]

df = add.cycle.state(df)

plot(df$State)
