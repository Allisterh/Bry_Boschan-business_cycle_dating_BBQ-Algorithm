source(paste0("C:\\Users\\Misha\\Documents\\BBQ-Algorithm\\",
              "BBQ-Auxilary functions.R"))

library(PerformanceAnalytics)

gdp = import.gdp.data(paste0("C:\\Users\\Misha\\Documents\\",
                             "BBQ-Algorithm\\GDP.csv"))

k = 5

peaks = rollapply(gdp, width = k, FUN = get.peaks.bbq.approx,
                            align = "center")

troughs = rollapply(gdp, width = k, FUN = get.troughs.bbq.approx,
                  align = "center") * (-1)

df = merge.xts(peaks,troughs)

df = df[complete.cases(df),]

df = merge.xts(gdp,add.cycle.state.bbq.approx(df),join = "inner")

# cycle_date = c("1999-01-01/2000-01-01")

cycle_date = as.Date(index(df)[df$State == 1])

start_dates = cycle_date[seq.int(from = 1,to = length(cycle_date)-1,by = 2)]

end_dates = cycle_date[seq.int(from = 2,to = length(cycle_date),by = 2)]

cycle_date = paste(start_dates,end_dates,sep = "/")

chart.TimeSeries(df$gdp,period.areas = cycle_date,
                 period.color = "lightblue",xaxis = FALSE)

axis(side = 1,at = seq_along(index(df)),labels = as.yearqtr(index(df)))
