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

library(PerformanceAnalytics)

gdp = import.gdp.data(paste0("C:\\Users\\Misha\\Documents\\",
                             "BBQ-Algorithm\\GDP.csv"))


# 1. Identify a set of potential peaks and troughs using turning point (K rule)

df = cbind.xts(gdp,rollapply(gdp, width = 5, FUN = identify.turning.point,
                             align = "center"))

names(df) = c("GDP","TP")

# 2. Enforce alternating points by selecting highest (resp. lowest)
# consecutive peaks (resp. troughs).

alt.peaks = get.alternating.peaks(peaks = df[df$TP == 1,],
                                  troughs = df[df$TP == -1,],
                                  timeframe = index(df))

alt.troughs = get.alternating.troughs(peaks = df[df$TP == 1,],
                                      troughs = df[df$TP == -1,],
                                      timeframe = index(df))

cycle_date = paste(as.Date(index(alt.peaks)),
                   as.Date(index(alt.troughs)),sep = "/")

chart.TimeSeries(df$GDP * 10 ^ (-3),
                 main = "Business Cycle - Recessions\n (quarterly GDP)",
                 xaxis = FALSE,ylab = "Billions ILS",
                 period.areas = cycle_date,period.color = "lightblue")

axis(side = 1,at = seq_along(index(df)),labels = index(df))

#--------------------------------------------------------------------------------

# 3. Implement censoring rules : Censor insufficient phase length

phase.censor_df = get.phase.censored.points (peaks = alt.peaks, troughs = alt.troughs,
                                       min_phase_length = 2)

cycle_date = paste(as.Date(index(phase.censor_df$censored_peaks)),
                   as.Date(index(phase.censor_df$censored_troughs)),sep = "/")

chart.TimeSeries(df$GDP * 10 ^ (-3),
                 main = "Business Cycle - Phase censored Recessions\n (quarterly GDP)",
                 xaxis = FALSE,ylab = "Billions ILS",
                 period.areas = cycle_date,period.color = "lightblue")

axis(side = 1,at = seq_along(index(df)),labels = index(df))

#------------------------------------------------------------------------------

cycle.censor_df = get.cycle.censored.points(peaks = phase.censor_df$censored_peaks,
                                             troughs = phase.censor_df$censored_troughs,
                                             min_cycle_length = 5)

cycle_date = paste(as.Date(index(cycle.censor_df$censored_peaks)),
                   as.Date(index(cycle.censor_df$censored_troughs)),sep = "/")

chart.TimeSeries(df$GDP * 10 ^ (-3),
                 main = "Business Cycle - Cycle censored Recessions\n (quarterly GDP)",
                 xaxis = FALSE,ylab = "Billions ILS",
                 period.areas = cycle_date,period.color = "lightblue")

axis(side = 1,at = seq_along(index(df)),labels = index(df))

