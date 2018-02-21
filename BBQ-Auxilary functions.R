import.gdp.data = function(filepath){
  
  gdp = read.csv(filepath)
  
  gdp$Date = gsub(pattern = "Q",replacement = "",
                  x = levels(gdp$Date)[gdp$Date],fixed = TRUE)
  
  gdp$Date = as.yearqtr(gdp$Date, format = "%Y-%q")
  
  gdp = xts(x = gdp$GDP, order.by = gdp$Date)
  
  return(gdp)
  
}

identify.turning.point = function(window){
  
  pos = ceiling(length(window) / 2)
  
  if (window[pos] == max(window)) {
    
    return(1)
    
  } else if (window[pos] == min(window)) {
    
    return(-1)
    
  } else {
    
    return(0)
  }
  
}

get.max.peak = function(peaks,start_period,end_period){
  
  # browser()
  
  sub_peaks = peaks[index(peaks) >= start_period & index(peaks) <= end_period,]
  
  if (length(sub_peaks) > 0) {
    
    return(sub_peaks[sub_peaks[,1] == max(sub_peaks[,1]) ,])
    
  }
}

get.alternating.peaks = function(peaks, troughs, timeframe){
  
  start_points = index(troughs)
  
  end_points = c(index(troughs)[-1],timeframe[length(timeframe)])
  
  points = cbind.data.frame(start_points,
                       end_points)
  
  names(points) = c("Start_Point","End_Point")
  
  points = apply(points, 1, list)
  
  alt.peaks = sapply(points,
         function(Z,peaks){return(get.max.peak(start_period = Z[[1]][1],
                                         end_period = Z[[1]][2],
                                         peaks = peaks))},
         peaks = peaks)
  
  alt.peaks = do.call(rbind.xts,alt.peaks)
  
}


