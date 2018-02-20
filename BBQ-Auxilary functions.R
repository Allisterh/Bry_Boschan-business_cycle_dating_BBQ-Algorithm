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

