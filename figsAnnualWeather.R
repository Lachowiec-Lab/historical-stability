library(raster)

###annual weather data at all locations studied
ann <- read.csv("https://github.com/Lachowiec-Lab/historical-stability/tree/main/location_weather/annualNOAAallLocations.csv")
locations <- c("Havre", "Sidney", "Huntley", "Bozeman", "Moccasin", "Kalispell")
summary(ann) ###missing data observed

##########################################################
###Figure 4e-h Moving CV analysis over annual weather#####
##########################################################
movCV <- function(x, n) {  #n is size of the moving window
  {
    stopifnot(is.numeric(x), is.numeric(n))
    if (length(n) != 1 || ceiling(n != floor(n)) || n <= 1) 
      stop("Window length 'n' must be a single integer greater 1.")
    nx <- length(x)
    if (n >= nx) 
      stop("Window length 'n' must be greater then length of time series.")
    y <- numeric(nx)
    for (k in 1:(n - 1)) y[k] <- raster::cv(x[1:k], na.rm = T)/100
    for (k in n:nx) y[k] <- raster::cv(x[(k - n + 1):k], na.rm = T)/100
    return(y)
  }
}

plot_movCV <- function(data, locations, metric_num, ylim, ylab, xlab) {
  plot(1, type="l", ylab=ylab, xlim=c(1964, 2004), ylim=ylim, xlab=xlab,
       cex.lab = 1.5, cex.axis = 1.5, cex = 1.3)
  colors <- c("red", "magenta", "orange", "green", "blue", "turquoise")
  line_type <- c(1,3,4,2,6,5)
  line_width <- c(0.8, 0.8, 0.8, 1.5, 0.8, 0.8)
  
  for(i in (1:length(locations))){
    lines(y = movCV(data[data$Location == locations[i], metric_num], 30)[15:55], x = c(1964:2004), 
          col = colors[i], lwd = line_width[i], lty = line_type[i])
  }
}

par(mfrow = c(2,2))
par(mar = c(5,7,3,1))
plot_movCV(data=ann, locations = locations,  metric_num = 16, 
           ylim = c(0.01,0.08), ylab = "Annual Temperature\n Mean Moving CV", xlab = "Year")
plot_movCV(data=ann, locations = locations,  metric_num = 17, 
           ylim = c(0.01,0.08), ylab = "Annual Temperature\n Maximum Moving CV", xlab = "Year")
plot_movCV(data=ann, locations = locations,  metric_num = 18, 
           ylim = c(0.01,0.08), ylab = "Annual Temperature\n Minimum Moving CV", xlab = "Year")
plot_movCV(data=ann, locations = locations,  metric_num = 14, 
           ylim = c(0.01,0.4), ylab = "Annual Accumulated\n Precipitation Moving CV", xlab = "Year")



##########################################################
#####Supp Figure 4a-d Average weather over time ##########
##########################################################
plot_line <- function(data, locations, metric_num, ylim, ylab, xlab) {
  plot(1, type="l", ylab=ylab, xlim=c(1949, 2019), ylim=ylim, xlab=xlab,
       cex.lab = 1.5, cex.axis = 1.5, cex = 1.3)
  colors <- c("red", "magenta", "orange", "green", "blue", "turquoise")
  for(i in (1:length(locations))){
    lines(data$DATE[data$Location == locations[i]], y = data[data$Location == locations[i], metric_num], 
          col = colors[i])
    abline(lm(data[data$Location == locations[i], metric_num] ~ data$DATE[data$Location == locations[i]]), col = colors[i])
    
  }
}

par(mfrow = c(2,2))
par(mar = c(5,5,1,1))
par(bty = 'n') 
plot_line(data = ann, locations = locations, metric_num = 16, ylim = c(20,60), 
          ylab = "Annual average\ntemperature (°F)", xlab = "")
legend(x = 1998, y = 38, cex = .8, legend = locations, fill = c("red", "magenta", "orange","green",  "turquoise", "blue"))
plot_line(data = ann, locations = locations, metric_num = 17, ylim = c(40,70), 
          ylab = "Annual average\nmaximum temperature (°F)", xlab = "")
plot_line(data = ann, locations = locations, metric_num = 18, ylim = c(20,40), 
          ylab = "Annual average\nminimum temperature (°F)", xlab = "")
plot_line(data = ann, locations = locations, metric_num = 14, ylim = c(0,30), 
          ylab = "Annual average accumulated\nprecipitation (in)", xlab = "")