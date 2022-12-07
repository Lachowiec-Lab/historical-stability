### Load env ###
renv::load()

### Load packages ###
library(zoo)
library(dplyr)
library(raster)

########################################
#####Preparing Monthly Weather Data#####
########################################

#combining data from multiple weather stations
#removing duplicates and adding empty rows 

##create vector with all dates--January 1949-December 2019 for merging
dates <- as.Date(c("1949-01-01", "2019-12-01"), "%Y-%m-%d")
month_seq <- as.character(seq.Date(from = dates[1], to = dates[2], by = "month"))
DATE <- data.frame(DATE = substr(month_seq,1,nchar(month_seq)-3))

##preparing Kalispell data#####
kaliDraft <- read.csv("location_weather/Kalispell_NOAA.csv")
head(kaliDraft)
kaliW <- merge(DATE, kaliDraft, by = "DATE", all.x = TRUE)

##preparing Sidney data#####
sidDraft <- read.csv("location_weather/Sidney_NOAA.csv")
head(sidDraft)
sidW <- merge(DATE, sidDraft, by = "DATE", all.x = TRUE)

##preparing Moccasin data#####
mocDraft <- read.csv("location_weather/Moccasin_NOAA.csv")
head(mocDraft)
mocW <- merge(DATE, mocDraft, by = "DATE", all.x = TRUE)

##preparing Huntley data#####
hunDraft <- read.csv("location_weather/Huntley_NOAA.csv")
head(hunDraft)
hunW <- merge(DATE, hunDraft, by = "DATE", all.x = TRUE)

####preparing Havre data########
havDraft <- read.csv("location_weather/Havre_NOAA.csv")
head(havDraft)
##removing station USC00243990 and line 146 because duplicates
havDraft <- havDraft[!havDraft$STATION == "USC00243990",]
havW <- havDraft[-146,]

#####Preparing Bozeman data#####
###have data from two stations to combine
bozDraft <- read.csv("location_weather/Bozeman_NOAA.csv")
temp <- bozDraft[with(bozDraft, order(NAME)),]
temp2 <- temp[!duplicated(temp$DATE),]
bozW <- merge(DATE, temp2, by = "DATE", all.x = TRUE)

#######################
#####Figure S4 e-h#####
#######################

####Aggregating monthly values over years to compare across locations
monthly_means <- function(data) {
  data$MONTH <- substring(data$DATE, first = 6, last = 7)
  agMean <- aggregate((TAVG-32)*(5/9) ~ MONTH, data = data, FUN = mean, na.rm =T)
  agMax <- aggregate((TMAX-32)*(5/9) ~ MONTH, data = data, FUN = mean, na.rm =T)
  agMin <- aggregate((TMIN-32)*(5/9) ~ MONTH, data = data, FUN = mean, na.rm =T)
  agPre <- aggregate(PRCP*2.54 ~ MONTH, data = data, FUN = mean, na.rm =T)
  return(list(agMean, agMax, agMin, agPre))
}

data_list <- list(havW, sidW, hunW, bozW, mocW, kaliW)
abbrev <- c("hav", "sid", "hun", "boz", "moc", "kal")

for (i in (1:length(abbrev))) {
 name <- paste(abbrev[i], "Aggs", sep = "")
 assign(name, monthly_means(data_list[[i]]))
}

plot_line_aggs <- function(data_list, metric_num, ylim, ylab, xlab) {
  plot(1, type="l", ylab=ylab, xlim=c(1, 12), ylim=ylim, xlab=xlab)
  colors <- c("red", "magenta", "orange", "green", "blue", "turquoise")
  for(i in (1:length(data_list))){
    lines(unlist(data_list[[1]][[1]][1]), unlist(data_list[[i]][[metric_num]][2]), col = colors[i])
  }
}

data_list <- list(havAggs, sidAggs, hunAggs, bozAggs, mocAggs, kalAggs)

par(mfrow = c(2,2))
par(mar = c(4,6,1,1))
par(bty = 'n') 
plot_line_aggs(data_list = data_list, metric_num = 1, ylim = c(-10, 25), ylab = "Mean\n temperature (°C)", xlab = "")
plot_line_aggs(data_list = data_list, metric_num = 2, ylim = c(-10, 40), ylab = "Maximum\n temperature (°C)", xlab = "")
legend(x = 5.5, y = 14, cex = 0.8, legend = c("Havre", "Sidney","Huntley","Bozeman","Moccasin","Kalispell"), fill = c("red", "magenta", "orange","green",  "turquoise", "blue"))
plot_line_aggs(data_list = data_list, metric_num = 3, ylim = c(-20, 15), ylab = "Minimum\n temperature (°C)", xlab = "Month")
plot_line_aggs(data_list = data_list, metric_num = 4, ylim = c(0,8), ylab = "Total Precipitation (cm)", xlab = "Month")

####################################################################
#######Figure 4 a-d month CV over time--location variability########
####################################################################

####Calculating CV for each month to compare across locations#####
monthly_CVs <- function(data) {
  data$MONTH <- substring(data$DATE, first = 6, last = 7)
  agMean <- aggregate(TAVG+30 ~ MONTH, data = data, FUN = cv) ##adding 30 to avoid negative values
  agMax <- aggregate(TMAX+30 ~ MONTH, data = data, FUN = cv)
  agMin <- aggregate(TMIN+30 ~ MONTH, data = data, FUN = cv)
  agPre <- aggregate(PRCP ~ MONTH, data = data, FUN = cv)
  return(list(agMean, agMax, agMin, agPre))
}

data_list <- list(havW, sidW, hunW, bozW, mocW, kaliW)
abbrev <- c("hav", "sid", "hun", "boz", "moc", "kal")

for (i in (1:length(abbrev))) {
  name <- paste(abbrev[i], "monthlyCVs", sep = "")
  assign(name, monthly_CVs(data_list[[i]]))
}

plot_line_CVs <- function(data_list, metric_num, ylim, ylab, xlab) {
  plot(1, type="l", ylab=ylab, xlim=c(1, 12), ylim=ylim, xlab=xlab,
       cex.lab = 1.5, cex.axis = 1.5, cex = 1.3)
  colors <- c("red", "magenta", "orange", "green", "blue", "turquoise")
  line_type <- c(1,3,4,2,6,5)
  line_width <- c(0.8, 0.8, 0.8, 1.5, 0.8, 0.8)
    for(i in (1:length(data_list))){
    lines(unlist(data_list[[1]][[1]][1]), unlist(data_list[[i]][[metric_num]][2])/100, 
          col = colors[i], lty = line_type[i], lwd = line_width[i])
  }
}

data_list <- list(havmonthlyCVs, sidmonthlyCVs, hunmonthlyCVs, bozmonthlyCVs, mocmonthlyCVs, kalmonthlyCVs)

par(mfrow = c(2,2))
par(bty = 'n')
par(mar = c(5, 6, 1, 1))

plot_line_CVs(data_list = data_list, metric_num = 1, ylim = c(0,0.25), ylab = "CV Mean\ntemperature", xlab = "")
plot_line_CVs(data_list = data_list, metric_num = 2, ylim = c(0,0.25), ylab = "CV Maximum\ntemperature", xlab = "")
legend(x = 5, y = .25, cex = .8, legend = c("Havre", "Sidney","Huntley","Bozeman","Moccasin","Kalispell"), 
       col = c("red", "magenta", "orange","green",  "turquoise", "blue"), lty = c(1,3,4,2,6,5), 
       lwd = c(0.8, 0.8, 0.8, 1.5, 0.8, 0.8))
plot_line_CVs(data_list = data_list, metric_num = 3, ylim = c(0,0.30), ylab = "CV Minimum\ntemperature", xlab = "Month")
plot_line_CVs(data_list = data_list, metric_num = 4, ylim = c(0,1.2), ylab = "CV Total\nPrecipitation", xlab = "Month")



