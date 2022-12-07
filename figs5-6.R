### Load env ###
renv::load()

### Load packages ###
library(tidyr)
library(readxl)
library(factoextra)
library(corrplot)
library(RColorBrewer)
library(ppcor)

#####################################
########### Functions ###############
#####################################

#### Merging weather and yields #####
merge_weather_yield <- function(weather_data, yield_data, location) {
  longData <- gather(yield_data, Year, Yield, `1949`:`2019`, factor_key=TRUE)
  #remove NA rows
  longData <- longData[!is.na(longData$Yield),]
  pshew <- merge(longData, ann[ann$Location == location,], by.x = "Year", by.y = "DATE")  
  pshew <- pshew %>% drop_na(TAVG, TMAX, TMIN, PRCP) 
  
  outs <- aggregate(TAVG ~ Year + Var + Relyear, data = pshew, FUN = mean )
  outs1 <- aggregate(TMAX ~ Year + Var + Relyear, data = pshew, FUN = mean )
  outs2 <- aggregate(Yield ~ Year + Var + Relyear, data = pshew, FUN = mean )
  outs4 <- aggregate(TMIN ~ Year + Var + Relyear, data = pshew, FUN = mean )
  outs5 <- aggregate(PRCP ~ Year + Var + Relyear, data = pshew, FUN = mean )
  
  outsData <- cbind(outs, outs1[4], outs2[4], outs4[4], outs5[4], location)
  outsData$Year <- as.numeric(as.character(outsData$Year))
  
  return(outsData)
}

aCVresPlot <- function(data1, location) {
  mod0 <- lm(Yield ~ TAVG * PRCP,
             data = data1)
  data1$resid <- mod0$residuals - min(mod0$residuals) # transform so all positive values
  particResMean <- aggregate(resid ~ Var + Relyear,
                             data = data1,
                             FUN = "mean")
  particResVariance <- aggregate(resid ~ Var + Relyear,
                                 data = data1, FUN = "var")
  temp <- metan::acv(particResMean$resid,
                     particResVariance$resid)
  particResMean$aCV <- temp$acv
  resMod <- lm(particResMean$aCV/100 ~ particResMean$Relyear)
  print(plot(particResMean$aCV/100 ~ particResMean$Relyear,
             pch = 2, cex.lab = 1.5, cex.axis = 1.5, cex = 1.3, 
             xlab = "Release year", ylab = bquote("aCV"[res]), 
             ylim = c(0,0.8), xlim = c(1920, 2020),
             main = location))
  mtext(bquote("R"^"2" == .(round(summary(resMod)$r.squared, 2))),
        line = -10.5, at = 1925, adj = 0, cex = 0.8)
  mtext(bquote("p =" ~ .(formatC(anova(resMod)$'Pr(>F)'[1], format = "e", digits = 2))),
        line = -11.5, at = 1925, adj = 0, cex = 0.8)
}

#####################################
########### Input Data ###############
#####################################

###yields for 10 years planted or more
b <-read.csv("location_yields/yield_bozeman.csv", check.names = F)
b10 <- b[b$count > 9, ]
hu <-read.csv("location_yields/yield_huntley.csv", check.names = F)
hu10 <- hu[hu$count > 9, ]
k <-read.csv("location_yields/yield_kalispell.csv", check.names = F)
k10 <- k[k$count > 9, ]
m <-read.csv("location_yields/yield_moccasin.csv", check.names = F)
m10 <- m[m$count > 9, ]
s <-read.csv("location_yields/yield_sidney.csv", check.names = F)
s10 <- s[s$count > 9, ]
ha <-read.csv("location_yields/yield_havre.csv", check.names = F)
ha10 <- ha[ha$count > 9, ]

###annual weather data 
ann <- read.csv("location_weather/annualNOAAallLocations.csv")

#convert to metric: degree Celsius and cm
ann$TAVG <- (ann$TAVG-32) *(5/9)
ann$TMAX <- (ann$TMAX-32) *(5/9)
ann$TMIN <- (ann$TMIN-32) *(5/9)
ann$PRCP <- ann$PRCP * 2.54



outsHav1 <- merge_weather_yield(ann, ha10, "Havre")
outsSid1 <- merge_weather_yield(ann, s10, "Sidney")
outsHun1 <- merge_weather_yield(ann, hu10, "Huntley")
outsBoz1 <- merge_weather_yield(ann, b10, "Bozeman")
outsMoc1 <- merge_weather_yield(ann, m10, "Moccasin")
outsKal1 <- merge_weather_yield(ann, k10, "Kalispell")

outsall <- rbind(outsHav1, outsSid1, outsBoz1, outsKal1, outsHun1, outsMoc1)
outsall$Yield <- outsall$Yield * 67.25 ##converting from bu/ac to kg/ha

####################################
###########Figure 5#################
####################################
outsMat <- data.matrix(outsall[,c(1,3,4,5,7,8)])
outs <- prcomp(outsMat, scale = T)

#Figure 5a
fviz_eig(outs, addlabels = TRUE, ylim = c(0, 70))
var <- get_pca_var(outs)
corrplot(var$cos2, is.corr=FALSE, method = "number", col = brewer.pal(n = 8, name = "RdBu"))

#Figure 5b
fviz_pca_biplot(outs, col.ind=outsall$Yield, invisible="quali", pointsize = 1, label = "var", repel = T,
                col.var = "orangered3", ggtheme = theme_classic())

####################################
###########Figure 6#################
####################################

par(mfrow = c(2,3))
par(mar = c(5, 5, 2, 1))
aCVresPlot(outsHav1, "Havre")
aCVresPlot(outsSid1, "Sidney")
aCVresPlot(outsHun1, "Huntley")
aCVresPlot(outsBoz1, "Bozeman")
aCVresPlot(outsMoc1 ,"Moccasin")
aCVresPlot(outsKal1, "Kalispell")


pcor(outsall[,c(6,4,5,7,8,1,3)], method = "pearson")
plot(outsall$TMIN, outsall$TMAX)
