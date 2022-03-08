library(readxl)
library(raster)
library(tidyverse)
library(urbnmapr)
library(beanplot)
library(rcompanion)
library(nlstools)

###Read in yield data for each location
b <-read.csv("https://github.com/Lachowiec-Lab/historical-stability/tree/main/location_yields/yield_bozeman.csv")
hu <-read.csv("https://github.com/Lachowiec-Lab/historical-stability/tree/main/location_yields/yield_huntley.csv")
k <-read.csv("https://github.com/Lachowiec-Lab/historical-stability/tree/main/location_yields/yield_kalispell.csv")
m <-read.csv("https://github.com/Lachowiec-Lab/historical-stability/tree/main/location_yields/yield_moccasin.csv")
s <-read.csv("https://github.com/Lachowiec-Lab/historical-stability/tree/main/location_yields/yield_sidney.csv")
ha <-read.csv("https://github.com/Lachowiec-Lab/historical-stability/tree/main/location_yields/yield_havre.csv")

location_vector <- c("Havre", "Sidney", "Huntley", "Bozeman", "Moccasin", "Kalispell")
data_list <- list(ha, s, hu, b, m, k)

all <- rbind(b, hu, k, m, s, ha)

#####################################
#########Figure 1####################
#####################################

###Figure 1A
par(mfrow = c(2,1))
plot(colMeans(all[3:73], na.rm = T) ~ as.numeric(colnames(all[3:73])), pch = 16, main = "", ylab = "Yield (bu/a)", xlab = "Year")
out <- lm(colMeans(all[3:73], na.rm = T) ~ as.numeric(colnames(all[3:73])))
abline(out)
mtext(bquote("R"["adj"]^"2" == .(round(summary(out)$adj.r.squared, 2))),
      line = -2, at = 1950, adj = 0, cex = 0.8)
mtext(bquote("p =" ~ .(formatC(anova(out)$'Pr(>F)'[1], format = "e", digits = 2))),
      line = -3, at = 1950, adj = 0, cex = 0.8)

###Figure 1B
yieldsup <- tibble(
  state_name = rep("Montana", 6), 
  county_name = c("Gallatin County", "Flathead County", "Hill County", "Yellowstone County", "Richland County", "Judith Basin County"), 
  yield_increase = c(0.7543, 1.01, 0.6329, 1.05, 0.3875, 0.4112)
)

spatial_data <- left_join(get_urbn_map(map = "counties", sf = TRUE),
                          yieldsup,
                          by = "county_name")
spatial_data %>%
  filter(state_name.x == "Montana") %>%
  ggplot() +
  geom_sf(mapping = aes(fill = yield_increase),
          color = "#ffffff", size = 0.05) +
  coord_sf(datum = NA) +
  labs(fill = "bu/ac/yr")

###Figure 1C
par(mfrow = c(2, 3))
par(mar = c(5,5,2,1))
par(bty = 'n') 

yaxes_title <- c("Yield (bu/a)", "", "", "Yield (bu/a)", "", "")
xaxes_title <- c("", "", "", "Year", "Year", "Year")

yield_plot <- function(data, location, yaxes_title, xaxes_title) {
  plot(colMeans(data[3:73], na.rm = T) ~ as.numeric(colnames(data[3:73])), pch = 16, 
       main = location, ylab = yaxes_title, xlab = xaxes_title, cex.lab = 1.5, cex.axis = 1.5, cex = 1.2)
  out <- lm(colMeans(data[3:73], na.rm = T) ~ as.numeric(colnames(data[3:73])))
  abline(out)
  mtext(bquote("R"["adj"]^"2" == .(round(summary(out)$adj.r.squared, 2))),
        line = -2, at = 1950, adj = 0, cex = 0.8)
  mtext(bquote("p =" ~ .(formatC(anova(out)$'Pr(>F)'[1], format = "e", digits = 2))),
        line = -3, at = 1950, adj = 0, cex = 0.8)
}

for(i in 1:length(location_vector)){
  yield_plot(data_list[[i]], location_vector[i], yaxes_title[i], xaxes_title[i])
}

#####################################
#####Supplemental Figure 1###########
#####################################

#Examine residuals of model fitting yield with year
par(mfrow = c(2,3))
par(mar = c(4,4,4,1))

resid_plot <- function(data, location){
  datamod <- lm(colMeans(data[3:73], na.rm = T) ~ as.numeric(colnames(data[3:73])))
  plot(datamod, which = 1, main = location)
}

for(i in 1:length(location_vector)){
  resid_plot(data_list[[i]], location_vector[i])
}

#####################################
#####Supplemental Figure 2###########
#####################################

#Fit a model to understand how many years of data are needed 
#to accurately estimate CV 

###Supp Figure 2a
#Initially create a linear model to describe relationship
all_narm <- all[complete.cases(all$CV),]
fit.lm <- lm(CV ~ count, data=all_narm)

#Save linear model coefficients
a.ini <- fit.lm$coefficients[1]
b.ini <- fit.lm$coefficients[2]
#Provide initial value for fitting plateau model
clx.ini <- 10

#quadratic plateau function and fit
quadplat <- function(x, a, b, clx) {
  ifelse(x  < clx, a + b * x   + (-0.5*b/clx) * x   * x,
         a + b * clx + (-0.5*b/clx) * clx * clx)}

model <- nls(CV ~ quadplat(count, a, b, clx),
            data = all_narm,
            start = list(a = a.ini,
                         b = b.ini,
                         clx = clx.ini),
            trace = FALSE,
            nls.control(maxiter = 4000))

summary(model)

#Define the null model
nullfunct <- function(x, m){m}

m.ini <- mean(all$count)

null <- nls(CV ~ nullfunct(count, m),
           data = all_narm,
           start = list(m = m.ini),
           trace = FALSE,
           nls.control(maxiter = 1000))
nagelkerke(model, null)

###Confidence intervals for parameters
confint2(model,
         level = 0.95)

boot <- nlsBoot(model)
summary(boot)

##plot quadratic plateau fit
par(mfrow = c(1,1))
par(mar = c(5,5,3,1))
plotPredy(data  = all,
          x     = count,
          y     = CV,
          model = model,
          xlab  = "Years grown",
          ylab  = "Yield CV", pch = 1)

#Supp Figure 2b
par(mfrow = c(2,3))
par(mar = c(5,5,3,1))
yaxes_title <- c("Yield CV", "", "", "Yield CV", "", "")
xaxes_title <- c("", "", "", "Years grown", "Years grown", "Years grown")

plot_CV_count <- function(data, location, yaxes_title, xaxes_title) {
  plot(data$CV ~ data$count, ylab = yaxes_title, xlab = xaxes_title, main = location, cex.lab = 1.5, cex.axis = 1.5, cex = 1.3)
}

for(i in 1:length(location_vector)){
  plot_CV_count(data_list[[i]], location_vector[i], yaxes_title[i], xaxes_title[i])
}

#####################################
#####Figure 2########################
#####################################

#Based on plateau estimate, only examine varieties grown more than 9 years
all10 <- all[all$count > 9,]

#Figure 2a
par(mfrow = c(1, 2))
par(mar = c(7.1, 4.1, 4.1, 2.1))
plot(all10$CV ~ all10$Relyear, ylab = "Yield CV", xlab = "Release Year", yaxt = "n")
abline(lm(all10$CV ~ all10$Relyear))
axis(side = 2, las = 2)

#Figure 2b

##CV values for each location of all released varieties
m10 <- m[m$count > 9,]
b10 <- b[b$count > 9,]
s10 <- s[s$count > 9,]
ha10 <- ha[ha$count > 9,]
hu10 <- hu[hu$count > 9,]
k10 <- k[k$count > 9,]


bCV <- as.data.frame(b10[,c("Var", "CV")])
huCV <- as.data.frame(hu10[,c("Var", "CV")])
kCV <- as.data.frame(k10[,c("Var", "CV")])
mCV <- as.data.frame(m10[,c("Var", "CV")])
sCV <- as.data.frame(s10[,c("Var", "CV")])
haCV <- as.data.frame(ha10[,c("Var", "CV")])

totes <- list(bCV, huCV, kCV, mCV, sCV, haCV) %>% 
  reduce(full_join, by = "Var")
names(totes) <- c("Var", "BozemanCV", "HuntleyCV", "KalispellCV", "MoccasinCV", "SidneyCV", "HavreCV")

oot <- aggregate(Relyear ~ Var, data = all10, FUN= median)

totes4 <- left_join(oot, totes, by = "Var")
totesLong <- gather(totes4, location, CV, BozemanCV:HavreCV, factor_key=TRUE)
totesLong$Var <- with(totesLong, reorder(Var, Relyear, median))

beanplot(totesLong$CV ~ totesLong$Var, las = 2, xlab = "", ylab = "Yield CV",
         col = c("gray", "black", "white", "red"), border = F, beanlinewd = 1, log = "", droplevel = T)

############################################
########Figure 3############################
############################################

#Figure 3a
par(mfrow = c(2,3))
par(mar = c(5,5,3,1))
yaxes_title <- c("Yield CV", "", "", "Yield CV", "", "")
xaxes_title <- c("", "", "", "Release year", "Release year", "Release year")
data_list <- list(ha10, s10, hu10, b10, m10, k10)

plot_CV_year <- function(data, location, yaxes_title, xaxes_title) {
  plot(data$CV ~ data$Relyear, ylab = yaxes_title, xlab = xaxes_title, main = location, cex.lab = 1.5, cex.axis = 1.2, cex = 1)
  abline(lm(data$CV ~ data$Relyear))
  
}

for(i in 1:length(location_vector)){
  plot_CV_year(data_list[[i]], location_vector[i], yaxes_title[i], xaxes_title[i])
}

par(mfrow = c(2,2))
for(i in 1:length(location_vector)){
  summary(lm(data_list[[i]]$CV ~ data_list[[i]]$Relyear))
  plot(lm(data_list[[i]]$CV ~ data_list[[i]]$Relyear))
  
}


#Figure 3b
par(mfrow = c(1,1))
par(mar = c(7,5,1,1))
beanplot(totesLong$CV ~ totesLong$location, las = 2, xlab = "", ylab = "Yield CV",
         col = c("gray", "black", "white", "red"), border = F, beanlinewd = 1, log = "", droplevel = T,
         names = location_vector)

##############################################
#####Supplemental Figure 3 #bootstrapping ####
##############################################

bootCV <- function(all5) {
  n = nrow(all5)
  #First we make an empty matrix where we can put the results from running our subsampled/permuted data
  r2all <- matrix(NA, 1000, 3)
  ##The following code takes a couple minutes to run
  ##We will test how taking 1000x 5 years of data affects the relationship between CV and release year
  for (j in 1:1000){
    #below we make an empty matrix to fill in for the 38 lines with 5 sampled years of data
    rand5 <- matrix(NA, n, 5)
    for (i in 1:n){
      rand5[i,] <- as.numeric(sample(x = c(all5[i, which(!is.na(all5[i,3:73]))+2]), size = 5, replace = T))
    }
    rand5 <- as.data.frame(rand5)
    rand5$CV <- apply(X = rand5[,1:5], MARGIN = 1, cv)
    rand5$Var <- all5$Var
    rand5$Relyear <- all5$Relyear
    head(rand5)
    meanCV <- aggregate(x = list(rand5$CV, rand5$Relyear), by = list(rand5$Var), FUN = mean)
    names(meanCV) <- c("Var", "CV", "RelYear")
    
    mod3 <- lm(meanCV$CV ~ meanCV$RelYear)
    
    r2all[j,1] <- mod3$coefficients[2]
    r2all[j,2] <- summary(mod3)$adj.r.squared
    r2all[j,3] <- anova(mod3)$'Pr(>F)'[1]
  } 
  return(r2all)
}


all5 <- all[all$count > 4,]
r2all <- bootCV(all5)

par(mfrow = c(3, 3))
plot(density(r2all[,3]), xlim = c(0,1), main = "", xlab = "p-value")
abline(v = 0.05, col = "red")
hist(r2all[,2], xlab = expression(R^2), main = "")
hist(r2all[,1]/10, xlab = "slope estimate", main = "")

mean(r2all[,1]/10)
mean(r2all[,2])
mean(r2all[,3])


r2b <- bootCV(b[b$count > 4,])
r2ha <- bootCV(ha[ha$count > 4,])
r2hu <- bootCV(hu[hu$count > 4,])
r2s <- bootCV(s[s$count > 4,])
r2m <- bootCV(m[m$count > 4,])
r2k <- bootCV(k[k$count > 4,])

plot_ps <- function(data, location) {
  plot(density(data[,3]), xlim = c(0,1), xlab = "p-value", main = location)
  abline(v = 0.05, col = "red")
}

par(mfrow = c(2,3))
par(mar = c(5.1, 4.1, 4.1, 2.1))

data_list <- list(r2ha, r2s, r2ha, r2b, r2m, r2k)

for(i in 1:length(location_vector)){
  plot_ps(data_list[[i]], location_vector[i])
}


