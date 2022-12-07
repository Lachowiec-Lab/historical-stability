### Load env ###
renv::load()

### Packages ###
library(readxl)
library(raster)
library(tidyverse)
library(urbnmapr)
library(beanplot)
library(rcompanion)
library(nlstools)
library(sfsmisc)

### Read in yield data for each location
# List files
lc_yd_files <- list.files("location_yields", full.names = T)

# Data list 
data_list <- lapply(lc_yd_files, FUN = function(df){
  read.csv(df, check.names=FALSE) %>% 
    mutate(Location=basename(df) %>% 
             fs::path_ext_remove() %>%
             gsub("yield_","",.) %>% 
             str_to_title %>% 
             factor)
})

names(data_list) <- lc_yd_files %>% 
  basename() %>%
  fs::path_ext_remove() %>%
  gsub("yield_","",.) %>% 
  str_to_title()

# Order list
data_list <- data_list[c("Havre","Sidney","Huntley",
                         "Bozeman","Moccasin","Kalispell")]


# Bind yield data in one data.frame
all <- do.call(rbind, c(data_list, make.row.names=FALSE))

#####################################
########### Functions ###############
#####################################

log10_round <- function(text, pVal) {
  y <- floor(log10(pVal))
  x <- round(pVal / 10^y, 3)
  bquote(.(text)* ~ .(x) %*% 10 ^ .(y))
}

convertKgHa <- function(convertFactor = 67.25, data) {
  convertedData <- data %>%
    select(`1949`:`2019`) %>% 
    pivot_longer(cols=everything(), names_to = "Year", values_to = "Yield") %>% 
    group_by(Year) %>% 
    mutate(Yield = Yield * convertFactor) %>% 
    summarize(Average=mean(Yield, na.rm=TRUE),
              Sd = sd(Yield, na.rm = TRUE)) %>% 
    mutate(Year=as.numeric(Year),
           CV = Sd/Average)
  return(convertedData)
}

# Data frame of Kg/ha grouped by variety and location
convertKgHa_byVarLoc <- function(convertFactor = 67.25, data) {
  convertedData <- data %>%
    select(Var, Location, `1949`:`2019`) %>% 
    pivot_longer(cols = `1949`:`2019`,
                 names_to = "Year", 
                 values_to = "Yield") %>% 
    group_by(Var, Location) %>% 
    mutate(Yield = Yield * convertFactor) %>% 
    summarize(Average=mean(Yield, na.rm=TRUE),
              Sd = sd(Yield, na.rm = TRUE)) %>% 
    mutate(CV = Sd/Average)
  return(convertedData)
}



# Function to plot mean yield by year
yield_plot <- function(data, location, yaxes_title, xaxes_title) {
  
  data_plot <- convertKgHa(data= data)
  
  plot(data=data_plot, Average ~ Year, pch = 16, 
       main = location, ylab = yaxes_title,
       xlab = xaxes_title, cex.lab = 1.5, cex.axis = 1.5, cex = 1.2)
  out <- lm(data=data_plot, Average ~ Year)
  abline(out)
  mtext(bquote("R"^"2" == .(round(summary(out)$r.squared, 2))),
        line = -2, at = 1950, adj = 0, cex = 0.8)
  mtext(log10_round("p = ", anova(out)$'Pr(>F)'[1]), 
        line = -3, at = 1950, adj = 0, cex = 0.8)
}

# Plot residuals
resid_plot <- function(data, location){
  data_plot <- convertKgHa(data=data) 
  datamod <- lm(data=data_plot, Average ~ Year)
  # Residuals vs fitted plot
  plot(datamod, which = 1, main = location, caption="")
}

# Plot CV
plot_CV_count <- function(data, location, yaxes_title, xaxes_title) {
  plot(data$CV ~ data$count, ylab = yaxes_title, xlab = xaxes_title, main = location, cex.lab = 1.5, cex.axis = 1.5, cex = 1.3)
}

# Adjusted CV
adjCV <- function(Average, CV) {
  testme <- data.frame(Average, CV)
  testme <- testme[complete.cases(testme),]
  names(testme) <- c("Average", "CV")
  
  logvar <- log((testme$CV * testme$Average)^2, base = 10)
  logavg <- log(testme$Average, base = 10)
  
  reg <- lm(logvar ~ logavg)
  res <- reg$residuals
  
  aCVpart1 <- (1/testme$Average)
  mflat <- mean(logavg)
  b <- reg$coefficients[2]
  
  aCVpart2a <- (2-b)*logavg
  aCVpart2b <- (b-2)*mflat
  aCVpart2c <- logvar
  aCVpart2 <- 10^(aCVpart2a + aCVpart2b + aCVpart2c)
  
  aCV <- aCVpart1 * (aCVpart2^0.5)
  return(aCV)
}

bootCV <- function(all5) {
  n = nrow(all5)
  #First we make an empty matrix where we can put the results from running our subsampled/permuted data
  r2all <- matrix(NA, 1000, 3)
  ##The following code takes a couple minutes to run
  ##We will test how taking 1000x 5 years of data affects the relationship between CV and release year
  for (j in 1:1000){
    #below we make an empty matrix to fill in for the 38 lines with 5 sampled years of data
    rand5 <- matrix(NA, n, 5)
    print(j)
    for (i in 1:n){
      rand5[i,] <- as.numeric(sample(x = c(all5[i, which(!is.na(all5[i,3:73]))+2]), size = 5, replace = T))
    }
    print(i)
    rand5 <- as.data.frame(rand5)
    rand5$variance <- apply(X = rand5[,1:5], MARGIN = 1, var)
    rand5$mean <- apply(X = rand5[,1:5], MARGIN = 1, mean)
    rand5$Variety <- all5$Var
    rand5$Relyear <- all5$Relyear
    rand5 <- rand5[!rand5$variance == 0,] #in some cases, the same value was samples, resulting variance =0, so removed
    temp <- metan::acv(rand5$mean, rand5$variance)
    rand5$aCV <- temp$acv/100
    head(rand5)
    meanAdjCV <- aggregate(x = list(rand5$aCV, rand5$Relyear), by = list(rand5$Variety), FUN = mean)
    names(meanAdjCV) <- c("Variety", "aCV", "RelYear")
    
    mod3 <- lm(meanAdjCV$aCV ~ meanAdjCV$RelYear)
    
    r2all[j,1] <- mod3$coefficients[2]
    r2all[j,2] <- summary(mod3)$r.squared
    r2all[j,3] <- anova(mod3)$'Pr(>F)'[1]
  } 
  return(r2all)
}

#####################################
#########Figure 1####################
#####################################

# Average yield per year
all_year <- convertKgHa(data=all) 

### Figure 1A ###
par(mfrow = c(2,1))
plot(data=all_year, Average ~ Year,
     pch = 16, main = "",
     ylab = expression(paste("Yield (kg ha"^-1,")", sep = "")),
     xlab = "Year")
out <- lm(data=all_year, Average ~ Year)
abline(out)
mtext(bquote("R"^"2" == .(round(summary(out)$r.squared, 2))),
      line = -2, at = 1950, adj = 0, cex = 0.8)
mtext(log10_round("p = ", anova(out)$'Pr(>F)'[1]), 
      line = -3, at = 1950, adj = 0, cex = 0.8)

### Figure 1B ###
# Data frame - yield increase
yieldsup <- tibble(
  state_name = rep("Montana", 6), 
  county_name = c("Gallatin County", "Flathead County", "Hill County", "Yellowstone County", "Richland County", "Judith Basin County"), 
  yield_increase = c(0.7543, 1.01, 0.6329, 1.05, 0.3875, 0.4112)*67.25
)

# Yield in Montana counties
spatial_data <- left_join(get_urbn_map(map = "counties", sf = TRUE),
                          yieldsup,
                          by = "county_name")
spatial_data %>%
  filter(state_name.x == "Montana") %>%
  ggplot() +
  geom_sf(mapping = aes(fill = yield_increase),
          color = "#ffffff", size = 0.05) +
  coord_sf(datum = NA) +
  labs(fill = expression(paste("kg ha"^-1,"year"^-1)))

### Figure 1C ###
### Yield increase by county ###
par(mfrow = c(2, 3))
par(mar = c(5,5,2,1))
par(bty = 'n') 

yaxes_title <- c("Yield (kg/ha)", "", "", "Yield (kg/ha)", "", "")
xaxes_title <- c("", "", "", "Year", "Year", "Year")


# Plot for each location
for(i in 1:length(data_list)){
  yield_plot(data_list[[i]], names(data_list)[i], yaxes_title[i], xaxes_title[i])
}

#####################################
#####Supplemental Figure 1###########
#####################################

#Examine residuals of model fitting yield with year
par(mfrow = c(2,3))
par(mar = c(4,4,4,1))


for(i in 1:length(data_list)){
  resid_plot(data_list[[i]], names(data_list)[i])
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

# Define the null model
nullfunct <- function(x, m){m}

m.ini <- mean(all$count, na.rm=TRUE)

null <- nls(CV ~ nullfunct(count, m),
            data = all_narm,
            start = list(m = m.ini),
            trace = FALSE,
            nls.control(maxiter = 1000))

### Pseudo r-squared
nagelkerke(model, null)

### Confidence intervals for parameters
confint2(model,
         level = 0.95)

boot <- nlsBoot(model)
summary(boot)

## plot quadratic plateau fit
par(mfrow = c(1,1))
par(mar = c(5,5,3,1))
plotPredy(data  = all_narm,
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


for(i in 1:length(data_list)){
  plot_CV_count(data_list[[i]], names(data_list)[i], yaxes_title[i], xaxes_title[i])
}

#####################################
#####Supplemental Figure 3###########
#####################################
all_byVarLoc <- convertKgHa_byVarLoc(data = all)

par(mar = c(4.1, 4.1, 2.1, 2.1))
par(mfrow = c(1, 3))
#relationship between yield CV and mean
plot(all_byVarLoc$Average, all_byVarLoc$CV,
     xlab = expression(paste("Yield (kg ha"^-1,")"), sep = ""),
     ylab = "Yield CV")

#calculate the log of the mean and variance of the yields
logvar <- log((all_byVarLoc$CV * all_byVarLoc$Average)^2, base = 10)
logavg <- log(all_byVarLoc$Average, base = 10)

#relationship between yield log(variance) and log(mean)
plot(logavg, logvar, xlab = expression(paste("log"[10],"Yield mean"), sep = ""), 
     ylab = expression(paste("log"[10],"Yield variance"), sep = ""))
reg <- lm(logvar ~ logavg)
b <- reg$coefficients[2]
abline(reg)
mtext(bquote("b" == .(round(b, 2))),
      line = -13, at = 3.1, adj = 0, cex = 0.8)

aCVpart1 <- (1/all_byVarLoc$Average)
mflat <- mean(logavg)
aCVpart2a <- (2-b)*logavg
aCVpart2b <- (b-2)*mflat
aCVpart2c <- logvar
aCVpart2 <- 10^(aCVpart2a + aCVpart2b + aCVpart2c)
aCV <- aCVpart1 * (aCVpart2^0.5)

#relationship between yield aCV and mean
plot(all_byVarLoc$Average, aCV, 
     xlab = expression(paste("Yield (kg ha"^-1,")"), sep = ""),
     ylab = "Yield aCV")


#####################################
#####Figure 2########################
#####################################

#Figure 2a

#Based on plateau estimate, only examine varieties grown more than 9 years
all10 <- all[all$count > 9,]

##CV values for each location of all released varieties
countGt9_var_cv <- all %>% 
  filter(count>9) %>% 
  convertKgHa_byVarLoc(data = .) %>% 
  group_by(Location) %>% 
  mutate(aCV = adjCV(Average, CV))



totes <- countGt9_var_cv %>% 
  select(Location,Var,aCV) %>% 
  pivot_wider(names_from = Location, values_from = aCV,
              names_glue = "{Location}aCV")

oot <- aggregate(Relyear ~ Var, data = all10, FUN= median)
totes4 <- left_join(oot, totes, by = "Var")

totesLong <- gather(totes4, location, aCV,
                    MoccasinaCV:KalispellaCV,
                    factor_key=TRUE)
totesLong$Var <- with(totesLong, 
                      reorder(Var, Relyear, median))

beanplot(totesLong$aCV ~ totesLong$Var, las = 2, 
         xlab = "", ylab = "Yield aCV",
         col = c("gray", "black", "white", "red"),
         border = F, beanlinewd = 1, log = "",
         droplevel = T)

totesLong$location <- gsub("aCV", "", totesLong$location)
totesLong$location <- factor(totesLong$location, 
                             levels = c("Havre", "Sidney", "Huntley", "Bozeman", "Moccasin", "Kalispell"))
beanplot(totesLong$aCV ~ totesLong$location,
         las = 2, xlab = "", ylab = "",
         col = c("gray", "black", "white", "red"),
         border = F, beanlinewd = 1, log = "", droplevel = T)


##############################################
#####Supplemental Figure 3 #bootstrapping ####
##############################################

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


plot_ps <- function(data, location) {
  plot(density(data[,3]), xlim = c(0,1), xlab = "p-value", main = location)
  abline(v = 0.05, col = "red")
}

par(mfrow = c(2,3))
par(mar = c(5.1, 4.1, 4.1, 2.1))

for(i in 1:length(unique(all5$Location))){
  location <- unique(all5$Location)[i]
  bCV <- bootCV(all5 %>% filter(Location==location))
  plot_ps(bCV, location)
}