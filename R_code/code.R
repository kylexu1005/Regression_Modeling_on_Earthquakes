library(Hmisc)
setwd("~/Desktop/earthquake/code")

data=read.csv('../data/all_month.csv',sep=",",header=TRUE)
dt=data[complete.cases(data),] ## remove the rows containing missing values
dt=subset(dt,dt$type=='earthquake')
dt$latCut <- cut2(dt$latitude, g = 5)
dt$lonCut <- cut2(dt$longitude, g = 5)
dt$nstCut <- cut2(dt$nst, g = 5)
dt$log10Depth <- log10(dt$depth-min(dt$depth)+1)
dt$time=as.character(dt$time)
day=substr(dt$time,1,10)
hour=substr(dt$time,12,19)
new.time=paste(day,hour)
dt$time=strptime(new.time,format="%Y-%m-%d %H:%M:%S")
summary(dt)

# plot the magnitude of every earthquake in the last month
plot(dt$time, dt$mag, pch = 19,cex=0.5)
# plot the depth of every earthquake in the last month
plot(dt$time, dt$depth, pch = 19,cex=0.5)

# the percentage of earthquakes with magnitudes less than 3 is:
mean(dt$mag < 3)
# the percentage of earthquakes with magnitudes between 3 and 5 is:
mean(dt$mag > 3 & dt$mag < 5)
# Distribution of depths
hist(dt$depth, col = "grey",main="Histgram of the Distribution of Depths",
     xlab="depth",ylab="frequency")
#Distribution of log 10 based depths
hist(dt$log10Depth, col = "grey",main="Histgram of the Distribution of log 10 based Depths",
     xlab="depth",ylab="frequency")

# Fit model with no adjustment variable
lm.no.adjust <- lm(dt$mag ~ dt$log10Depth)

# Plot residuals, colored by different variables (latitude, longitude,
# number of sites observing the quake)
par(mfrow = c(1, 3))
plot(dt$log10Depth, lm.no.adjust$residuals, col = dt$latCut, pch = 19)
plot(dt$log10Depth, lm.no.adjust$residuals, col = dt$lonCut, pch = 19)
plot(dt$log10Depth, lm.no.adjust$residuals, col = dt$nstCut, pch = 19)

lm.final <- lm(dt$mag ~ dt$log10Depth + dt$latCut + dt$lonCut + dt$nst)
par(mfrow = c(1, 3))
plot(dt$log10Depth, lm.final$residuals, col = dt$latCut, pch = 19)
plot(dt$log10Depth, lm.final$residuals, col = dt$lonCut, pch = 19)
plot(dt$log10Depth, lm.final$residuals, col = dt$nstCut, pch = 19)

summary(lm.final)
confint(lm.final)