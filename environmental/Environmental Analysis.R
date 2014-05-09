
#clear everything, just to be safe 
rm(list=ls(all=TRUE))

library(rjags)
library(ggplot2)
library(arm)
library(popbio)
library(plyr)
library(numDeriv)
library(splines)
library(reshape)
library(gridExtra)
library(eha)

setwd("~/Projects/Current/Westbrook/Brook Trout/Data/Environmental")


envData <- read.csv("./Environmental Data Converted.csv", header=T)

envData$Spring <- 0
envData$Summer <- 0
envData$Fall <- 0
envData$Winter <- 0
  
envData$Spring[which(envData$Jday > 79 & envData$Jday < 173)] <- 1
envData$Summer[which(envData$Jday > 172 & envData$Jday < 266)] <- 2
envData$Fall[which(envData$Jday > 265 & envData$Jday < 356)] <- 3
envData$Winter[which(envData$Jday > 355 )] <- 4
envData$Winter[which(envData$Jday < 80 )] <- 4


envData$Season <- envData$Spring + envData$Summer + envData$Fall + envData$Winter

envData$SYear <- envData$Year

envData$SYear[which(envData$Jday < 80 )] <- envData$Year[which(envData$Jday < 80 )]-1

envWest <- envData[which(envData$Drainage=="WEST"),]
envWest <- envWest[-which(envWest$SYear<2002),]


##This graphs the daily values to quality check

v2 <- envWest[which(envWest$Season==1),]
#win.graph(); par(mfrow=c(1,1));
p <- ggplot( v2, aes(Jday,AvgOfAvg.Daily.Temp) ) 
Spr <- p + facet_grid(River~SYear,scales="free") + 
  geom_point() +  theme_bw() + geom_line()+
  # scale_x_continuous('Date') +
  scale_y_continuous('Daily Temp Spr')

v2 <- envWest[which(envWest$Season==2),]
#win.graph(); par(mfrow=c(1,1));
p <- ggplot( v2, aes(Jday,AvgOfAvg.Daily.Temp) ) 
Sum <- p + facet_grid(River~SYear,scales="free") + 
  geom_point() +  theme_bw() + geom_line()+
  # scale_x_continuous('Date') +
  scale_y_continuous('Daily Temp Sum')

v2 <- envWest[which(envWest$Season==3),]
#win.graph(); par(mfrow=c(1,1));
p <- ggplot( v2, aes(Jday,AvgOfAvg.Daily.Temp) ) 
Fall <- p + facet_grid(River~SYear,scales="free") + 
  geom_point() +  theme_bw() + geom_line()+
  # scale_x_continuous('Date') +
  scale_y_continuous('Daily Temp Fall')

v2 <- envWest[which(envWest$Season==4),]
v2$Jday[which(v2$Jday < 356 )] <- v2$Jday[which(v2$Jday < 356 )]+360
#win.graph(); par(mfrow=c(1,1));
p <- ggplot( v2, aes(Jday,AvgOfAvg.Daily.Temp) ) 
Win <- p + facet_grid(River~SYear,scales="free") + 
  geom_point() +  theme_bw() + geom_line()+
  # scale_x_continuous('Date') +
  scale_y_continuous('Daily Temp Win')


#png(filename="./Graphics/dailyVarTemp.png",width=575, height=1225, bg="white")
win.graph(); par(mfrow=c(1,1));
par(mfrow=c(1,1));
sidebysideplot <- grid.arrange(Spr,Sum,Fall,Win,ncol=2)
#dev.off()

envWest$AvgOfAvg.Daily.Temp[which(envWest$SYear==2012 & envWest$Season==1)] <- NA
envWest$AvgOfAvg.Daily.Temp[which(envWest$River%in%"WB JIMMY" & envWest$SYear==2006 & envWest$Season==1)] <- NA
envWest$AvgOfAvg.Daily.Temp[which(envWest$River%in%"WB JIMMY" & envWest$SYear==2006 & envWest$Season==2)] <- NA
envWest$AvgOfAvg.Daily.Temp[which(envWest$River%in%"WB MITCHELL" & envWest$SYear==2006 & envWest$Season==2)] <- NA
envWest$AvgOfAvg.Daily.Temp[which(envWest$River%in%"WB MITCHELL" & envWest$SYear==2005 & envWest$Season==2)] <- NA
envWest$AvgOfAvg.Daily.Temp[which(envWest$River%in%"WEST BROOK" & envWest$SYear==2008 & envWest$Season==2)] <- NA
envWest$AvgOfAvg.Daily.Temp[which(envWest$River%in%"WB MITCHELL" & envWest$SYear==2005 & envWest$Season==4)] <- NA

##Now for Flow
v2 <- envWest[which(envWest$Season==1),]
#win.graph(); par(mfrow=c(1,1));
p <- ggplot( v2, aes(Jday,log(AvgOfAvg.Daily.Discharge)) ) 
Spr <- p + facet_grid(River~SYear,scales="free") + 
  geom_point() +  theme_bw() + geom_line()+
  # scale_x_continuous('Date') +
  scale_y_continuous('Daily Discharge Spr')

v2 <- envWest[which(envWest$Season==2),]
#win.graph(); par(mfrow=c(1,1));
p <- ggplot( v2, aes(Jday,log(AvgOfAvg.Daily.Discharge)) ) 
Sum <- p + facet_grid(River~SYear,scales="free") + 
  geom_point() +  theme_bw() + geom_line()+
  # scale_x_continuous('Date') +
  scale_y_continuous('Daily Discharge Sum')

v2 <- envWest[which(envWest$Season==3),]
#win.graph(); par(mfrow=c(1,1));
p <- ggplot( v2, aes(Jday,log(AvgOfAvg.Daily.Discharge)) ) 
Fall <- p + facet_grid(River~SYear,scales="free") + 
  geom_point() +  theme_bw() + geom_line()+
  # scale_x_continuous('Date') +
  scale_y_continuous('Daily Discharge Fall')

v2 <- envWest[which(envWest$Season==4),]
v2$Jday[which(v2$Jday < 356 )] <- v2$Jday[which(v2$Jday < 356 )]+360
#win.graph(); par(mfrow=c(1,1));
p <- ggplot( v2, aes(Jday,log(AvgOfAvg.Daily.Discharge)) ) 
Win <- p + facet_grid(River~SYear,scales="free") + 
  geom_point() +  theme_bw() + geom_line()+
  # scale_x_continuous('Date') +
  scale_y_continuous('Daily Discharge Win')


#png(filename="./Graphics/meanVarFlow.png",width=575, height=1225, bg="white")
win.graph(); par(mfrow=c(1,1));
par(mfrow=c(1,1));
sidebysideplot <- grid.arrange(Spr,Sum,Fall,Win,ncol=2)
#dev.off()

envWest$AvgOfAvg.Daily.Discharge[which(envWest$SYear==2012 & envWest$Season==1)] <- NA




envWestMean <- ddply( envWest, .(SYear,Season,River), summarise,   
                   meanTemp=mean(AvgOfAvg.Daily.Temp, na.rm=TRUE),meanFlow=mean(AvgOfAvg.Daily.Discharge, na.rm=TRUE),
                   varTemp=var(AvgOfAvg.Daily.Temp, na.rm=TRUE),varFlow=var(AvgOfAvg.Daily.Discharge, na.rm=TRUE),
                   minTemp=min(AvgOfAvg.Daily.Temp, na.rm=TRUE),minFlow=min(AvgOfAvg.Daily.Discharge, na.rm=TRUE),
                   maxTemp=max(AvgOfAvg.Daily.Temp, na.rm=TRUE),maxFlow=max(AvgOfAvg.Daily.Discharge, na.rm=TRUE),
                   medianTemp=median(AvgOfAvg.Daily.Temp, na.rm=TRUE),medianFlow=median(AvgOfAvg.Daily.Discharge, na.rm=TRUE))



envWestMeanY <- ddply( envWestMean, .(Season,River), summarise,   
                      meanTempY=mean(meanTemp, na.rm=TRUE),meanFlowY=mean(meanFlow, na.rm=TRUE),
                      varTempY=var(meanTemp, na.rm=TRUE),varFlowY=var(meanFlow, na.rm=TRUE)  )

envWestMean <- merge(x=envWestMean,y=envWestMeanY,by.x=c('Season','River'), 
      by.y=c('Season','River'))

envWestMean$stdTemp <- (envWestMean$meanTemp-envWestMean$meanTempY)/(envWestMean$varTempY)^(1/2)
envWestMean$stdFlow <- (envWestMean$meanFlow-envWestMean$meanFlowY)/(envWestMean$varFlowY)^(1/2)

envWestMean$SYearCent <- envWestMean$SYear-2002

envWestMean$Season <- factor(envWestMean$Season,levels=c(1:4),ordered=T)
names(envWest$Season) <- list(c(1:4))


envWestMean$lnmeanFlow <- log(envWestMean$meanFlow)
envWestMean$lnvarFlow <- log(envWestMean$varFlow)

write.csv(envWestMean, file = "./envWestMean.csv")



mod <- glm( (meanTemp) ~    0 + River*Season*SYearCent #-SYearCent -River -Season -SYearCent*River -SYearCent*Season 
, family=gaussian,data=envWestMean)
summary(mod)


mod <- glm( (meanFlow) ~    1 + SYearCent + River + River*SYearCent
            , family=gaussian,data=envWestMean)
summary(mod)









##This graphs the yearly means and variances
##Flow first
v2 <- envWestMean

v2$Season <- factor(v2$Season,levels=c(1:4),ordered=T)

model <- lm(log(meanFlow) ~ SYearCent + factor(Season) + factor(River)+ 
  factor(River)*factor(Season) + factor(River)*SYearCent +factor(Season)*SYearCent+ 
  factor(River)*factor(Season)*SYearCent, data=v2)
grid <- with(v2, expand.grid(
  SYearCent = seq(min(SYearCent), max(SYearCent), length = 20),
  River = levels(factor(River)),
  Season = levels(factor(Season))))
grid$SYear <- grid$SYear+2002
grid$meanFlow <- exp(stats::predict(model, newdata=grid))
meanF <- qplot(SYear, log(meanFlow), data=v2, colour=factor(River)) + facet_wrap(~Season,scales="free")+
  geom_line(data=grid)+
  scale_x_continuous('Year') +
  scale_y_continuous('LN Mean Flow')

model <- lm(log(varFlow) ~ SYearCent + factor(Season) + factor(River)+ 
  factor(River)*factor(Season) + factor(River)*SYearCent +factor(Season)*SYearCent+ 
  factor(River)*factor(Season)*SYearCent, data=v2)
grid <- with(v2, expand.grid(
  SYearCent = seq(min(SYearCent), max(SYearCent), length = 20),
  River = levels(factor(River)),
  Season = levels(factor(Season))))
grid$SYear <- grid$SYear+2002
grid$varFlow <- exp(stats::predict(model, newdata=grid))
varF <- qplot(SYear, log(varFlow), data=v2, colour=factor(River)) + facet_wrap(~Season,scales="free")+
  geom_line(data=grid)+
  scale_x_continuous('Year') +
  scale_y_continuous('LN Variance Flow')

png(filename="./Graphics/meanVarFlow.png",width=575, height=1225, bg="white")
#win.graph(); par(mfrow=c(1,1));
par(mfrow=c(1,1));
sidebysideplot <- grid.arrange(meanF,varF,ncol=1)
dev.off()





##Temp
v2 <- envWestMean

v2$Season <- factor(v2$Season,levels=c(1:4),ordered=T)

model <- lm(meanTemp ~ SYearCent + factor(Season) + factor(River)+ 
  factor(River)*factor(Season) + factor(River)*SYearCent +factor(Season)*SYearCent+ 
  factor(River)*factor(Season)*SYearCent, data=v2)
grid <- with(v2, expand.grid(
  SYearCent = seq(min(SYearCent), max(SYearCent), length = 20),
  River = levels(factor(River)),
  Season = levels(factor(Season))))
grid$SYear <- grid$SYear+2002
grid$meanTemp <- stats::predict(model, newdata=grid)
meanT <- qplot(SYear, meanTemp, data=v2, colour=factor(River)) + facet_wrap(~Season,scales="free")+
  geom_line(data=grid)+
  geom_smooth(method = "lm", formula = y ~ poly(x,5), colour = "red")+
  scale_x_continuous('Year') +
  scale_y_continuous('Mean Temp')

model <- lm(varTemp ~ SYearCent + factor(Season) + factor(River)+ 
  factor(River)*factor(Season) + factor(River)*SYearCent +factor(Season)*SYearCent+ 
  factor(River)*factor(Season)*SYearCent, data=v2)
grid <- with(v2, expand.grid(
  SYearCent = seq(min(SYearCent), max(SYearCent), length = 20),
  River = levels(factor(River)),
  Season = levels(factor(Season))))
grid$SYear <- grid$SYear+2002
grid$varTemp <- stats::predict(model, newdata=grid)
varT <- qplot(SYear, varTemp, data=v2, colour=factor(River)) + facet_wrap(~Season,scales="free")+
  geom_line(data=grid)+
  scale_x_continuous('Year') +
  scale_y_continuous('Variance Temp')

png(filename="./Graphics/meanVarTemp.png",width=575, height=1225, bg="white")
#win.graph(); par(mfrow=c(1,1));
par(mfrow=c(1,1));
sidebysideplot <- grid.arrange(meanT,varT,ncol=1)
dev.off()


























##This is for the westbrook 
v2 <- envWestMean

v2$Season <- factor(v2$Season,levels=c(1:4),ordered=T)
v2$minTemp[which(v2$maxTemp=="Inf")] <- NaN
#v2$River <- factor(v2$River,levels=c('Westbrook','Jimmy','Mitchell','Obear'),ordered=T)
#   win.graph(); par(mfrow=c(1,1));
p <- ggplot( v2, aes(SYear,minFlow) ) 
stdF <- p + facet_grid(River~Season,scales="free") + 
  geom_point() +  theme_bw() + geom_line()+
  scale_x_continuous('Year') +
  scale_y_continuous('Min Flow') +
  geom_smooth(method = "lm", formula = y ~ x, colour = "red")


#   win.graph(); par(mfrow=c(1,1));
p <- ggplot( v2, aes(SYear,minTemp) ) 
stdT <- p + facet_grid(Season~River,scales="free") +
  geom_point() +  theme_bw() + geom_line()+
  scale_x_continuous('Year') +
  scale_y_continuous('Min Temperature') +
  geom_smooth(method = "lm", formula = y ~ x, colour = "red")

png(filename="./Graphics/minflowandtemp.png",width=575, height=1225, bg="white")
#win.graph(); par(mfrow=c(1,1));
par(mfrow=c(1,1));
sidebysideplot <- grid.arrange(stdT,stdF,ncol=1)
dev.off()




##This is for the westbrook 
v2 <- envWestMean
v2$maxTemp[which(v2$maxTemp=="-Inf")] <- NaN
v2$Season <- factor(v2$Season,levels=c(1:4),ordered=T)
#v2$River <- factor(v2$River,levels=c('Westbrook','Jimmy','Mitchell','Obear'),ordered=T)
#   win.graph(); par(mfrow=c(1,1));
p <- ggplot( v2, aes(SYear,maxFlow) ) 
stdF <- p + facet_grid(River~Season,scales="free") + 
  geom_point() +  theme_bw() + geom_line()+
  scale_x_continuous('Year') +
  scale_y_continuous('Max Flow') +
  geom_smooth(method = "lm", formula = y ~ x, colour = "red")


#   win.graph(); par(mfrow=c(1,1));
p <- ggplot( v2, aes(SYear,maxTemp) ) 
stdT <- p + facet_grid(Season~River,scales="free") +
  geom_point() +  theme_bw() + geom_line()+
  scale_x_continuous('Year') +
  scale_y_continuous('Max Temperature') +
  geom_smooth(method = "lm", formula = y ~ x, colour = "red")

png(filename="./Graphics/maxflowandtemp.png",width=575, height=1225, bg="white")
#win.graph(); par(mfrow=c(1,1));
par(mfrow=c(1,1));
sidebysideplot <- grid.arrange(stdT,stdF,ncol=1)
dev.off()



##This is for the westbrook 
v2 <- envWestMean

v2$Season <- factor(v2$Season,levels=c(1:4),ordered=T)
#v2$River <- factor(v2$River,levels=c('Westbrook','Jimmy','Mitchell','Obear'),ordered=T)
#   win.graph(); par(mfrow=c(1,1));
p <- ggplot( v2, aes(SYear,medianFlow) ) 
stdF <- p + facet_grid(River~Season,scales="free") + 
  geom_point() +  theme_bw() + geom_line()+
  scale_x_continuous('Year') +
  scale_y_continuous('median Flow') +
  geom_smooth(method = "lm", formula = y ~ x, colour = "red")


#   win.graph(); par(mfrow=c(1,1));
p <- ggplot( v2, aes(SYear,medianTemp) ) 
stdT <- p + facet_grid(Season~River,scales="free") +
  geom_point() +  theme_bw() + geom_line()+
  scale_x_continuous('Year') +
  scale_y_continuous('median Temperature') +
  geom_smooth(method = "lm", formula = y ~ x, colour = "red")

png(filename="./Graphics/medianflowandtemp.png",width=575, height=1225, bg="white")
#win.graph(); par(mfrow=c(1,1));
par(mfrow=c(1,1));
sidebysideplot <- grid.arrange(stdT,stdF,ncol=1)
dev.off()


























p <- ggplot( v2, aes(SYear,varFlow) ) 
stdF <- p + facet_grid(River~Season,scales="free") + 
  geom_point() +  theme_bw() + geom_line()+
  scale_x_continuous('Year') +
  scale_y_continuous('Variance Flow') +
  geom_smooth(method = "lm", formula = y ~ x, colour = "red")


#   win.graph(); par(mfrow=c(1,1));
p <- ggplot( v2, aes(SYear,varTemp) ) 
stdT <- p + facet_grid(River~Season,scales="free") +
  geom_point() +  theme_bw() + geom_line()+
  scale_x_continuous('Year') +
  scale_y_continuous('Variance Temperature') +
  geom_smooth(method = "lm", formula = y ~ x, colour = "red")


win.graph(); par(mfrow=c(1,1));
par(mfrow=c(1,1));
sidebysideplot <- grid.arrange(stdT,stdF,ncol=2)






v2 <- envWest

v2$Season <- factor(v2$Season,levels=c(1:4),ordered=T)
v2$SYear <- factor(v2$SYear,levels=c(1997:2012),ordered=T)
#v2$RiverOrdered <- factor(v2$River,levels=c('Westbrook','Jimmy','Mitchell','Obear'),ordered=T)
#   win.graph(); par(mfrow=c(1,1));

  win.graph(); par(mfrow=c(1,1));
qplot(log(AvgOfAvg.Daily.Discharge), data = v2, geom = "freqpoly", binwidth = 0.5,
      colour = River)+facet_grid(SYear~Season,scales="free")


p <- ggplot( v2, aes(SYear,AvgOfAvg.Daily.Discharge) ) 
stdF <- p + facet_grid(River~Season,scales="free") + 
  geom_boxplot() +  theme_bw()  


#   win.graph(); par(mfrow=c(1,1));
p <- ggplot( v2, aes(SYear,AvgOfAvg.Daily.Temp) ) 
stdT <- p + facet_grid(River~Season,scales="free") +
  geom_boxplot() +  theme_bw()  


win.graph(); par(mfrow=c(1,1));
par(mfrow=c(1,1));
sidebysideplot <- grid.arrange(stdT,stdF,ncol=2)


save.image("./EnvironmentalVars.RData")
