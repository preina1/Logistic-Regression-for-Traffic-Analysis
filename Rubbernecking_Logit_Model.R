#Load data
data<-read.table('rubbernecking_trainingdata.csv', header=T, sep=',')

#Name variables
rubbernecking<-data$Rubbernecking
AMpeak<-data$AM.peak
PMpeak<-data$PM.peak
peak<-data$AM.PM
weather<-data$BadWeather
barrier<-data$Barrier
duration<-data$Duration..min.
onramp<-data$Onramp
offramp<-data$Offramp
fwy<-data$Fwy_fwy
hovlane<-data$HOV.lane
numlanes<-data$NoLanes
trucks<-data$TruckPercent

#Convert categorical variables
rubberneckingf<-factor(rubbernecking)
AMpeakf<-factor(AMpeak)
PMpeakf<-factor(PMpeak)
peakf<-factor(peak)
weatherf<-factor(weather)
barrierf<-factor(barrier)
onrampf<-factor(onramp)
offrampf<-factor(offramp)
fwyf<-factor(fwy)
hovlanef<-factor(hovlane)

logit2<-glm(rubberneckingf ~ onrampf + hovlanef + trucks, family='binomial')
summary(logit2)

#Fitted values
coeffs<-logit2$coefficients

logitc1<-exp(coeffs[1] + coeffs[2]*onramp + coeffs[3]*hovlane + coeffs[4]*trucks)

fittedc1<-(logitc1/(1+logitc1))

#Extract fitted
#write.table(fittedc1,file='Fitted_2019.xls',append=F, sep="\t", row.names=F)

#ROC Curve General
library(pROC)

dev.new()
rocobject<-roc(rubberneckingf, fittedc1)
area<-auc(rocobject)
#jpeg('roc_fitted.jpg')
plot(rocobject, col='blue', cex.lab=1.75, cex.axis=1.4)
#dev.off()
#legend(1,1,legend=area)
area

#plot(rubberneckingf, fittedc1, type='b', col="blue", legend="area", xlab='x', ylab='y')

#Create vectors for threshold specific ROC
fitted50<-fittedc1
fitted45<-fittedc1
fitted40<-fittedc1
fitted35<-fittedc1
fitted30<-fittedc1
fitted25<-fittedc1
fitted20<-fittedc1
fitted15<-fittedc1
fitted10<-fittedc1

#Predictions with thresholds
#50 %
for (k in 1:length(fittedc1))
{ if (fittedc1[k] < 0.5) {
  fitted50[k]=0  
}
  else {
    fitted50[k]=1
  }
}

#Predictions with thresholds
#45 %
for (k in 1:length(fittedc1))
{ if (fittedc1[k] < 0.45) {
  fitted45[k]=0  
}
  else {
    fitted45[k]=1
  }
}

#Predictions with thresholds
#40 %
for (k in 1:length(fittedc1))
{ if (fittedc1[k] < 0.4) {
  fitted40[k]=0  
}
  else {
    fitted40[k]=1
  }
}

#Predictions with thresholds
#35 %
for (k in 1:length(fittedc1))
{ if (fittedc1[k] < 0.35) {
  fitted35[k]=0  
}
  else {
    fitted35[k]=1
  }
}

#Predictions with thresholds
#30 %
for (k in 1:length(fittedc1))
{ if (fittedc1[k] < 0.30) {
  fitted30[k]=0  
}
  else {
    fitted30[k]=1
  }
}

#Predictions with thresholds
#25 %
for (k in 1:length(fittedc1))
{ if (fittedc1[k] < 0.25) {
  fitted25[k]=0  
}
  else {
    fitted25[k]=1
  }
}

#Predictions with thresholds
#20 %
for (k in 1:length(fittedc1))
{ if (fittedc1[k] < 0.2) {
  fitted20[k]=0  
}
  else {
    fitted20[k]=1
  }
}

#Predictions with thresholds
#15 %
for (k in 1:length(fittedc1))
{ if (fittedc1[k] < 0.15) {
  fitted15[k]=0  
}
  else {
    fitted15[k]=1
  }
}

#Predictions with thresholds
#10 %
for (k in 1:length(fittedc1))
{ if (fittedc1[k] < 0.1) {
  fitted10[k]=0  
}
  else {
    fitted10[k]=1
  }
}

#ROC Curve 50
dev.new()
rocobject50<-roc(rubberneckingf, fitted50)
#jpeg('roc_50.jpg')
plot(rocobject50, col='red', cex.lab=1.75, cex.axis=1.4)
area50<-auc(rocobject50)
area50

#ROC Curve 45
dev.new()
rocobject45<-roc(rubberneckingf, fitted45)
#jpeg('roc_45.jpg')
plot(rocobject45, col='green', cex.lab=1.75, cex.axis=1.4)
area45<-auc(rocobject45)
area45

#ROC Curve 40
dev.new()
rocobject40<-roc(rubberneckingf, fitted40)
#jpeg('roc_40.jpg')
plot(rocobject40, col='magenta', cex.lab=1.75, cex.axis=1.4)
area40<-auc(rocobject40)
area40

#ROC Curve 35
dev.new()
rocobject35<-roc(rubberneckingf, fitted35)
#jpeg('roc_35.jpg')
plot(rocobject35, col='cyan', cex.lab=1.75, cex.axis=1.4)
area35<-auc(rocobject35)
area35

#ROC Curve 30
dev.new()
rocobject30<-roc(rubberneckingf, fitted30)
#jpeg('roc_30.jpg')
plot(rocobject30, col='blue', cex.lab=1.75, cex.axis=1.4)
area30<-auc(rocobject30)
area30

#ROC Curve 25
dev.new()
rocobject25<-roc(rubberneckingf, fitted25)
#jpeg('roc_25.jpg')
plot(rocobject25, col='red', cex.lab=1.75, cex.axis=1.4)
area25<-auc(rocobject25)
area25

#ROC Curve 20
dev.new()
rocobject20<-roc(rubberneckingf, fitted20)
#jpeg('roc_20.jpg')
plot(rocobject20, col='green', cex.lab=1.75, cex.axis=1.4)
area20<-auc(rocobject20)
area20

#ROC Curve 15
dev.new()
rocobject15<-roc(rubberneckingf, fitted15)
#jpeg('roc_15.jpg')
plot(rocobject15, col='magenta', cex.lab=1.75, cex.axis=1.4)
area15<-auc(rocobject15)
area15

#ROC Curve 10
dev.new()
rocobject10<-roc(rubberneckingf, fitted10)
#jpeg('roc_10.jpg')
plot(rocobject10, col='cyan', cex.lab=1.75, cex.axis=1.4)
area10<-auc(rocobject10)
area10
