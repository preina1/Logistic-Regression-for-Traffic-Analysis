### Set working directory
setwd("/Users/paulinareina/Logistic-Regression-for-Traffic-Analysis")

### Load data
data<-read.table('Rubbernecking_data_takeoneout.csv', header=T, sep=',')

### Assign variables
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

### Preliminary model
logit<-glm(rubbernecking ~ AMpeak + weather + barrier + duration + onramp + offramp + fwy + hovlane + numlanes + trucks, family='binomial')
summary(logit)

### Forward selection
logit2 <-glm(rubbernecking ~ onramp, family = 'binomial')
summary(logit2)

logit3 <- glm(rubbernecking ~ onramp + trucks, family = 'binomial')
summary(logit3)

logit4 <- glm(rubbernecking ~ onramp + trucks + hovlane, family = 'binomial')
summary(logit4)

logit5 <- glm(rubbernecking ~ onramp + trucks + hovlane + numlanes, family = 'binomial')
summary(logit5)

### Fit selected model

logit_final<-glm(rubbernecking ~ onramp + hovlane + trucks, family='binomial')
summary(logit_final)

### Get fitted values
coeffs<-logit_final$coefficients

logitc1<-exp(coeffs[1] + coeffs[2]*onramp + coeffs[3]*hovlane + coeffs[4]*trucks)

fittedc1<-(logitc1/(1+logitc1))

### Plot ROC curve
#### Load library
library(pROC)

#### Plot curve using fitted probabilities
rocobject<-roc(rubbernecking, fittedc1)
area<-auc(rocobject)
plot(rocobject, col='blue', cex.lab=1.75, cex.axis=1.4)
area

### ROC analysis to determine optimal decision threshold
fitted50<-fittedc1
fitted45<-fittedc1
fitted40<-fittedc1
fitted35<-fittedc1
fitted30<-fittedc1
fitted25<-fittedc1
fitted20<-fittedc1
fitted15<-fittedc1
fitted10<-fittedc1

#### ROC curve per probability threshold
#### 50%
for (k in 1:length(fittedc1))
{ if (fittedc1[k] < 0.5) {
  fitted50[k]=0  
}
  else {
    fitted50[k]=1
  }
}

rocobject50<-roc(rubbernecking, fitted50)
plot(rocobject50, col='red', cex.lab=1.75, cex.axis=1.4)
area50<-auc(rocobject50)
area50

#### 45%
for (k in 1:length(fittedc1))
{ if (fittedc1[k] < 0.45) {
  fitted45[k]=0  
}
  else {
    fitted45[k]=1
  }
}

rocobject45<-roc(rubbernecking, fitted45)
plot(rocobject45, col='green', cex.lab=1.75, cex.axis=1.4)
area45<-auc(rocobject45)
area45

#### 40%
for (k in 1:length(fittedc1))
{ if (fittedc1[k] < 0.4) {
  fitted40[k]=0  
}
  else {
    fitted40[k]=1
  }
}

rocobject40<-roc(rubbernecking, fitted40)
plot(rocobject40, col='magenta', cex.lab=1.75, cex.axis=1.4)
area40<-auc(rocobject40)
area40

#### 35%
```{r }
for (k in 1:length(fittedc1))
{ if (fittedc1[k] < 0.35) {
  fitted35[k]=0  
}
  else {
    fitted35[k]=1
  }
}

rocobject35<-roc(rubbernecking, fitted35)
plot(rocobject35, col='cyan', cex.lab=1.75, cex.axis=1.4)
area35<-auc(rocobject35)
area35

#### 30%
for (k in 1:length(fittedc1))
{ if (fittedc1[k] < 0.30) {
  fitted30[k]=0  
}
  else {
    fitted30[k]=1
  }
}

rocobject30<-roc(rubbernecking, fitted30)
plot(rocobject30, col='blue', cex.lab=1.75, cex.axis=1.4)
area30<-auc(rocobject30)
area30

#### 25%
for (k in 1:length(fittedc1))
{ if (fittedc1[k] < 0.25) {
  fitted25[k]=0  
}
  else {
    fitted25[k]=1
  }
}

rocobject25<-roc(rubbernecking, fitted25)
plot(rocobject25, col='red', cex.lab=1.75, cex.axis=1.4)
area25<-auc(rocobject25)
area25

#### 20%
for (k in 1:length(fittedc1))
{ if (fittedc1[k] < 0.2) {
  fitted20[k]=0  
}
  else {
    fitted20[k]=1
  }
}

rocobject20<-roc(rubbernecking, fitted20)
plot(rocobject20, col='green', cex.lab=1.75, cex.axis=1.4)
area20<-auc(rocobject20)
area20

#### 15%
for (k in 1:length(fittedc1))
{ if (fittedc1[k] < 0.15) {
  fitted15[k]=0  
}
  else {
    fitted15[k]=1
  }
}

rocobject15<-roc(rubbernecking, fitted15)
plot(rocobject15, col='magenta', cex.lab=1.75, cex.axis=1.4)
area15<-auc(rocobject15)
area15

#### 10%
for (k in 1:length(fittedc1))
{ if (fittedc1[k] < 0.1) {
  fitted10[k]=0  
}
  else {
    fitted10[k]=1
  }
}
rocobject10<-roc(rubbernecking, fitted10)
plot(rocobject10, col='cyan', cex.lab=1.75, cex.axis=1.4)
area10<-auc(rocobject10)
area10

## Threshold with highest AUC is 15%, however AUC is still too low, recommendation is to get larger 
## data set and include additional features no currently present in the dataset that include driver behavior 
## characteristics which are believed to provide higher explanatory value to the dependent variable, 
## thus testing for this model would be redundant (since it has already been determined that model doesn't 
## perform well). Suggested improvements are outside the scope of this project. Next, I end with a couple of 
## metrics for completeness.

### Get metrics
tp<-rocobject15$sensitivities
tn<-rocobject15$specificities

tpr<-tp[2]
tnr<-tn[2]
print(paste('True positive rate:', tpr))
print(paste('True negative rate:', tnr))