Logistic Regression for Traffic Analysis
================

### Set working directory

``` r
setwd("/Users/paulinareina/Logistic-Regression-for-Traffic-Analysis")
```

### Load data

``` r
data<-read.table('Rubbernecking_data_takeoneout.csv', header=T, sep=',')
```

### Assign variables

``` r
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
```

### Preliminary model

``` r
logit<-glm(rubbernecking ~ AMpeak + weather + barrier + duration + onramp + offramp + fwy + hovlane + numlanes + trucks, family='binomial')
summary(logit)
```

    ## 
    ## Call:
    ## glm(formula = rubbernecking ~ AMpeak + weather + barrier + duration + 
    ##     onramp + offramp + fwy + hovlane + numlanes + trucks, family = "binomial")
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -4.476988   1.206333  -3.711 0.000206 ***
    ## AMpeak       0.003911   0.276238   0.014 0.988703    
    ## weather     -0.044279   0.290288  -0.153 0.878765    
    ## barrier      0.581929   1.087464   0.535 0.592563    
    ## duration    -0.003330   0.002662  -1.251 0.211052    
    ## onramp       1.013617   0.342242   2.962 0.003059 ** 
    ## offramp     -0.295330   0.310726  -0.950 0.341882    
    ## fwy          0.116039   0.374861   0.310 0.756901    
    ## hovlane      0.707509   0.312452   2.264 0.023551 *  
    ## numlanes     0.240667   0.165136   1.457 0.145009    
    ## trucks       0.075539   0.030006   2.517 0.011821 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 423.95  on 552  degrees of freedom
    ## Residual deviance: 391.36  on 542  degrees of freedom
    ## AIC: 413.36
    ## 
    ## Number of Fisher Scoring iterations: 5

### Forward selection

``` r
logit2 <-glm(rubbernecking ~ onramp, family = 'binomial')
summary(logit2)
```

    ## 
    ## Call:
    ## glm(formula = rubbernecking ~ onramp, family = "binomial")
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  -2.5701     0.2679  -9.594  < 2e-16 ***
    ## onramp        0.9394     0.3052   3.078  0.00208 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 423.95  on 552  degrees of freedom
    ## Residual deviance: 413.17  on 551  degrees of freedom
    ## AIC: 417.17
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
logit3 <- glm(rubbernecking ~ onramp + trucks, family = 'binomial')
summary(logit3)
```

    ## 
    ## Call:
    ## glm(formula = rubbernecking ~ onramp + trucks, family = "binomial")
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -2.87908    0.30208  -9.531  < 2e-16 ***
    ## onramp       0.91007    0.30757   2.959  0.00309 ** 
    ## trucks       0.07476    0.02791   2.679  0.00738 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 423.95  on 552  degrees of freedom
    ## Residual deviance: 406.74  on 550  degrees of freedom
    ## AIC: 412.74
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
logit4 <- glm(rubbernecking ~ onramp + trucks + hovlane, family = 'binomial')
summary(logit4)
```

    ## 
    ## Call:
    ## glm(formula = rubbernecking ~ onramp + trucks + hovlane, family = "binomial")
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -3.09708    0.32002  -9.678  < 2e-16 ***
    ## onramp       0.90907    0.31020   2.931  0.00338 ** 
    ## trucks       0.06890    0.02765   2.492  0.01270 *  
    ## hovlane      0.77505    0.26983   2.872  0.00407 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 423.95  on 552  degrees of freedom
    ## Residual deviance: 398.83  on 549  degrees of freedom
    ## AIC: 406.83
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
logit5 <- glm(rubbernecking ~ onramp + trucks + hovlane + numlanes, family = 'binomial')
summary(logit5)
```

    ## 
    ## Call:
    ## glm(formula = rubbernecking ~ onramp + trucks + hovlane + numlanes, 
    ##     family = "binomial")
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -4.29760    0.72886  -5.896 3.72e-09 ***
    ## onramp       0.85214    0.31112   2.739  0.00616 ** 
    ## trucks       0.07504    0.02879   2.607  0.00914 ** 
    ## hovlane      0.60242    0.28662   2.102  0.03557 *  
    ## numlanes     0.30181    0.16179   1.865  0.06213 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 423.95  on 552  degrees of freedom
    ## Residual deviance: 395.29  on 548  degrees of freedom
    ## AIC: 405.29
    ## 
    ## Number of Fisher Scoring iterations: 5

### Fit selected model

``` r
logit_final<-glm(rubbernecking ~ onramp + hovlane + trucks, family='binomial')
```

``` r
summary(logit_final)
```

    ## 
    ## Call:
    ## glm(formula = rubbernecking ~ onramp + hovlane + trucks, family = "binomial")
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -3.09708    0.32002  -9.678  < 2e-16 ***
    ## onramp       0.90907    0.31020   2.931  0.00338 ** 
    ## hovlane      0.77505    0.26983   2.872  0.00407 ** 
    ## trucks       0.06890    0.02765   2.492  0.01270 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 423.95  on 552  degrees of freedom
    ## Residual deviance: 398.83  on 549  degrees of freedom
    ## AIC: 406.83
    ## 
    ## Number of Fisher Scoring iterations: 5

### Get fitted values

``` r
coeffs<-logit_final$coefficients

logitc1<-exp(coeffs[1] + coeffs[2]*onramp + coeffs[3]*hovlane + coeffs[4]*trucks)

fittedc1<-(logitc1/(1+logitc1))
```

### Plot ROC curve

#### Load library

``` r
library(pROC)
```

    ## Type 'citation("pROC")' for a citation.

    ## 
    ## Attaching package: 'pROC'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     cov, smooth, var

#### Plot curve using fitted probabilities

``` r
rocobject<-roc(rubbernecking, fittedc1)
```

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

``` r
area<-auc(rocobject)
plot(rocobject, col='blue', cex.lab=1.75, cex.axis=1.4)
```

![](Logistic-Reg-for-Traffic_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
area
```

    ## Area under the curve: 0.6521

### ROC analysis to determine optimal decision threshold

``` r
fitted50<-fittedc1
fitted45<-fittedc1
fitted40<-fittedc1
fitted35<-fittedc1
fitted30<-fittedc1
fitted25<-fittedc1
fitted20<-fittedc1
fitted15<-fittedc1
fitted10<-fittedc1
```

#### ROC curve per probability threshold

#### 50%

``` r
for (k in 1:length(fittedc1))
{ if (fittedc1[k] < 0.5) {
  fitted50[k]=0  
}
  else {
    fitted50[k]=1
  }
}

rocobject50<-roc(rubbernecking, fitted50)
```

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

``` r
plot(rocobject50, col='red', cex.lab=1.75, cex.axis=1.4)
```

![](Logistic-Reg-for-Traffic_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
area50<-auc(rocobject50)
area50
```

    ## Area under the curve: 0.499

#### 45%

``` r
for (k in 1:length(fittedc1))
{ if (fittedc1[k] < 0.45) {
  fitted45[k]=0  
}
  else {
    fitted45[k]=1
  }
}

rocobject45<-roc(rubbernecking, fitted45)
```

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

``` r
plot(rocobject45, col='green', cex.lab=1.75, cex.axis=1.4)
```

![](Logistic-Reg-for-Traffic_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
area45<-auc(rocobject45)
area45
```

    ## Area under the curve: 0.499

#### 40%

``` r
for (k in 1:length(fittedc1))
{ if (fittedc1[k] < 0.4) {
  fitted40[k]=0  
}
  else {
    fitted40[k]=1
  }
}

rocobject40<-roc(rubbernecking, fitted40)
```

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

``` r
plot(rocobject40, col='magenta', cex.lab=1.75, cex.axis=1.4)
```

![](Logistic-Reg-for-Traffic_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
area40<-auc(rocobject40)
area40
```

    ## Area under the curve: 0.5271

#### 35%

``` r
for (k in 1:length(fittedc1))
{ if (fittedc1[k] < 0.35) {
  fitted35[k]=0  
}
  else {
    fitted35[k]=1
  }
}

rocobject35<-roc(rubbernecking, fitted35)
```

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

``` r
plot(rocobject35, col='cyan', cex.lab=1.75, cex.axis=1.4)
```

![](Logistic-Reg-for-Traffic_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
area35<-auc(rocobject35)
area35
```

    ## Area under the curve: 0.5321

#### 30%

``` r
for (k in 1:length(fittedc1))
{ if (fittedc1[k] < 0.30) {
  fitted30[k]=0  
}
  else {
    fitted30[k]=1
  }
}

rocobject30<-roc(rubbernecking, fitted30)
```

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

``` r
plot(rocobject30, col='blue', cex.lab=1.75, cex.axis=1.4)
```

![](Logistic-Reg-for-Traffic_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
area30<-auc(rocobject30)
area30
```

    ## Area under the curve: 0.54

#### 25%

``` r
for (k in 1:length(fittedc1))
{ if (fittedc1[k] < 0.25) {
  fitted25[k]=0  
}
  else {
    fitted25[k]=1
  }
}

rocobject25<-roc(rubbernecking, fitted25)
```

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

``` r
plot(rocobject25, col='red', cex.lab=1.75, cex.axis=1.4)
```

![](Logistic-Reg-for-Traffic_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
area25<-auc(rocobject25)
area25
```

    ## Area under the curve: 0.5575

#### 20%

``` r
for (k in 1:length(fittedc1))
{ if (fittedc1[k] < 0.2) {
  fitted20[k]=0  
}
  else {
    fitted20[k]=1
  }
}

rocobject20<-roc(rubbernecking, fitted20)
```

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

``` r
plot(rocobject20, col='green', cex.lab=1.75, cex.axis=1.4)
```

![](Logistic-Reg-for-Traffic_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
area20<-auc(rocobject20)
area20
```

    ## Area under the curve: 0.557

#### 15%

``` r
for (k in 1:length(fittedc1))
{ if (fittedc1[k] < 0.15) {
  fitted15[k]=0  
}
  else {
    fitted15[k]=1
  }
}

rocobject15<-roc(rubbernecking, fitted15)
```

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

``` r
plot(rocobject15, col='magenta', cex.lab=1.75, cex.axis=1.4)
```

![](Logistic-Reg-for-Traffic_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
area15<-auc(rocobject15)
area15
```

    ## Area under the curve: 0.6214

#### 10%

``` r
for (k in 1:length(fittedc1))
{ if (fittedc1[k] < 0.1) {
  fitted10[k]=0  
}
  else {
    fitted10[k]=1
  }
}
rocobject10<-roc(rubbernecking, fitted10)
```

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

``` r
plot(rocobject10, col='cyan', cex.lab=1.75, cex.axis=1.4)
```

![](Logistic-Reg-for-Traffic_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

``` r
area10<-auc(rocobject10)
area10
```

    ## Area under the curve: 0.601

``` r
## Threshold with highest AUC is 15%, however AUC is still too low, recommendation is to get larger dataset and include additional features no currently present in the dataset that include driver behavior characteristics which are believed to provide higher explanatory value to the dependent variable, thus testing for this model would be redundant (since it has already been determined that model doesn't perform well). Suggested improvements are outside the scope of this project. Next, I end with a couple of metrics for completeness.
```

### Get metrics

``` r
tp<-rocobject15$sensitivities
tn<-rocobject15$specificities

tpr<-tp[2]
tnr<-tn[2]
print(paste('True positive rate:', tpr))
```

    ## [1] "True positive rate: 0.464788732394366"

``` r
print(paste('True negative rate:', tnr))
```

    ## [1] "True negative rate: 0.778008298755187"
