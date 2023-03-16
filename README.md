# Logistic-Regression-of-Rubbernecking-on-Freeways
When a major traffic incident occurs on a busy freeway, it has the potential of generating a traffic backup
not only in the direction of the incident, but on the opposite direction of traffic, a phenomenon known as rubbernecking.

The aim of this logistic regression model is to answer the following question: Are there any freeway, traffic, and/or incident characteristics that significantly affect the emergence of rubbernecking on freeways?

Public California state traffic incidents data from major freeways in the Los Angeles, Orange, and San Diego counties were used to train this model. The target variable was presence of rubbernecking (yes/no) as a result of the incident and the attributes included several traffic, freeway, and incident-related characteristics. The model was trained using R, the holdout method was used for testing, and forward selection was used to select attributes to be included in the final model.

Results showed a statistically significant relationship between emergence of rubbernecking queues and the presence of HOV lanes, on-ramps, and percent of trucks. However, the classification model showed a tendency to classify events as negative (no rubbernecking). ROC curves were analyzed to evaluate model accuracy for different probability decision thresholds using AUC as the performance measure. The highest AUC was obtained for the 15% probability threshold with a value of 0.6214, a recall of 46.5% and a true negative rate of 77.8%. These results suggested limitations in the predictive capabilities of the model. A thorough discussion of the data, analysis, and results can be found in the journal publication of this research:
https://doi.org/10.1016/j.trip.2020.100266

This repository includes the present README file, the R code file for the logistic regression model and the ROC curves, and the training data in CSV format.

