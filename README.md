# Data Science Lab project

This repository contains a project developed as part of *Data Science Lab* course of the [Master's degree in Data Science](https://www.unimib.it/graduate/data-science),
University of Milano Bicocca.

## Description

This project aims at analyzing and forecasting daily revenue and the daily number of receipts across six distinct restaurants. By employing a statistical approach and utilizing predictive models, particularly the SARIMA and TBATS models, the focuse is on analyzing historical data related to daily revenue and the number of receipts in each restaurant and formulating future forecasts. Finally, the results of the two models are compared to potentially identify the better-performing one.

## Repository structure

This repository is structured as follows: 

```
.
├── DATA_PREPROCESSING_EXPLORATORY_ANALYSIS.ipynb   # preprocessing operations and exploratory data analysis
├── FORECAST MODELS.R                               # forecast models script in R language 
├── ORGANIZED DATASETS                              # data used (daily revenue and recepits) 
│   ├── fat_r000.csv                                 # revenue data related to the first restaurant
│   ├── fat_r001.csv                                 # revenue data related to the second restaurant
│   ├── fat_r002.csv                                 # revenue data related to the third restaurant
│   ├── fat_r003.csv                                 # revenue data related to the fourth restaurant
│   ├── fat_r004.csv                                 # revenue data related to the fifth restaurant
│   ├── fat_r005.csv                                 # revenue data related to the sixth restaurant
│   ├── fatturato.csv                                # revenue data related to all restaurants
│   ├── scon_r000.csv                                # recepit data related to the first restaurant
│   ├── scon_r001.csv                                # recepit data related to the second restaurant
│   ├── scon_r002.csv                                # recepit data related to the third restaurant
│   ├── scon_r003.csv                                # recepit data related to the fourth restaurant
│   ├── scon_r004.csv                                # recepit data related to the fifth restaurant
│   ├── scon_r005.csv                                # recepit data related to the sixth restaurant
│   └── scontrini.csv                                # recepit data related to all restaurants
└── README.md
```


For the forecast models, the R code is provided as an attachment. This code is specific to a single restaurant. You can change the restaurant by modifying the .csv file to be loaded and then replacing "R00_" with the corresponding number in the code lines marked with "*******CHANGE RESTAURANT NUMBER HERE*******".
\
\
The code is divided into two parts: one for the analysis with pre and post COVID-19 data and one for the analysis with only post COVID-19 data. Each part is further divided into REVENUE and RECEIPTS, and within those, SARIMA and TBATS models are used separately.






