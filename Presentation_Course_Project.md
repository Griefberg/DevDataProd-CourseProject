Developing Data Products - Course Project
========================================================
author: Roman Solov'ev
date: 22.03.2015

Income Census Data Analysis
========================================================

This presentation is designed as part of the course project for the coursera developing data products class. The following instruments are used in the process:

- [Shiny](http://shiny.rstudio.com/) to build an application
- R presentation for creating a presentation (sorry for battology)

The Application
========================================================

Application called Income Census Data Analysis has been developed and deployed at https://griefberg.shinyapps.io/Course_project_app_final/

(You have to give some time for an application to load, because there is a lot of calculations there)


The application allows the user to:

- Understand socio-economic chracteristics of the US population relatilve to its income level
- Predict your own potential income level using CART algorithm

Data Source
========================================================

This application is based on the 1996 U.S. Census Income Data set.

Dataset has been obtained from [here](http://archive.ics.uci.edu/ml/datasets/Census+Income) and processed for the course project.

Source code for the project is available on the [GitHub](https://github.com/Griefberg/DevDataProd-CourseProject).

Data 
========================================================


```
'data.frame':	32561 obs. of  9 variables:
 $ age           : int  39 50 38 53 28 37 49 52 31 42 ...
 $ education     : Factor w/ 16 levels "10th","11th",..: 10 10 12 2 10 13 7 12 13 10 ...
 $ marital_status: Factor w/ 7 levels "Divorced","Married-AF-spouse",..: 5 3 1 3 3 3 4 3 5 3 ...
 $ occupation    : Factor w/ 15 levels "?","Adm-clerical",..: 2 5 7 7 11 5 9 5 11 5 ...
 $ race          : Factor w/ 5 levels "Amer-Indian-Eskimo",..: 5 5 5 3 3 5 3 5 5 5 ...
 $ sex           : Factor w/ 2 levels "Female","Male": 2 2 2 2 1 1 1 2 1 2 ...
 $ hours_per_week: int  40 13 40 40 40 40 16 45 50 40 ...
 $ native_country: Factor w/ 42 levels "?","Cambodia",..: 40 40 40 40 6 40 24 40 40 40 ...
 $ income        : Factor w/ 2 levels "<=50K",">50K": 1 1 1 1 1 1 1 2 2 2 ...
```
