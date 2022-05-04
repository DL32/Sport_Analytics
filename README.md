# Sportylytics
## Is the World linear?

### An inquiry into supervised machine learning to determine NBA players salary
Project 3 Group 2 <br />
20630 - Introduction to Sports Analytics <br />
Università Bocconi, Milan
<br />
<br />
**Members:**<br />
Alberto Allegri, Beatrice Guidotti, Federico Leonardi, Tiziano Paci, Jakob Schlierf & Leonardo Yang <br />

**Tech Stack:**<br />
![R](https://img.shields.io/badge/r-%23276DC3.svg?style=for-the-badge&logo=r&logoColor=white)
![Python](https://img.shields.io/badge/python-3670A0?style=for-the-badge&logo=python&logoColor=ffdd54)
 [<img src='https://img.shields.io/badge/heroku-%23430098.svg?style=for-the-badge&logo=heroku&logoColor=white'>](https://sportylytics-predictions.herokuapp.com/)

## Table of Contents
1. [General Info](#general-info)
2. [File Structure](#file-structure)
3. [Models](#models)
4. [Results](#results)


## General Info
This repository serves as the central storage for the files (both code as well as data) for the final project of class 20630 - Introduction to Sports Analytics Spring 2022 at Università Bocconi. In the following, we will explain what files we have provided here, the structure that orders them, which models we included, as well as the results we achieved.  
## File Structure

The repository contains folders for every model, as well as one for the dataset. <br /> Each folder contains both the code, as well as the relevant data files (either the saved models which can be imported instead of retrained to save time, or the scraped and cleaned data). The only exception to this is the Random Forest folder, where the trained models were too big to be stored on Github, so instead they are stored under this [link](https://drive.google.com/drive/folders/1GTHWWY3naEVQNf64pUhkcUv5Qbj_VSdC).<br />
The code files all have relative paths for the import of data, therefore can be used directly without having to change paths once the entire repository is cloned to a local machine. Further, we have included pdf versions of both our intermediate as well as our final presentations, for the sake of completeness. 
Lastly, the deployed heroku app can be reached under this [link](https://sportylytics-predictions.herokuapp.com/).


## Models

We ran 5 different models:
1. **Ordinary Least Squares Regression (OLS)**
2. **K-Nearest Neighbors (KNN)**
3. **Random Forest**
4. **Gradient Boosting**
5. **Support Vector Machines (SVM)**
<br />
In the following, we will give a short introduction to each of the models.

### 1. Ordinary Least Squares Regression (OLS)
The Linear Regression Model has been proposed as our initial benchmark for its ability to achieve quite good and very interpretable results with little effort in terms of training time. Moreover, the possibility to have a linear benchmark also helped us in understanding the improvements achieved when addressing non-linear models. Indeed, we started by running a simple OLS on the whole dataset and then trying to fine-tune it in order to have the best possible initial benchmark. After the first outcome, we noticed issues of multicollinearity across variables which result in making our model unreliable. Therefore, we decided to address the issue in various ways:
Firstly, we manually excluded the variables which provided the highest source of collinearity. However, this approach didn’t help in improving the performance.
Subsequently, we became a bit more formal by applying PCA as a variable reduction technique to understand which components could provide the most relevant information for our purpose. As expected, we eliminated the issue of multicollinearity but performance didn’t improve as we hoped.
So, we tried to apply 4 types of variable selection techniques:
*	Forward stepwise selection
*	Backward stepwise selection
*	Bi-directional stepwise selection
*	Forward stepwise selection (based on the AIC)
<br />
The Forward stepwise selection (based on AIC) turned out working the best among the others and therefore we re-ran the OLS only with the subset of variables selected by the above-mentioned technique. Improvements achieved both in terms of R-Squared and RMSE.
Finally, we decided to apply the best model up to now (the OLS based on the forward stepwise variable selection) but only to the subset of our dataset represented by the last 7 NBA seasons (2013-14 up to 2020-21). This option turned out working particularly well and as expected the model, in this context, managed to understand which variables are becoming the most important in the last years.
<br />
<br />

### 2. K-Nearest Neighbors (KNN)
In the KNN model we estimate the value of a specific data point by looking at the values of data points surrounding it. K in this context is the number of neighbors that we look at to estimate our data point of interest. Since we estimate our data point of interest by majority vote of the surrounding data points, we would want our K to be odd, such that we cannot have a stalemate vote. We ran 5 different versions of this model:
* Simple KNN model with K = 5
* More complex model with 25 resampled bootstrap repetitions with a cross validated K (K = 9)
* Similiarly resampled model where we additionally centered and scaled our features (K = 9)
* A model where we (in addition to the previous preprocessing steps) gave the model a broader range of K to try (K = 11)
* Lastly, we ran the previous model on only the last 7 seasons, which gave us the best results
<br />

### 3. Random Forest
Random Forest is a tree ensembling method, where each tree depends on a random variable. By averaging many trees we can improve predictions performance, however if there are important predictors, trees will be really similar. Thus leading to correlated predictions. Random Forest solves this  problem by decorellating the trees. This is obtained by considering for each split, not all the predictor but just a random subset m ~ sqrt(p).
We ran three different versions of this model (all with tuned hyperparameters):
* Random Forest trained on full dataset (all observations, no filters)
* Random Forest trained using only players who played more than 20 games in a single season (so discarding players whose statistics where not fully reliable)
* Lastly, Random Forest trained using only the last 7 seasons (from 2014-2015 to 2020-2021), which gave us the best results
<br />

### 4. Gradient Boosting
Gradient Boosting is an iterative functional gradient algorithm, i.e an algorithm which minimizes a loss function by iteratively choosing a function that points towards the negative gradient. It is key to underline the fact that, as opposed to Random Forest, it trains many models in a gradual, additive and **sequential** manner. This means that Gradient Boosting is based on the **errors** made by the previous decision tree, and sequentially tries to improve performances and reduce errors. We ran three different versions of this model (all with tuned hyperparameters):
* Gradient Boosting trained on full dataset (all observations, no filters)
* Gradient Boosting trained using only players who played more than 20 games in a single season (so discarding players whose statistics where not fully reliable)
* Lastly, Gradient Boosting trained using only the last 7 seasons (from 2014-2015 to 2020-2021), which gave us the best results
<br />

### 5. Support Vector Machines (SVM)
To understand how a SVM works, let's start from a two classes classification problem in a n-dimensional space. SVM tries to find a hyperplane (n-1 dimensions plane) that separates these two classes. Then it classifies the new point depending on whether it lies on the positive or negative side of the hyperplane depending on the classes to predict. In the regression case, we have to find a function that approximates mapping from an input domain to real numbers on the basis of a training sample. We ran three different versions of this model (all with tuned hyperparameters):
* SVM trained on full dataset (all observations, no filters)
* SVM trained using only players who played more than 20 games in a single season (so discarding players whose statistics where not fully reliable)
* Lastly, SVM trained using only the last 7 seasons (from 2014-2015 to 2020-2021), which gave us the best results
<br />

## Results
The below tables details the RMSE & R&#x00B2; score for each of the models that we ran. The score presented is the that of the respective best version of that model.
| Model             | RMSE  | R&#x00B2;  |
| :---------------: | :----:| :--------: |
| **OLS**           | 0.040 | 0.715 |
| **KNN**           | 0.033 | 0.808 |
| **Random Forest**   | 0.035 | 0.765 |
| **Gradient Boosting** | 0.036 | 0.777 |
| **SVM**           | 0.032 | 0.821|

<br />
<br />

A short interpretation of the results shows that: <br />
* If we fit well-performing model to only the last 7 seasons (in line with the experts opinion on when the increased focus for 3pt shots happened), we see an increase in model performance
* The most important variables proved to be minutes played, points scored, age as well as our contract variables
* Advanced Metrics do not perform as well as expected,  instead, we see a market reliance on standard metrics
* If we compare variable importance for the whole dataset and only the last 7 seasons, we see that the biggest increase in importance is 3pt% (11x), Box Plus-Minus (8x), and Defensive Win Shares (7.9x)

