# car-price-prediction-IITB

#BUISNESS UNDERSTANDING#
car is a system
which has input features such as enginesize,body,engine locaton,Brand of car 
output performance are messured by  such as peak rpm,city mpg,horsepower ,highway mpg,symboling 
we have to best establish relation between them to predict price of car!

#METHODS#

Used Multivariate Linear Regression For Price Prediction.
Step AIC for feature selection.
Removal of Variable Based on VIF(Multicollinearity) and P-value.
Used Dummies for conversion of Categorical variable to Numeric.

#PERFORMANCE METRICS#
Accuracy=correlation(test_predicted_price,test_price)^2=.90
This was used as R-Square Measure for test set.
Both R suare were comparable train/test.


RMSE:2520
MAE: 2049

#CONCLUSION#

we can see that engine location slope is -ve values are 1 for front and 0 for back ,so if engine location increases price decrease.
More car width emplies high price
engine size increases price increase
rotor engine describes high price with positive slope
BMW has extra high price.

These 5 variables explain 90% variance in the data.
