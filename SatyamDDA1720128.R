#################################################Reading The CSV FILE#################################################
car_price=read.csv('CarPrice_Assignment.csv',header = T)
####Check For Total NAS and duplicates
sum(is.na(car_price))
sum(duplicated(car_price))
#### NO NAS and duplicated values#######
###buisness understanding::\\

###  in this problem
### car is a system
##which has input features such as enginesize,body,engine locaton,Brand of car 
##output performance are messured by  such as peak rpm,city mpg,horsepower ,highway mpg,symboling 
## we have to best establish relation between them to predict price of car!

##basic data cleaning############# 
str(car_price)
name_cars=data.frame(do.call('rbind',strsplit(as.character(car_price$CarName),' ',fixed=T)))
library(plyr)
count(name_cars$X1)
##As we can see that some are upper case and some are lower case in Nisaan.porsche,volkswagen,toyota are named differtly
##we have to look at that ambiguity
name_cars$X1=tolower(name_cars$X1)
name_cars_make1=gsub("porcshce","porsche",name_cars$X1)
name_cars_make2=gsub("vokswagen","volkswagen",name_cars_make1)
name_cars_make3=gsub("maxda","mazda",name_cars_make2)
name_cars_make4=gsub("vw","volvo",name_cars_make3)
name_cars_make_final=gsub("toyouta","toyota",name_cars_make4)
count(name_cars_make_final)
car_price$Make_name=name_cars_make_final
rm(name_cars_make1,name_cars_make2,name_cars_make3,name_cars_make4,name_cars_make_final,name_cars)

##### Now data is cleaned at initial level  and EDA ##########################################################################
#univariate analysis and dealing with outliers##
library(ggplot2)
univariate_price <- function(category1)
{
  
  p1=ggplot(car_price,aes(category1,price))
  p2=p1+geom_boxplot()
  
  return (p2)  
}

univariate_price(as.factor(car_price$symboling))
### we can see that symbols cant be most significant for price
univariate_price(as.factor(car_price$Make_name))
##we can see  that BMW,BUICK,JAGUAR,PORCHE are high priced while rest are priced less than 20000
univariate_price(car_price$aspiration)
##we can see  turbo are slightly more priced
univariate_price(car_price$doornumber)
##number of doors arent significant
univariate_price(car_price$carbody)
##convertible and hardtop are slightly more priced
univariate_price(car_price$drivewheel)
##rwd is more priced than others drive wheel
univariate_price(car_price$enginelocation)
## rear engine location cars are more priced
univariate_price(car_price$cylindernumber)
## As number of cylinder increases price increases from the plot
univariate_price(car_price$fuelsystem)
## mpfi,mfi and idi  cars are more priced

##UNIVARIATE ANALYSI DONE ON CATEGORICAL VARIABLES


###UNIVARIATE ANALYSIS ON NUMErical Variables


library(ggplot2)
univariate_price_measure <- function(numeric_var)
{
  
  p1=ggplot(car_price,aes(numeric_var,price))
  p2=p1+geom_point()
  
  return (p2)  
}


univariate_price_measure(car_price$wheelbase)
##we can see that as wheel base increases price increases in general

univariate_price_measure(car_price$carlength)
##we can see that as car length increases price increases in general with some non linearity

univariate_price_measure(car_price$carwidth)
##we can see that as car width increases price increases in general

univariate_price_measure(car_price$carheight)
##we can see that as car height has no effect in price 

univariate_price_measure(car_price$curbweight)
##It is very signifiucant in deciding price of car as it has high amount of linearity with price
## when curb weight increses price increses

univariate_price_measure(car_price$enginesize)
##we can see that engine size is also having siginifact postive linear relationship with price

univariate_price_measure(car_price$boreratio)
##we can slightly linear relationship of bore ratio with car price



univariate_price_measure(car_price$stroke)
##Not a godd relationship of stroke with price
univariate_price_measure(car_price$compressionratio)
##Not good in deciding price
univariate_price_measure(car_price$horsepower)
##we can see that as horsepower  increases price increases in general
univariate_price_measure(car_price$peakrpm)
#No linear relatoionsghip not significat
univariate_price_measure(car_price$citympg)
##we can see that as city mileage increase price decreases as high price cars give less milegae
univariate_price_measure(car_price$highwaympg)
##we can see that as highway mileage increase price decreases as high price cars give less milegae!


##multivariate analysis
cor_numeric_price=car_price[,c(2,10,11,12,13,14,17,19:26)]
corrl=cor(cor_numeric_price)
corrplot(corrl,method = 'circle')

##we can see that price is highly positively correlated with carlength,carwidth,curbweight,engine size and 
## highly negatively correlated with city mpg and highway mpg
##moderately corrlated with bore ratio and wheel base

##The highest amount of correlation is with engine size(0.87)\
##But engine size is correlated with curb weight,carlength,carwidth,city mpg and highway mpg
## we have to take care of these correlated terms while buiding model
##As if they highly corrleated then it can give rise toi multi collinearity
## 




##data Preparation####
#### creating data in numeric for Model buiding and dummy variable####################
car_price1=car_price
summary(car_price1$symboling)
summary(factor(car_price1$symboling))
str(car_price1$fueltype)

str(factor(car_price1$fueltype))
#1 diseal 0 #gas
levels(car_price1$fueltype)<-c(1,0)
str(car_price1$fueltype)
car_price1$fueltype<- as.numeric(levels(car_price1$fueltype))[car_price1$fueltype]
table(car_price1$fueltype)

str(car_price1$aspiration)
#0 turbo 1 #std
levels(car_price1$aspiration)<-c(1,0)
str(car_price1$aspiration)
car_price1$aspiration<- as.numeric(levels(car_price1$aspiration))[car_price1$aspiration]
table(car_price1$aspiration)

str(car_price1$doornumber)
#1 four 0 #two
levels(car_price1$doornumber)<-c(1,0)
str(car_price1$doornumber)
car_price1$doornumber<- as.numeric(levels(car_price1$doornumber))[car_price1$doornumber]
table(car_price1$doornumber)


str(car_price1$enginelocation)
#1 front 0 #rear
levels(car_price1$enginelocation)<-c(1,0)
str(car_price1$enginelocation)
car_price1$enginelocation<- as.numeric(levels(car_price1$enginelocation))[car_price1$enginelocation]
table(car_price1$enginelocation)


dummy_carbody<- data.frame(model.matrix( ~carbody, data = car_price1))

#check the dummy_carbody data frame.
#View(dummy_carbody)

#This column should be removed from the newly created dummy_carbody dataframe containing the dummy values for the variable "fusrnishingstatus". 
dummy_carbody <- dummy_carbody[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "carbody" column
car_price1 <- cbind(car_price1[,-7], dummy_carbody)
#View(car_price1)

str(car_price1)

str(car_price1$drivewheel)

dummy_drivewheel<- data.frame(model.matrix( ~drivewheel, data = car_price1))

#check the dummy_drivewheel data frame.
#View(dummy_drivewheel)

#This column should be removed from the newly created dummy_drivewheel dataframe containing the dummy values for the variable "fusrnishingstatus". 
dummy_drivewheel <- dummy_drivewheel[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "drivewheel" column
car_price1 <- cbind(car_price1[,-7], dummy_drivewheel)
#View(car_price1)


str(car_price1)

str(car_price1$enginetype)

dummy_enginetype<- data.frame(model.matrix( ~enginetype, data = car_price1))

#check the dummy_enginetype data frame.
#View(dummy_enginetype)

#This column should be removed from the newly created dummy_enginetype dataframe containing the dummy values for the variable "fusrnishingstatus". 
dummy_enginetype <- dummy_enginetype[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "enginetype" column
car_price1 <- cbind(car_price1[,-13], dummy_enginetype)
#View(car_price1)
str(car_price1)

str(car_price1$cylindernumber)

dummy_cylindernumber<- data.frame(model.matrix( ~cylindernumber, data = car_price1))

#check the dummy_cylindernumber data frame.
#View(dummy_cylindernumber)

#This column should be removed from the newly created dummy_cylindernumber dataframe containing the dummy values for the variable "fusrnishingstatus". 
dummy_cylindernumber <- dummy_cylindernumber[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "cylindernumber" column
car_price1 <- cbind(car_price1[,-13], dummy_cylindernumber)
#View(car_price1)
str(car_price1)


str(car_price1$fuelsystem)

dummy_fuelsystem<- data.frame(model.matrix( ~fuelsystem, data = car_price1))

#check the dummy_fuelsystem data frame.
#View(dummy_fuelsystem)

#This column should be removed from the newly created dummy_fuelsystem dataframe containing the dummy values for the variable "fusrnishingstatus". 
dummy_fuelsystem <- dummy_fuelsystem[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "fuelsystem" column
car_price1 <- cbind(car_price1[,-14], dummy_fuelsystem)
#View(car_price1)
str(car_price1)


str(car_price1$Make_name)
car_price1$Make_name=as.factor(car_price1$Make_name)
dummy_Make_name<- data.frame(model.matrix( ~Make_name, data = car_price1))

#check the dummy_Make_name data frame.
#View(dummy_Make_name)

#This column should be removed from the newly created dummy_Make_name dataframe containing the dummy values for the variable "fusrnishingstatus". 
dummy_Make_name <- dummy_Make_name[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "Make_name" column
car_price1 <- cbind(car_price1[,-22], dummy_Make_name)
#View(car_price1)
str(car_price1)
car_clean=car_price1[,-c(1,3)]     ##no use car name and car id as we have already made a new Make_name and car id is just unique id
rm(dummy_carbody,dummy_cylindernumber,dummy_drivewheel,dummy_enginetype,dummy_fuelsystem,dummy_Make_name)





###ouliers testing and significance if unexpected valuesare present
quantile(car_clean$wheelbase,seq(0,1,0.01))##NO peak increase  
quantile(car_clean$carlength,seq(0,1,0.01))####NO peak increase
quantile(car_clean$carwidth,seq(0,1,0.01))##NO peak increase
quantile(car_clean$carheight,seq(0,1,0.01))##NO peak increase
quantile(car_clean$curbweight,seq(0,1,0.01))##NO peak increase
#we can expensive cars can have high curb weight therefore there is sudden rise at beyond 98%,There are not considered as 
#ouliers according to me
quantile(car_clean$enginesize,seq(0,1,0.01))
#we can expensive cars can have large engine size therefore there is sudden rise at beyond 98%,There are not considered as 
#ouliers according to me
quantile(car_clean$boreratio,seq(0,1,0.01))
quantile(car_clean$stroke,seq(0,1,0.01))
quantile(car_clean$compressionratio,seq(0,1,0.01))
quantile(car_clean$horsepower,seq(0,1,0.01))
## with large engine size they might have possiblity of high horesepower which is reflected in beyond 99%
quantile(car_clean$peakrpm,seq(0,1,0.01))
## with large engine engine size might be peak RPM is greater according to buisness understanding
## so we cant treat them as outliers
quantile(car_clean$citympg,seq(0,1,0.01))
## Car which are fuel efficient can give mpg as 49 cant be treated as ouliers
quantile(car_clean$highwaympg,seq(0,1,0.01))
## same applies for this
quantile(car_clean$price,seq(0,1,0.01))
## we can see there are high priced car beyond 98% which had been taken care by Company of car dummy variables
## we dont need to remove them

## We can see that there are sharp increase in some of variables beyond 98% but they should be present as
## high priced car are possible in our case and can have large engine and above staed variables

## Rest all categorical variables made numeric no need to check outliers for them
###################################Linear Regression Model Buiding#################################
set.seed(50)

trainindices= sample(1:nrow(car_clean), 0.7*nrow(car_clean))
# generate the train data set
train3 = car_clean[trainindices,]

#Similarly store the rest of the observations into an object "test3".
test3 = car_clean[-trainindices,]
# as engine size is highest correlated with price,and from univariate analysis also lets do LR on Engine size
slr1=lm(price~enginesize,data=train3)
summary(slr1)

Predict_3 <- predict(slr1,test3[,-19])
test3$test_price <- Predict_3

# Accuracy of the predictions
# Calculate correlation
r3 <- cor(test3$price,test3$test_price)
# calculate R squared by squaring correlation
r3squared <- cor(test3$price,test3$test_price)^2

# check R-squared
r3squared
## On engine size it is giving 85 % accuracy in test set
error3 <- test3$price-test3$test_price
rmse <- function(error)
{
  sqrt(mean(error^2))
}

mae <- function(error)
{
  mean(abs(error))
}
#rmse(error)
#mae(error)
rmse(error3)
mae(error3)

## lets do LR on curb weight and price 

set.seed(999)

trainindices= sample(1:nrow(car_clean), 0.7*nrow(car_clean))
# generate the train data set
train4 = car_clean[trainindices,]

#Similarly store the rest of the observations into an object "test3".
test4 = car_clean[-trainindices,]
# as engine size is highest correlated with price lets do LR on Engine size
slr2=lm(price~curbweight,data=train4)
summary(slr2)

Predict_4 <- predict(slr2,test4[,-19])
test4$test_price <- Predict_4

# Accuracy of the predictions
# Calculate correlation
r4 <- cor(test4$price,test4$test_price)
# calculate R squared by squaring correlation
r4squared <- cor(test4$price,test4$test_price)^2

# check R-squared
r4squared
## On cur weight  it is giving 70 % accuracy in test set
error4 <- test4$price-test4$test_price

rmse(error4)
mae(error4)


## so we can say thate ngine size is stringest predictor of the price of the vehicle.
## we can check further by looking to built multiple linear regression if Test r square and rmse and mse improves!





###############################Model BUIding  Multivariate Analysis##############################################################

# Divide into training and test data set
#set the seed to 1000, let's run it 
set.seed(1000)

# randomly generate row indices for train dataset
trainindices= sample(1:nrow(car_clean), 0.7*nrow(car_clean))
# generate the train data set
train = car_clean[trainindices,]

#Similarly store the rest of the observations into an object "test".
test = car_clean[-trainindices,]


lm1=lm(price~.,data=train)
summary(lm1)
library(car)
library(MASS)


step <- stepAIC(lm1, direction="both")
#now we need to know our model equation so lets write the Step command here. 
step

lm2=lm(price ~ fueltype + aspiration + enginelocation + 
            wheelbase + carlength + carwidth + carheight + curbweight + 
            enginesize + boreratio + stroke + compressionratio + peakrpm + 
            carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
            drivewheelrwd + enginetypel + enginetypeohcf + enginetyperotor + 
            cylindernumberfive + cylindernumberfour + cylindernumbersix + 
            cylindernumberthree + fuelsystemmfi + fuelsystemmpfi + fuelsystemspdi + 
            Make_nameaudi + Make_namebmw + Make_namebuick + Make_namechevrolet + 
            Make_namedodge + Make_namehonda + Make_namejaguar + Make_namemitsubishi + 
            Make_nameplymouth + Make_nameporsche + Make_namesaab + Make_namevolvo, 
          data = train)

summary(lm2)
vif(lm2)

## we can see that fuel type has high P value and high VIF
## removing it and making new model




lm3=lm(price ~aspiration + enginelocation + 
         wheelbase + carlength + carwidth + carheight + curbweight + 
         enginesize + boreratio + stroke + compressionratio + peakrpm + 
         carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
         drivewheelrwd + enginetypel + enginetypeohcf + enginetyperotor + 
         cylindernumberfive + cylindernumberfour + cylindernumbersix + 
         cylindernumberthree + fuelsystemmfi + fuelsystemmpfi + fuelsystemspdi + 
         Make_nameaudi + Make_namebmw + Make_namebuick + Make_namechevrolet + 
         Make_namedodge + Make_namehonda + Make_namejaguar + Make_namemitsubishi + 
         Make_nameplymouth + Make_nameporsche + Make_namesaab + Make_namevolvo, 
       data = train)
summary(lm3)
vif(lm3)

## we can see car lenghhas high p value and More vif we can remove this


lm4=lm(price ~  aspiration + enginelocation + 
         wheelbase  + carwidth + carheight + curbweight + 
         enginesize + boreratio + stroke + compressionratio + peakrpm + 
         carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
         drivewheelrwd + enginetypel + enginetypeohcf + enginetyperotor + 
         cylindernumberfive + cylindernumberfour + cylindernumbersix + 
         cylindernumberthree + fuelsystemmfi + fuelsystemmpfi + fuelsystemspdi + 
         Make_nameaudi + Make_namebmw + Make_namebuick + Make_namechevrolet + 
         Make_namedodge + Make_namehonda + Make_namejaguar + Make_namemitsubishi + 
         Make_nameplymouth + Make_nameporsche + Make_namesaab + Make_namevolvo, 
       data = train)
summary(lm4)
vif(lm4)

## we can drivewheelrwd has more vif and more p vlaue we can remove this


lm5=lm(price ~aspiration + enginelocation + 
         wheelbase+ carwidth + carheight + curbweight + 
         enginesize + boreratio + stroke + compressionratio + peakrpm + 
         carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
         enginetypel + enginetypeohcf + enginetyperotor + 
         cylindernumberfive + cylindernumberfour + cylindernumbersix + 
         cylindernumberthree + fuelsystemmfi + fuelsystemmpfi + fuelsystemspdi + 
         Make_nameaudi + Make_namebmw + Make_namebuick + Make_namechevrolet + 
         Make_namedodge + Make_namehonda + Make_namejaguar + Make_namemitsubishi + 
         Make_nameplymouth + Make_nameporsche + Make_namesaab + Make_namevolvo, 
       data = train)

summary(lm5)
vif(lm5)




## we can see engine type has both high p value and high VIf
lm6=lm(price ~aspiration + enginelocation + 
         wheelbase+ carwidth + carheight + curbweight + 
         enginesize + boreratio + stroke + compressionratio + peakrpm + 
         carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
         +enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + cylindernumbersix + 
         cylindernumberthree + fuelsystemmfi + fuelsystemmpfi + fuelsystemspdi + 
         Make_nameaudi + Make_namebmw + Make_namebuick + Make_namechevrolet + 
         Make_namedodge + Make_namehonda + Make_namejaguar + Make_namemitsubishi + 
         Make_nameplymouth + Make_nameporsche + Make_namesaab + Make_namevolvo, 
       data = train)

summary(lm6)
vif(lm6)


#we can see that fuel system pfi has high vif and high P value 
lm7=lm(price ~aspiration + enginelocation + 
         wheelbase+ carwidth + carheight + curbweight + 
         enginesize + boreratio + stroke + compressionratio + peakrpm + 
         carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
         + enginetypeohcf + enginetyperotor + 
         cylindernumberfive + cylindernumberfour + cylindernumbersix + 
         cylindernumberthree + fuelsystemmfi + fuelsystemmpfi  + 
         Make_nameaudi + Make_namebmw + Make_namebuick + Make_namechevrolet + 
         Make_namedodge + Make_namehonda + Make_namejaguar + Make_namemitsubishi + 
         Make_nameplymouth + Make_nameporsche + Make_namesaab + Make_namevolvo, 
       data = train)

summary(lm7)
vif(lm7)






## Make_name Jaguar is high P value and VIF removing it

lm8=lm(price ~aspiration + enginelocation + 
         wheelbase+ carwidth + carheight + curbweight + 
         enginesize + boreratio + stroke + compressionratio + peakrpm + 
         carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
         + enginetypeohcf + enginetyperotor + 
         cylindernumberfive + cylindernumberfour + cylindernumbersix + 
         cylindernumberthree + fuelsystemmfi + fuelsystemmpfi  + 
         Make_nameaudi + Make_namebmw + Make_namebuick + Make_namechevrolet + 
         Make_namedodge + Make_namehonda + Make_namemitsubishi + 
         Make_nameplymouth + Make_nameporsche + Make_namesaab + Make_namevolvo, 
       data = train)

summary(lm8)
vif(lm8)


## we can see that cylinder number six is having p value grater than 0.05 and high VIF,Removing it

lm9=lm(price ~aspiration + enginelocation + 
         wheelbase+ carwidth + carheight + curbweight + 
         enginesize + boreratio + stroke + compressionratio + peakrpm + 
         carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
         + enginetypeohcf + enginetyperotor + 
         cylindernumberfive + cylindernumberfour +  
         cylindernumberthree + fuelsystemmfi + fuelsystemmpfi  + 
         Make_nameaudi + Make_namebmw + Make_namebuick + Make_namechevrolet + 
         Make_namedodge + Make_namehonda + Make_namemitsubishi + 
         Make_nameplymouth + Make_nameporsche + Make_namesaab + Make_namevolvo, 
       data = train)

summary(lm9)
vif(lm9)
## we can see that P value of Make Audi is Very high and Vif is around 3 we can remove it 
lm10=lm(price ~aspiration + enginelocation + 
         wheelbase+ carwidth + carheight + curbweight + 
         enginesize + boreratio + stroke + compressionratio + peakrpm + 
         carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
         + enginetypeohcf + enginetyperotor + 
         cylindernumberfive + cylindernumberfour +  
         cylindernumberthree + fuelsystemmfi + fuelsystemmpfi  + 
         Make_namebmw + Make_namebuick + Make_namechevrolet + 
         Make_namedodge + Make_namehonda + Make_namemitsubishi + 
         Make_nameplymouth + Make_nameporsche + Make_namesaab + Make_namevolvo, 
       data = train)

summary(lm10)
vif(lm10)


##make chevrolet has vigf greater than 2 but high P value we have to remove it
lm11=lm(price ~aspiration + enginelocation + 
         wheelbase+ carwidth + carheight + curbweight + 
         enginesize + boreratio + stroke + compressionratio + peakrpm + 
         carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
         + enginetypeohcf + enginetyperotor + 
         cylindernumberfive + cylindernumberfour +  
         cylindernumberthree + fuelsystemmfi + fuelsystemmpfi   + Make_namebmw + Make_namebuick  + 
         Make_namedodge + Make_namehonda + Make_namemitsubishi + 
         Make_nameplymouth + Make_nameporsche + Make_namesaab + Make_namevolvo, 
       data = train)

summary(lm11)
vif(lm11)
##Not any significant change in Adjusted R square



#compression ratio has very high p value and vif greater 2.5 
#we can remove it
lm12=lm(price ~aspiration + enginelocation + 
         wheelbase+ carwidth + carheight + curbweight + 
         enginesize + boreratio + stroke +  peakrpm + 
         carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
         + enginetypeohcf + enginetyperotor + 
         cylindernumberfive + cylindernumberfour +  
         cylindernumberthree + fuelsystemmfi + fuelsystemmpfi  + 
          Make_namebmw + Make_namebuick + 
         Make_namedodge + Make_namehonda + Make_namemitsubishi + 
         Make_nameplymouth + Make_nameporsche + Make_namesaab + Make_namevolvo, 
       data = train)

summary(lm12)
vif(lm12)

##now fuel system mmpfi has large pvalue>0.05 and high vif>3 we can remove it

lm13=lm(price ~aspiration + enginelocation + 
          wheelbase+ carwidth + carheight + curbweight + 
          enginesize + boreratio + stroke +  peakrpm + 
          carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
          + enginetypeohcf + enginetyperotor + 
          cylindernumberfive + cylindernumberfour +  
          cylindernumberthree + fuelsystemmfi  
          +Make_namebmw + Make_namebuick + 
          Make_namedodge + Make_namehonda + Make_namemitsubishi + 
          Make_nameplymouth + Make_nameporsche + Make_namesaab + Make_namevolvo, 
        data = train)

summary(lm13)
vif(lm13)

## now we have to check for highest p value and adjusted R square
##removing fuel mmfi having high p value
lm14=lm(price ~aspiration + enginelocation + 
          wheelbase+ carwidth + carheight + curbweight + 
          enginesize + boreratio + stroke +  peakrpm + 
          carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
          + enginetypeohcf + enginetyperotor + 
          cylindernumberfive + cylindernumberfour +  
          cylindernumberthree+  
          Make_namebmw + Make_namebuick + 
          Make_namedodge + Make_namehonda + Make_namemitsubishi + 
          Make_nameplymouth + Make_nameporsche + Make_namesaab + Make_namevolvo, 
        data = train)

summary(lm14)
vif(lm14)


##no significant change in adjusted r sQUARE

##removing wheel base having high vif and high p value 


lm15=lm(price ~aspiration + enginelocation + 
          carwidth + carheight + curbweight + 
          enginesize + boreratio + stroke +  peakrpm + 
          carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
          + enginetypeohcf + enginetyperotor + 
          cylindernumberfive + cylindernumberfour +  
          cylindernumberthree + 
          Make_namebmw + Make_namebuick + 
          Make_namedodge + Make_namehonda + Make_namemitsubishi + 
          Make_nameplymouth + Make_nameporsche + Make_namesaab + Make_namevolvo, 
        data = train)

summary(lm15)
vif(lm15)

##car body sedan is having high p value
lm16=lm(price ~aspiration + enginelocation + 
          carwidth + carheight + curbweight + 
          enginesize + boreratio + stroke +  peakrpm + 
          carbodyhardtop + carbodyhatchback + carbodywagon
          + enginetypeohcf + enginetyperotor + 
          cylindernumberfive + cylindernumberfour +  
          cylindernumberthree + 
          Make_namebmw + Make_namebuick + 
          Make_namedodge + Make_namehonda + Make_namemitsubishi + 
          Make_nameplymouth + Make_nameporsche + Make_namesaab + Make_namevolvo, 
        data = train)

summary(lm16)
#NO SIGNIFICANT CHANGE IN r SUARE

##car body wagon is having high p value
lm17=lm(price ~aspiration + enginelocation + 
          carwidth + carheight + curbweight + 
          enginesize + boreratio + stroke +  peakrpm + 
          carbodyhardtop + carbodyhatchback 
        + enginetypeohcf + enginetyperotor + 
          cylindernumberfive + cylindernumberfour +  
          cylindernumberthree + 
          Make_namebmw + Make_namebuick + 
          Make_namedodge + Make_namehonda + Make_namemitsubishi + 
          Make_nameplymouth + Make_nameporsche + Make_namesaab + Make_namevolvo, 
        data = train)

summary(lm17)
vif(lm17)

##no signignificant effect in sdjusted R square


##car body hardtopis having high p value
lm18=lm(price ~aspiration + enginelocation + 
          carwidth + carheight + curbweight + 
          enginesize + boreratio + stroke +  peakrpm + 
          carbodyhatchback 
        + enginetypeohcf + enginetyperotor + 
          cylindernumberfive + cylindernumberfour +  
          cylindernumberthree + 
          Make_namebmw + Make_namebuick + 
          Make_namedodge + Make_namehonda + Make_namemitsubishi + 
          Make_nameplymouth + Make_nameporsche + Make_namesaab + Make_namevolvo, 
        data = train)

summary(lm18)
vif(lm18)

##no significant drop in adjusted R square


## car body hatchback has high p value


lm19=lm(price ~aspiration + enginelocation + 
          carwidth + carheight + curbweight + 
          enginesize + boreratio + stroke +  peakrpm + 
          enginetypeohcf + enginetyperotor + 
          cylindernumberfive + cylindernumberfour +  
          cylindernumberthree + 
          Make_namebmw + Make_namebuick + 
          Make_namedodge + Make_namehonda + Make_namemitsubishi + 
          Make_nameplymouth + Make_nameporsche + Make_namesaab + Make_namevolvo, 
        data = train)

summary(lm19)
vif(lm19)




## aspiration has effect on Adjusted R square lets check it


lm20=lm(price ~ enginelocation + 
          carwidth + carheight + curbweight + 
          enginesize + boreratio + stroke +  peakrpm + 
          enginetypeohcf + enginetyperotor + 
          cylindernumberfive + cylindernumberfour +  
          cylindernumberthree + 
          Make_namebmw + Make_namebuick + 
          Make_namedodge + Make_namehonda + Make_namemitsubishi + 
          Make_nameplymouth + Make_nameporsche + Make_namesaab + Make_namevolvo, 
        data = train)

summary(lm20)
vif(lm20)

## no significant drop in adjusted R square

## make name Plymounth has low p value lets check if it has significant drop in adjusted R square

lm21=lm(price ~ enginelocation + 
          carwidth + carheight + curbweight + 
          enginesize + boreratio + stroke +  peakrpm + 
          enginetypeohcf + enginetyperotor + 
          cylindernumberfive + cylindernumberfour +  
          cylindernumberthree + 
          Make_namebmw + Make_namebuick + 
          Make_namedodge + Make_namehonda + Make_namemitsubishi + 
          Make_nameporsche + Make_namesaab + Make_namevolvo, 
        data = train)

summary(lm21)
vif(lm21)

# no significant drop in adjusted R square
# lets check for Make_name dodge if its afftect the Adjusted R square


lm22=lm(price ~ enginelocation + 
          carwidth + carheight + curbweight + 
          enginesize + boreratio + stroke +  peakrpm + 
          enginetypeohcf + enginetyperotor + 
          cylindernumberfive + cylindernumberfour +  
          cylindernumberthree + 
          Make_namebmw + Make_namebuick + 
          Make_namehonda + Make_namemitsubishi + 
          Make_nameporsche + Make_namesaab + Make_namevolvo, 
        data = train)

summary(lm22)
vif(lm22)
# no significant drop in adjusted R square

# lets check for Make_name honda if its afftect the Adjusted R square


lm23=lm(price ~ enginelocation + 
          carwidth + carheight + curbweight + 
          enginesize + boreratio + stroke +  peakrpm + 
          enginetypeohcf + enginetyperotor + 
          cylindernumberfive + cylindernumberfour +  
          cylindernumberthree + 
          Make_namebmw + Make_namebuick + 
          Make_namemitsubishi + 
          Make_nameporsche + Make_namesaab + Make_namevolvo, 
        data = train)

summary(lm23)
vif(lm23)
# no significant drop in adjusted R square

# lets check for Peak Rpm if its afftect the Adjusted R square


lm24=lm(price ~ enginelocation + 
          carwidth + carheight + curbweight + 
          enginesize + boreratio + stroke + 
          enginetypeohcf + enginetyperotor + 
          cylindernumberfive + cylindernumberfour +  
          cylindernumberthree + 
          Make_namebmw + Make_namebuick + 
          Make_namemitsubishi + 
          Make_nameporsche + Make_namesaab + Make_namevolvo, 
        data = train)

summary(lm24)
vif(lm24)
# no significant drop in adjusted R square



# lets check for Make BUick if its afftect the Adjusted R square


lm25=lm(price ~ enginelocation + 
          carwidth + carheight + curbweight + 
          enginesize + boreratio + stroke + 
          enginetypeohcf + enginetyperotor + 
          cylindernumberfive + cylindernumberfour +  
          cylindernumberthree + 
          Make_namebmw+ 
          Make_namemitsubishi + 
          Make_nameporsche + Make_namesaab + Make_namevolvo, 
        data = train)

summary(lm25)
vif(lm25)
# no significant drop in adjusted R square


#lets check for Make mitsubishi if its afftect the Adjusted R square


lm26=lm(price ~ enginelocation + 
          carwidth + carheight + curbweight + 
          enginesize + boreratio + stroke + 
          enginetypeohcf + enginetyperotor + 
          cylindernumberfive + cylindernumberfour +  
          cylindernumberthree + 
          Make_namebmw+ 
          Make_nameporsche + Make_namesaab + Make_namevolvo, 
        data = train)

summary(lm26)
vif(lm26)
# no significant drop in adjusted R square


# lets check for carheight if its significant affetcs  adjusted R square
lm27=lm(price ~ enginelocation + 
          carwidth + curbweight + 
          enginesize + boreratio + stroke + 
          enginetypeohcf + enginetyperotor + 
          cylindernumberfive + cylindernumberfour +  
          cylindernumberthree + 
          Make_namebmw+ 
          Make_nameporsche + Make_namesaab + Make_namevolvo, 
        data = train)

summary(lm27)
vif(lm27)

# not affect significantly

# lets check for enginetypeohcf if its significant affetcs  adjusted R square
lm28=lm(price ~ enginelocation + 
          carwidth + curbweight + 
          enginesize + boreratio + stroke + 
           enginetyperotor + 
          cylindernumberfive + cylindernumberfour +  
          cylindernumberthree + 
          Make_namebmw+ 
          Make_nameporsche + Make_namesaab + Make_namevolvo, 
        data = train)

summary(lm28)
vif(lm28)

# not affect significantly

# lets check for Make Saab if its significant affetcs  adjusted R square
lm29=lm(price ~ enginelocation + 
          carwidth + curbweight + 
          enginesize + boreratio + stroke + 
          enginetyperotor + 
          cylindernumberfive + cylindernumberfour +  
          cylindernumberthree + 
          Make_namebmw+ 
          Make_nameporsche +Make_namevolvo, 
        data = train)

summary(lm29)
vif(lm29)
# not affect significantly
# lets check for Make porshe if its significant affetcs  adjusted R square
lm30=lm(price ~ enginelocation + 
          carwidth + curbweight + 
          enginesize + boreratio + stroke + 
          enginetyperotor + 
          cylindernumberfive + cylindernumberfour +  
          cylindernumberthree + 
          Make_namebmw+ 
          +Make_namevolvo, 
        data = train)

summary(lm30)
vif(lm30)


## NOW WE CAN LOOK DEEPER INTO COPRRLATION mATRIX AND UNDERSTAND MULTI COLLINIRITY AFTER REMOVAL OF  30 VARIABLES
##for further removal and Interpreation

cleaned_cor_car_data=car_clean[,c(5,8,10,11,12,13,31,32,33,35,46,65)]
library(corrplot)
corrplot(cor(cleaned_cor_car_data),method = "circle")

cor_31=cor(cleaned_cor_car_data)

# not affect significantly
# lets check for Make curb weight  if its significant affetcs  adjusted R square 
# also because engine size ,bore ratio,curb weight and car width sre highly vorrelatred they can give arise to 
# multi collinearity 
#previous high  Vif curbweight to be checked




lm31=lm(price ~ enginelocation + 
          carwidth + 
          enginesize + boreratio + stroke + 
          enginetyperotor + 
          cylindernumberfive + cylindernumberfour +  
          cylindernumberthree + 
          Make_namebmw+ 
          +Make_namevolvo, 
        data = train)

summary(lm31)
vif(lm31)
## we can see that not significant changes in adjusted R square meaning 
## It was leading to multicollinearity problem


#P value of Make_Volvo is highest we have to check if we can remove it

lm32=lm(price ~ enginelocation + 
          carwidth + 
          enginesize + boreratio + stroke + 
          enginetyperotor + 
          cylindernumberfive + cylindernumberfour +  
          cylindernumberthree + 
          Make_namebmw, 
        data = train)

summary(lm32)
vif(lm32)
## no significant drop in R square


##cylinder number four check if R square drops significantlly

lm33=lm(price ~ enginelocation + 
          carwidth + 
          enginesize + boreratio + stroke + 
          enginetyperotor + 
          cylindernumberfive +  
          cylindernumberthree + 
          Make_namebmw, 
        data = train)


summary(lm33)
vif(lm33)
##not significant change in R square


## cylinder number five check p value highest
lm34=lm(price ~ enginelocation + 
          carwidth + 
          enginesize + boreratio + stroke + 
          enginetyperotor + 
          cylindernumberthree + 
          Make_namebmw, 
        data = train)

summary(lm34)
vif(lm34)
# not significant change in R Square


# cylinder three check for adjusted R square after removing
lm35=lm(price ~ enginelocation + 
          carwidth + 
          enginesize + boreratio + stroke + 
          enginetyperotor + 
          Make_namebmw, 
        data = train)

summary(lm35)
vif(lm35)

#not significant change

## we can see also that number of cylinders have collinerity problem in cor31 matrix in workspace


## Bore ration Removal test having highest p value
lm36=lm(price ~ enginelocation + 
          carwidth + 
          enginesize + stroke + 
          enginetyperotor + 
          Make_namebmw, 
        data = train)

summary(lm36)
vif(lm36)



## Wer can see that bore ratio is high corrlated from engine size cor 31 and so if doent affected adjusted R square 
## model

## stoke havng highest P value in previous model we have to check it after remocal of it
lm37=lm(price ~ enginelocation + 
          carwidth + 
          enginesize + 
          enginetyperotor + 
          Make_namebmw, 
        data = train)

summary(lm37)
vif(lm37)
##not signifiant effect in adjusted R square

## nw lets check corrlation matrix on remaining variables

cor_car=train[,c(5,8,11,31,46)]
##Multicollinearity has been removed as sugggested by correlation matrix of remaining
## As p values of carwidth is low it cant be removed instead it is 70% correlated with engine size
cor35=cor(cor_car)
## for checking which model is better previous or this one

anova(lm36,lm37)
## indication lm37 is more better
## now all the variables are low p values all r three stars
summary(lm37)
vif(lm37)
## all vif are under 4 which is scceptable

##lm37 is the final model########
##lets predict for  test set
Predict_1 <- predict(lm37,test[,-19])
test$test_price <- Predict_1

# Accuracy of the predictions
# Calculate correlation
r <- cor(test$price,test$test_price)
# calculate R squared by squaring correlation
rsquared <- cor(test$price,test$test_price)^2

# check R-squared
rsquared

error=test$price-test$test_price

rmse(error)
mae(error)

###lets set a different training and test for same model


set.seed(10)

# randomly generate row indices for train dataset
trainindices= sample(1:nrow(car_clean), 0.7*nrow(car_clean))
# generate the train1 data set
train1 = car_clean[trainindices,]

#Similarly store the rest of the observations into an object "test1".
test1 = car_clean[-trainindices,]



lm38=lm(price ~ enginelocation + 
          carwidth + 
          enginesize + 
          enginetyperotor + 
          Make_namebmw, 
        data = train1)

summary(lm38)
vif(lm38)



Predict_2 <- predict(lm38,test1[,-19])
test1$test_price <- Predict_2

# Accuracy of the predictions
# Calculate correlation
r2 <- cor(test1$price,test1$test_price)
# calculate R squared by squaring correlation
r2squared <- cor(test1$price,test1$test_price)^2

# check R-squared
rsquared


error1 <- test1$price-test1$test_price
rmse(error1)
mae(error1)
## now we have check for any splitting it is giving 90% accuracy on test set##
## so model is robust ##

summary(lm37)

##model interpretation:
#we can see that engine location slope is -ve values are 1 for front and 0 for back dso if engine location increases price decrease
#More car width emplies high price
#engine size increases price increase
#rotor engine describes high price with positive slope
#BMW has extra high price

"Summary these five features can collectively decide price of  any car" 
## Its better than single linear regression on engine size as we are getting 90% accuracy on test set and low mae and rmse####







