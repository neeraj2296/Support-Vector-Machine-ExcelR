#Including necassary libraries
library(kernlab)
library(klar)
library(caret)
library(plyr)

#Loading the data set
Forest <- read.csv(file.choose())
View(Forest)
class(Forest)
str(Forest)

# The area value has lots of zeros
hist(Forest$area)
rug(Forest$area)

# Transform the Area value to Y 
FF1 <- mutate(Forest, y = log(area + 1))  # default is to the base e
hist(FF1$y)
summary(Forest) # Confirms on the different scale and demands normalizing the data.


# Prediction of Forest fires requires only prediction from 
# temperature, rain, relative humidity and wind speed
# Apply Normalization technique to the whole dataset :
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
#Normalizing the following variables.
Forest$temp = normalize(Forest$temp)
Forest$RH   = normalize(Forest$RH)
Forest$wind = normalize(Forest$wind)
Forest$rain = normalize(Forest$rain)
# We need to tweak this as a classification problem.lets base out the Size using this criteria :
attach(Forest)


# Data is Partitioned as train and test set
set.seed(123)
ind <- sample(2, nrow(Forest), replace = TRUE, prob = c(0.7,0.3))
FF_train <- Forest[ind==1,]
FF_test  <- Forest[ind==2,]
FF_train$size_category<-as.factor(FF_train$size_category)
str(FF_train)


# Building model 
model1<-ksvm(size_category~temp+rain+wind+RH, 
             data= FF_train,kernel = "vanilladot")
Area_pred <- predict(model1, FF_test)
table(Area_pred,FF_test$size_category)

agreement <- Area_pred == FF_test$size_category
table(agreement)
prop.table(table(agreement))


# kernel = vanilladot
model_vanilla<-ksvm(size_category~temp+rain+wind+RH, 
                    data= FF_train,kernel = "vanilladot")
pred_vanilla<-predict(model_vanilla,newdata=FF_test)
mean(pred_vanilla==FF_test$size_category) # 67.80


# kernel = rfdot 
model_rfdot<-ksvm(size_category~temp+rain+wind+RH, 
                  data= FF_train,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata=FF_test)
mean(pred_rfdot==FF_test$size_category) # 68.41

# kernal = besseldot
model_besseldot<-ksvm(size_category~temp+rain+wind+RH, 
                      data= FF_train,kernel = "besseldot")
pred_bessel<-predict(model_besseldot,newdata=FF_test)
mean(pred_bessel==FF_test$size_category) # 67.80


# kvsm() function is best for gaussian RBF kernel with accuracy 68.41%