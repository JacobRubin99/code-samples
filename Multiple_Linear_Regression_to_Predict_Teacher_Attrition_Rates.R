---
title: " Using Linear Regression to Predict Teacher Attrition Rates from SES/School Factors"
author: "Jacob Rubin"
output: html_document
---

#data from years 2010, 2012, 2014
data2010 = read.csv("Sources_Source_2010_woPASC_csv.csv")
data2012 = read.csv("Sources_Source_2012_woCHER_csv.csv")
data2014 = read.csv("Sources_Source_2014_woCHER_csv.csv")

#create testing and training data
train = rbind(data2010, data2012)
test = data2014

#first linear model (using significant predictors from python output)
mlr_model = lm(Attrition.Rate ~ Reduced + Free + Dropout.Rate, data = train)
summary(mlr_model)

#residuals look good, does not seem to be any pattern
plot(mlr_model$residual, x = train[,'Dropout.Rate'], xlab = "Free", ylab = "Residuals")

#major fanning, want to try a transformation (log?)
plot(mlr_model$residual, x = train[,'Reduced'], xlab = "Reduced", ylab = "Residuals")
plot(mlr_model$residual, x = train[,'Free'], xlab = "Free", ylab = "Residuals")

#new model with logs 
mlr_model2 = lm(Attrition.Rate ~ log(Reduced) + log(Free) + Dropout.Rate, data = train)
summary(mlr_model2)

#still fanning, no major improvement
plot(mlr_model2$residual, x = train[,'Free'], xlab = "Free", ylab = "Residuals")
abline(0,0)

#create transformed variables for free and reduced lunch
train$Free2 = sqrt(train$Free)
train$Reduced2 = sqrt(train$Reduced^2)
test$Free2 = sqrt(test$Free^2)
test$Reduced2 = sqrt(test$Reduced^2)

#new model with sqrt transformed variables
mlr_model3 = lm(Attrition.Rate ~ Reduced2 + Free2 + Dropout.Rate, data = train)
summary(mlr_model3)

#still fanning!! no major improvement
plot(mlr_model3$residual, x = train[,'Free'], xlab = "Free", ylab = "Residuals")
abline(0,0)

#let's try predicting anyway
attr_predict = predict(mlr_model, test)
actualpred_df <- data.frame(cbind(actual=test$Attrition.Rate, predicted=attr_predict))
correlation_accuracy = cor(actualpred_df)
correlation_accuracy
#correlation: .1226

#let's try adding race variables
mlr_model4 = lm(Attrition.Rate ~ Reduced + Dropout.Rate + log(Black.Students) + log(White.Students), data = train)
summary(mlr_model4)

#looking good!
plot(mlr_model4$residual, x = train[,'Dropout.Rate'], xlab = "X", ylab = "Residuals")

#better with log transformations (still fanning, but improved)
plot(mlr_model4$residual, x = train[,'White.Students'], xlab = "X", ylab = "Residuals")
plot(mlr_model4$residual, x = train[,'Black.Students'], xlab = "X", ylab = "Residuals")

#ok... fanning but prediction seems fine
plot(mlr_model4$residual, x = train[,'Reduced'], xlab = "X", ylab = "Residuals")

#let's predict
attr_predict1 = predict(mlr_model4, test)
actualpred_df1 <- data.frame(cbind(actual=test$Attrition.Rate, predicted=attr_predict1))
correlation_accuracy1 <- cor(actualpred_df1)
correlation_accuracy1
#correlation: .6278
#yay!!!! adding race seems to have done the trick

#check model using student percentages instead of student numbers
mlr_model5 = lm(Attrition.Rate ~ Reduced + Dropout.Rate + log(Black.Perc) + (White.Perc) + Acts.per.1000.students, data = train)
summary(mlr_model5)

plot(mlr_model4$residual, x = train[,'Acts.per.1000.students'], xlab = "X", ylab = "Residuals")

attr_predict2 = predict(mlr_model5, test)
actualpred_df2 <- data.frame(cbind(actual=test$Attrition.Rate, predicted=attr_predict2))
correlation_accuracy2 <- cor(actualpred_df2)
correlation_accuracy2
#correlation: .5911
#not bad, though using student numbers seems to help prediction; let's stick with model 4

#what if we split training and testing randomly instead of by year?

alldata = rbind(data2010, data2012, data2014)

trainingRowIndex = sample(1:nrow(alldata), 0.8*nrow(alldata))

#randomly split data into training and testing
newtrain = alldata[trainingRowIndex, ] 
newtest  = alldata[-trainingRowIndex, ]

new_mlr = lm(Attrition.Rate ~ Reduced + Dropout.Rate + log(Black.Students) + log(White.Students), data = newtrain)
summary(new_mlr)
plot(new_mlr$residual, x = newtrain[,'White.Students'], xlab = "X", ylab = "Residuals")

#let's try to predict
attr_predict_new = predict(new_mlr, newtest)
actualpred_new <- data.frame(cbind(actual=newtest$Attrition.Rate, predicted=attr_predict_new))
correlation_accuracy2 <- cor(actualpred_new)
correlation_accuracy2
#correlation: .5689

# However, this model will vary depending on which observations end up in training and which end up in testing
