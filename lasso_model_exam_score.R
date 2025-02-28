library(skimr)
library(caret)
library(tidyverse)
library(car)
options(scipen=999)

# install and load packages for LASSO
#install.packages("MASS")
#install.packages("e1071")
library(e1071)
#install.packages("glmnet")
library(glmnet)
#install.packages("Matrix")
library(Matrix)

Student_data <- read.csv(file = "D://Direct Downloads//Applied Machine Learning//SchoolPerformanceFactors.csv", header=T)
Student_data$X <- NULL

#change categorical to factor
Student_data <- Student_data %>% mutate_at(c("Parental_Involvement","Access_to_Resources","Extracurricular_Activities","Motivation_Level","Internet_Access","Family_Income","Teacher_Quality","School_Type","Peer_Influence","Learning_Disabilities","Parental_Education_Level","Distance_from_Home","Gender"), as.factor) 
Student_data_predictors_dummy <- model.matrix(Exam_Score~ ., data = Student_data)#create dummy variables expect for the response
Student_data_predictors_dummy<- data.frame(Student_data_predictors_dummy[,-1]) #get rid of intercept and make data frame
Student_data <- cbind(Exam_Score=Student_data$Exam_Score, Student_data_predictors_dummy)

set.seed(87) #set random seed
index <- createDataPartition(Student_data$Exam_Score, p = .8,list = FALSE)
Student_data_train <-Student_data[index,]
Student_data_test <- Student_data[-index,]

# Train Model

set.seed(7) #set the seed again since within the train method the validation set is randomly selected



lasso_model <- train(Exam_Score ~ .,
                     data = Student_data_train,
                     method = "glmnet", #"glmnet"
                     standardize=TRUE,#standardize coefficients
                     tuneGrid=expand.grid(alpha=1,
                                          lambda=seq(0.1,2, by = 0.1)),#add in grid of lambda values
                     trControl =trainControl(method = "cv",number=5))#5-fold cross validation

#get best lambda  
lasso_model
lasso_model$bestTune$lambda

#list coefficients selected
coef(lasso_model$finalModel, lasso_model$bestTune$lambda)

lasso_pred<-predict(lasso_model, Student_data_test)

MSE<-mean((lasso_pred- Student_data_test$Exam_Score)^2)
MSE
