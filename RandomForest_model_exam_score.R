library(tidyverse)
library(ggplot2)
library(skimr)
library(dplyr)
library(GGally)
library(caret)

Ran_Student_data <- read.csv(file = "D://Direct Downloads//Applied Machine Learning//SchoolPerformanceFactors.csv", header=T)
Ran_Student_data$X <- NULL

#change categorical to factor
Ran_Student_data <- Ran_Student_data %>% mutate_at(c("Parental_Involvement","Access_to_Resources","Extracurricular_Activities","Motivation_Level","Internet_Access","Family_Income","Teacher_Quality","School_Type","Peer_Influence","Learning_Disabilities","Parental_Education_Level","Distance_from_Home","Gender"), as.factor) 
#create dummy variables expect for the response
dummies_model <- dummyVars(Exam_Score ~ ., data = Ran_Student_data)

#provide only predictors that are now converted to dummy variables
Ran_Student_data_predictors_dummy<- data.frame(predict(dummies_model, newdata = Ran_Student_data)) 

#recombine predictors including dummy variables with response
Ran_Student_data <- cbind(Exam_Score=Ran_Student_data$Exam_Score, Ran_Student_data_predictors_dummy) 



#create training and testing
set.seed(99)
index <- createDataPartition(Ran_Student_data$Exam_Score, p = .8,list = FALSE)
Ran_Student_data_train <-Ran_Student_data[index,]
Ran_Student_data_test <- Ran_Student_data[-index,]

library(randomForest)  

set.seed(8)
model_rf <- train(Exam_Score ~.,
                  data = Ran_Student_data_train,
                  method = "rf",
                  tuneGrid= expand.grid(mtry = c(2,12,23)),
                  trControl=trainControl(method = 'cv',number = 5
                                         ## Estimate class probabilities
                                         #classProbs = FALSE,
                                         #needed to get ROC
                                         #summaryFunction = twoClassSummary
                  ),
                  #metric="ROC"
)

model_rf
plot(model_rf)
model_rf$bestTune
plot(varImp(model_rf), top=10)
flight_pred_tree<-predict(model_rf , Ran_Student_data_test)
MSE<-mean((flight_pred_tree- Ran_Student_data_test$Exam_Score)^2)
MSE
