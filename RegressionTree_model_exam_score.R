#install.packages("caret") #this may take a while
library(caret)
Reg_Student_data <- read.csv(file = "D://Direct Downloads//Applied Machine Learning//SchoolPerformanceFactors.csv", header=T)
Reg_Student_data$X <- NULL

#change categorical to factor
Reg_Student_data <- Reg_Student_data %>% mutate_at(c("Parental_Involvement","Access_to_Resources","Extracurricular_Activities","Motivation_Level","Internet_Access","Family_Income","Teacher_Quality","School_Type","Peer_Influence","Learning_Disabilities","Parental_Education_Level","Distance_from_Home","Gender"), as.factor) 
#create dummy variables expect for the response
dummies_model <- dummyVars(Exam_Score ~ ., data = Reg_Student_data)

#provide only predictors that are now converted to dummy variables
Reg_Student_data_predictors_dummy<- data.frame(predict(dummies_model, newdata = Reg_Student_data)) 

#recombine predictors including dummy variables with response
Reg_Student_data <- cbind(Exam_Score=Reg_Student_data$Exam_Score, Reg_Student_data_predictors_dummy) 


set.seed(99) #set random seed
index <- createDataPartition(Reg_Student_data$Exam_Score, p = .8,list = FALSE)
Reg_Student_data_train <-Reg_Student_data[index,]
Reg_Student_data_test <- Reg_Student_data[-index,]

# install and load packages for machine learning model
#install.packages("rpart")
library(rpart)

set.seed(10) #set random seed for k-fold cross validation
Reg_Student_data_model <- train(Exam_Score ~ .,
                       data = Reg_Student_data_train,
                       method = "rpart",
                       ## Now specify the exact models 
                       ## to evaluate:
                       tuneGrid = expand.grid(cp=seq(0.01,0.2,0.001)),
                       #cross validation
                       trControl =trainControl(method = "cv",number = 5)) 

Reg_Student_data_model #provides information of parameter tuning via cross validation

plot(Reg_Student_data_model) #provides plot of parameter tuning via cross validation

#install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(Reg_Student_data_model$finalModel, type=5)

Exam_Score_pred_tree<-predict(Reg_Student_data_model , Reg_Student_data_test)
mean((Exam_Score_pred_tree- Reg_Student_data_test$Exam_Score)^2)
