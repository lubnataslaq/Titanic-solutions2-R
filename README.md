# Titanic-solutions2-R
Titanic solutions
library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)
library(rpart.plot)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

set.seed(42, sample.kind = "Rounding")
test_index <- createDataPartition(titanic_clean$Survived, times=1, p=0.2, list=FALSE)
test <- titanic_clean[test_index,]
train <- titanic_clean[-test_index,]

---------------------
# Q7 
set.seed(1, sample.kind = "Rounding")
lda_model <- train(Survived ~ Fare, method = "lda", data = train)
pre0 <- predict(lda_model, test)
mean(pre0 == test$Survived)
confusionMatrix(predict(lda_model, test), test$Survived)

# Set the seed to 1. Train a model using quadratic discriminant analysis
#  (QDA) with the caret qda method using fare as the only predictor.
# What is the accuracy on the test set for the QDA model?
set.seed(1, sample.kind = "Rounding")
qda_model <- train(Survived ~ Fare, method = "qda", data = train)
pre <- predict(qda_model, test)
mean(pre == test$Survived)
confusionMatrix(predict(qda_model, test), test$Survived)

---------------
# Q8 
set.seed(1, sample.kind = "Rounding")
glm_model <- train(Survived ~ ., method = "glm", data = train)
glm_model
pre1 <- predict(glm_model, test)
mean(pre1 == test$Survived)
confusionMatrix(predict(glm_model, test), test$Survived)

# What is the accuracy of a logistic regression model with the caret
#  glm method using four predictors: sex, class, fare, and age?
set.seed(1, sample.kind = "Rounding")
glm_model <- train(Survived ~ Sex+Pclass+Age+Fare, method = "glm", data = train)
glm_model
confusionMatrix(predict(glm_model, test), test$Survived)

-----------------
 # Q9 #set.seed(6) optimal value of the number of neighbors k
  set.seed(6, sample.kind = "Rounding") # if using R 3.6 or later
train_knn <- train(Survived ~ .,
                   method = "knn",
                   data = train_set,
                   tuneGrid = data.frame(k = seq(3, 51, 2)))
train_knn$bestTune
# accuracy for knn model 
knn_preds <- predict(train_knn, test_set)
mean(knn_preds == test_set$Survived)

-------------
# Q10 cross validation, Set the seed to 8 and train a new kNN model. 
# Instead of the default training control, 
# use 10-fold cross-validation where each partition consists of 10% of the total. 
  # Try tuning with k = seq(3, 51, 2).
  
set.seed(8, sample.kind = "Rounding") # if using R 3.6 or later
train_knn <- train(Survived ~ .,
                   method = "knn",
                   data = train,
                   trControl =trainControl(method="cv", number=10, p=0.9) ,
                   tuneGrid = data.frame(k = seq(3, 51, 2)))
train_knn$bestTune
plot(train_knn)
confusionMatrix(predict(train_knn, test), test$Survived)

---------
# Q11 Classification tree model
  set.seed(10, sample.kind="Rounding")
train_rpart <- train(Survived ~ .,
                     method = "rpart",
                     data = train,
                     tuneGrid = data.frame(cp=seq(from=0, to=0.05, by=0.002)))
train_rpart$bestTune 
confusionMatrix(predict(train_rpart, test), test$Survived)
plot(train_rpart$finalModel, margin = 0.2)
text(train_rpart$finalModel)

------------------
# Q12 Random forest model, using varImp() 
# on the random forest model object to determine 
  #the importance of various predictors to the random forest model.  
  set.seed(14, sample.kind="Rounding")
train_rpart <- train(Survived ~ .,
                     method = "rf",
                     data = train,
                     tuneGrid = data.frame(mtry = seq(1:7)),
                     ntree = 100)
train_rpart$bestTune 
confusionMatrix(predict(train_rpart, test), test$Survived)
varImp(train_rpart)
