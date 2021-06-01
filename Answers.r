library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

###Question 1

set.seed(42, sample.kind = "Rounding") # if using R 3.6 or later
y<-titanic_clean$Survived
test_index <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)

test_set <- titanic_clean[test_index, ]
train_set <- titanic_clean[-test_index, ]

nrow(test_set)

nrow(train_set)

mean(train_set$Survived==1)


###Question 2


set.seed(3, sample.kind = "Rounding") 
n<-length(test_set$Survived)
y_hat<-sample(c(0,1),n,replace=TRUE)
y_hat
mean(y_hat==test_set$Survived)


###Question 3

train_set %>%
  group_by(Sex) %>%
  summarize(Survived = mean(Survived == 1)) %>%
  filter(Sex == "female") %>%
  pull(Survived)


train_set %>%
  group_by(Sex) %>%
  summarize(Survived = mean(Survived == 1)) %>%
  filter(Sex == "male") %>%
  pull(Survived)

y_hat_S <- ifelse(test_set$Sex=="female",1,0)
mean(y_hat_S==test_set$Survived)


###Question 4


train_set %>%
  group_by(Pclass) %>%
  summarize(Survived = mean(Survived == 1))

y_hat_P <- ifelse(test_set$Pclass=="1",1,0)
mean(y_hat_P==test_set$Survived)

train_set %>%
  group_by(Sex,Pclass) %>%
  summarize(Survived = mean(Survived == 1))

y_hat_SP <- ifelse(test_set$Sex=="female" & (test_set$Pclass== "1" | test_set$Pclass== "2"),1,0)
mean(y_hat_SP==test_set$Survived)



###Question 5

confusionMatrix(factor(y_hat_S), test_set$Survived)

confusionMatrix(factor(y_hat_P), test_set$Survived)

confusionMatrix(factor(y_hat_SP), test_set$Survived)


###Question 6

F_meas(factor(y_hat_S), test_set$Survived)
F_meas(factor(y_hat_P), test_set$Survived)
F_meas(factor(y_hat_SP), test_set$Survived)



###Question 7

set.seed(1, sample.kind = "Rounding") 
train_lda <- train(Survived ~ Fare , method = "lda", data = train_set)
lda_prediction <- predict(train_lda, test_set)
confusionMatrix(lda_prediction, test_set$Survived)$overall["Accuracy"]



set.seed(1, sample.kind = "Rounding") 


train_qda <- train(Survived ~ Fare , method = "qda", data = train_set)
qda_prediction <- predict(train_qda, test_set)
confusionMatrix(qda_prediction, test_set$Survived)$overall["Accuracy"]


###Question 8

set.seed(1, sample.kind = "Rounding") 

train_glm <- train(Survived ~ Age , method = "glm", data = train_set)

y_hat_glm <- predict(train_glm, test_set)
confusionMatrix(y_hat_glm, test_set$Survived)$overall["Accuracy"]


set.seed(1, sample.kind = "Rounding") 

train_glm2 <- train(Survived ~ Sex+Pclass+Fare+Age , method = "glm", data = train_set)

y_hat_glm2<- predict(train_glm2, test_set)
confusionMatrix(y_hat_glm2, test_set$Survived)$overall["Accuracy"]


set.seed(1, sample.kind = "Rounding") 

train_glm3 <- train(Survived ~ . , method = "glm", data = train_set)

y_hat_glm3<- predict(train_glm3, test_set)
confusionMatrix(y_hat_glm2, test_set$Survived)$overall["Accuracy"]


###Question 9, to be able to run R:  https://rdrr.io/snippets/

set.seed(6, sample.kind = "Rounding") 

train_knn <- train(Survived ~ . , method = "knn", data = train_set, tuneGrid = data.frame(k = seq(3, 51, 2)))

train_knn$bestTune

plot(train_knn)
plot(train_knn$results$k,train_knn$results$Accuracy)

y_hat_knn<- predict(train_knn, test_set)
confusionMatrix(y_hat_knn, test_set$Survived)$overall["Accuracy"]



###Question 10


