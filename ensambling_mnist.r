models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")

library(caret)
library(dslabs)
library(tidyverse)

set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models

##### Q2

pred <- sapply(fits, function(object) 
  predict(object, newdata = mnist_27$test))
dim(pred)



##### Q3

Accur<-function(i) {
                    confusionMatrix(factor(pred[,i]),mnist_27$test$y)$overall["Accuracy"]
                   }


acc<-sapply(seq(1:10),Accur)
mean(acc)


##### Q4

ensamble<-function(i) {
                      ifelse(length(which(pred[i,]==2))>5,2,7)
}

ensfit<-sapply(seq(1:200),ensamble)

confusionMatrix(factor(ensfit),mnist_27$test$y)$overall["Accuracy"]

##### Q5

ind <- acc > mean(ensfit == mnist_27$test$y)
sum(ind)
models[ind]


##### Q6

acc_hat <- sapply(fits, function(fit) min(fit$results$Accuracy))
mean(acc_hat)


fitmins <- lapply(models, function(model){ 
  print(model)
  train_control <- trainControl(method = "cv")
  fit<-train(y ~ ., method = model, data = mnist_27$train,trControl = train_control)
  min(fit$results$Accuracy)
}) 

fitmins[[1]]+fitmins[[2]]+fitmins[[3]]+fitmins[[4]]+fitmins[[5]]+fitmins[[6]]+fitmins[[7]]+fitmins[[8]]+fitmins[[9]]+fitmins[[10]]




##### Q7

modidx<-which(acc_hat>=0.8)
fits[modidx]

newpred <- sapply(fits[modidx], function(object) 
  predict(object, newdata = mnist_27$test))
dim(newpred)

votes <- rowMeans(newpred == "7")
y_hat <- ifelse(votes >= 0.5, "7", "2")
mean(y_hat == mnist_27$test$y)

