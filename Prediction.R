library(dplyr)
library(caret)
library(rattle)

training_raw <- read.csv("data/pml-training.csv")
testing_raw <- read.csv("data/pml-testing.csv")
raw <- training_raw



all_num <- function (index) {
  ifelse(
    class(raw[,index]) == "integer" || class(raw[,index]) == "numeric'",
    !is.na(sum(raw[,index])), FALSE)
}



data_f1 <- raw[all_nums]

data_f1 <- data_f1[,-seq(1,4)]

data_f1 <- data_f1 %>% mutate(classe=raw$classe)


## Separate testing and training data sets.
set.seed(12345)
inTrain <- createDataPartition(data_f2$classe, p=0.70, list=FALSE)
training <- data_f1[inTrain,]
testing <- data_f1[-inTrain,]


M <- abs(cor(training[,-26]))
diag(M) <- 0
which(M > 0.9, arr.ind=TRUE)
plot(training[,1],training[,3])


## Predicting with trees
modFit <- train(classe ~ .,method="rpart", data=training)

print(modFit$finalModel)

library(rattle)
fancyRpartPlot(modFit$finalModel)

