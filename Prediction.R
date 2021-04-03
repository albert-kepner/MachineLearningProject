library(dplyr)
library(caret)


training_raw <- read.csv("data/pml-training.csv")
testing_raw <- read.csv("data/pml-testing.csv")
raw <- training_raw

summary(raw)

no_na_per_column <- sapply(raw, function(x) {0 == sum(is.na(x))})

sum(no_na_per_column)

class_per_column <- sapply(raw, function(x) {class(x)})
table(class_per_column)

class_numeric <- sapply(raw, 
                        function(x) {class(x) == "numeric" | 
                            class(x) == "integer"})

numeric_no_na <- class_numeric & no_na_per_column


summary(raw$skewness_yaw_forearm)
class(raw$skewness_yaw_forearm)


all_num <- function (index) {
  ifelse(
    class(raw[,index]) == "integer" | class(raw[,index]) == "numeric",
    !is.na(sum(raw[,index])), FALSE)
}

all_int <- function (index) {
  ifelse(
  class(raw[,index]) == "integer",
  !is.na(sum(raw[,index])), FALSE)
}


all_nums <- sapply(seq_along(raw), function(x) {all_num(x)})
all_ints <- sapply(seq_along(raw), function(x) all_int(x))
sum(all_ints)


sum(numeric_no_na)
sum(all_nums)

exceptions <- numeric_no_na & !all_nums
names(raw[, exceptions])

data_f1 <- raw[all_nums]

data_f1 <- data_f1[,-seq(1,4)]

data_quiz <- testing_raw[all_nums]
data_quiz <- data_quiz[,-seq(1,4)]

data_f1 <- data_f1 %>% mutate(classe=raw$classe)


## Separate testing and training data sets.
set.seed(12345)
inTrain <- createDataPartition(data_f1$classe, p=0.70, list=FALSE)
training <- data_f1[inTrain,]
testing <- data_f1[-inTrain,]



## Predicting with trees
modFit <- train(classe ~ .,method="rpart", data=training)

print(modFit$finalModel)

library(rattle)
fancyRpartPlot(modFit$finalModel)

library(randomForest)
## rfCV <- rfcv(training[,-26], training$classe)

## Try predicting with random forest
set.seed(2468)
rf_fit <- train(classe ~ ., 
                data=training,
                method = 'rf',
                trControl = trainControl(method='cv',
                                         number=5))

print(rf_fit)

testPredictions <- predict(rf_fit, newdata = testing)

test_result <- data.frame(predicted = testPredictions, actual = testing$classe)

test_result <- test_result %>% mutate(correct = predicted==actual)

accuracy <- mean(test_result$correct)
accuracy


######################################

## final test set of 20 cases for quiz

quiz <- predict(rf_fit, newdata = data_quiz)

quiz.frame = data.frame(rownum = c(1:20), answer=quiz)


