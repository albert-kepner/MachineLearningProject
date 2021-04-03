library(dplyr)
library(caret)


training_raw <- read.csv("data/pml-training.csv")
testing_raw <- read.csv("data/pml-testing.csv")
raw <- training_raw

class <- sapply(raw, function(x) class(x))

is_na <- sapply(raw, function(x) any(is.na(x)))

## Summarize column types and completeness of data by column.
col_desc <- data.frame(name = names(raw), 
                       index = seq_along(names(raw)), 
                       class = class, 
                       no_missing = !is_na)
View(col_desc)

summary(raw$amplitude_yaw_forearm)

head(col_desc)

c <- col_desc
no_missing <- col_desc[c$no_missing,]

int_only <- no_missing[no_missing$class=="integer",]
head(int_only)
numeric_only <- no_missing[no_missing$class=="numeric",]
head(numeric_only)

## the first 4 integer columns x through num_window appear to
## be record keeping rather that physical measurements of activity.
int_only <- int_only[-1:-4,]


## Separate testing and training data sets.
set.seed(12345)
inTrain <- createDataPartition(raw$classe, p=0.70, list=FALSE)
training <- raw[inTrain,]
testing <- raw[-inTrain,]

## Now we want to select feature subsets of the testing and training data


training.integer = training[, int_only$index]
testing.integer = testing[, int_only$index]
training.numeric = training[, numeric_only$index]
testing.numeric = testing[, numeric_only$index]
all_numbers = c(int_only$index, numeric_only$index)
training.all = training[, all_numbers]
testing.all = testing[, all_numbers]
dim(training.all)
dim(testing.all)



## Add the outcome variable classe back into the training data frames:

training.integer <- training.integer %>% mutate(classe = training$classe)
training.numeric <- training.numeric %>% mutate(classe = training$classe)
training.all <- training.all %>% mutate(classe = training$classe)



## Try fitting each model with random forest

set.seed(2468)
old <- Sys.time()
integer.fit <- train(classe ~ ., 
                     data=training.integer,
                     method = 'rf',
                     trControl = trainControl(method='cv',
                                              number=5))
integer_fit_time <- Sys.time() - old

print(integer_fit_time)
print(integer.fit)


set.seed(2468)
old <- Sys.time()
numeric.fit <- train(classe ~ ., 
                     data=training.numeric,
                     method = 'rf',
                     trControl = trainControl(method='cv',
                                              number=5))
numeric_fit_time <- Sys.time() - old

print(numeric_fit_time)
print(numeric.fit)

set.seed(2468)
old <- Sys.time()
all.fit <- train(classe ~ ., 
                 data=training.all,
                 method = 'rf',
                 trControl = trainControl(method='cv',
                                          number=5))
all_fit_time <- Sys.time() - old


print(all_fit_time)
print(all.fit)


i <- integer.fit$results
i_acc <- i[1,2]
i_acc

n <- numeric.fit$results
n_acc <- n[2,2]
n_acc

a <- all.fit$results
a_acc <- a[2,2]
a_acc
Predictors_Used <- c("25 integer columns", "27 numeric columns", "52 integer + numeric columns")
Accuracy <- c(i_acc, n_acc, a_acc)
Traning_Time <- c(integer_fit_time, numeric_fit_time, all_fit_time)
SummaryFrame <- data.frame(Predictors_Used=Predictors_Used, In_Sample_Accuracy=Accuracy, Traning_Time)


library(xtable)
xt <- xtable(SummaryFrame)
print(xt, type="html")


integer.predict <- predict(integer.fit, newdata = testing.integer)

integer.result <- data.frame(predicted = integer.predict, actual = testing$classe)

integer.result <- integer.result %>% mutate(correct = predicted==actual)

integer.accuracy <- mean(integer.result$correct)
integer.accuracy


numeric.predict <- predict(numeric.fit, newdata = testing.numeric)

numeric.result <- data.frame(predicted = numeric.predict, actual = testing$classe)

numeric.result <- numeric.result %>% mutate(correct = predicted==actual)

numeric.accuracy <- mean(numeric.result$correct)
numeric.accuracy


all.predict <- predict(all.fit, newdata = testing.all)

all.result <- data.frame(predicted = all.predict, actual = testing$classe)

all.result <- all.result %>% mutate(correct = predicted==actual)

all.accuracy <- mean(all.result$correct)
all.accuracy


out_accuracy <- c(integer.accuracy, numeric.accuracy, all.accuracy)
SummaryFrame <- SummaryFrame %>% mutate(Out_Of_Sample_Accuracy = out_accuracy)
SummaryFrame