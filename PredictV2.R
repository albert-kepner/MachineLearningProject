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