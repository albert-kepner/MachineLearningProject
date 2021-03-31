library(dplyr)
library(caret)

training_raw <- read.csv("data/pml-training.csv")
testing_raw <- read.csv("data/pml-testing.csv")

names(training)

names(testing)

str(training)

summary(training)


na_count <-sapply(training_raw, function(y) sum(length(which(is.na(y)))))

na_test <-sapply(testing_raw, function(y) sum(length(which(is.na(y)))))

na_count

na_count2 <-lapply(training, function(y) sum(length(which(is.na(y)))))

x <- c(2,5,3,9,8,11,6)
count <- 0
for (val in x) {
  if(val %% 2 == 0)  {
    count = count+1
    print(val)
  }
}
print(count)

raw <- training_raw

ss <- seq_len(length(raw))

for (index in seq(1,length(raw), by=1)) {
  any_na <- any(is.na(raw[,index]))
  msg <- paste(index, any_na)
  print(msg)
}

raw_has_na <- function (index) {
  any(is.na(raw[,index]))
}

raw_no_na <- function (index) {
  !any(is.na(raw[,index]))
}



has_na <- sapply(seq_along(raw), raw_has_na)

no_na <- sapply(seq_along(raw), raw_no_na)
data_f1 <- raw[,no_na]

data_na <- raw[,has_na]
data_na <- data_na %>% mutate(classe=raw$classe)
data_na2 <- data_na[!is.na(data_na$max_roll_belt),]

count_na_not <- function(index) {
  sum(!is.na(data_na[,index]))
}

not_nas <- sapply(seq_along(data_na), count_na_not)

not_nas

sum(no_na)



