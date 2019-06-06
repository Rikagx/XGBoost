library(tidyverse)
library(readxl)
library(here)
library(janitor)
library(xgboost)
library(inspectdf)


names <- c("mean", "sd", "kurtosis", "skewness", "DM-SNR_curve_mean", "DM-SNR_curve_sd", "DM-SNR_curve_kurtosis", "DM-SNR_curve_skewness", "target")

all_stars <- read_csv("pulsar_stars.csv", col_names = TRUE)

names(all_stars) <- names

target_df <- all_stars %>% select(target)

all_stars_notarget <- all_stars %>% select(-target)

full_matrix <- data.matrix(all_stars_notarget)

numberOfTrainingSamples <- round(nrow(all_stars) * .7)



try <- all_stars %>% mutate(target = ifelse(target == "0", NA, "1"))
try <- try %>% mutate(target = as.numeric(target))

xLabels <- try %>%
  select(target) %>%
  is.na() %>%
  magrittr::not()


train_data <- full_matrix[1:numberOfTrainingSamples,]
train_labels <- xLabels[1:numberOfTrainingSamples]

test_data <- full_matrix[-(1:numberOfTrainingSamples),]
test_labels <- xLabels[-(1:numberOfTrainingSamples)]


dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)


model1 <- xgboost(data = dtrain, # the data   
                 nround = 2, # max number of boosting iterations
                 objective = "binary:logistic")

pred <- predict(model1, dtest)

err <- mean(as.numeric(pred > 0.5) != test_labels)
print(paste("test-error=", err))

