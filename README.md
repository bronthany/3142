# 3142

#### BRIAN - COMMON DATA SET ####
library(dplyr)

data <- read.csv("ACTL31425110AssignmentData2022.csv", header = TRUE, stringsAsFactors = TRUE)
summary(data)
data$accident_month <- as.Date(data$accident_month)
data$claim_loss_date <- as.Date(data$claim_loss_date)
data$term_start_date <- as.Date(data$term_start_date)
data$term_expiry_date <- as.Date(data$term_expiry_date)

#### CLEANING ####
duplicates <- duplicated(data)
data <- data[!duplicates,]
test <- na.omit(data) # test has no NA's
