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

d1 <- data[!duplicated(data),] # Duplicates removed
d2 <- na.omit(d1) # test has no NA's

# Training and test data set - Frequency data set
training_d1 <- d1$accident_month < as.Date("2020-07-31")
d1.train <- d1[training_d1,]
d1.test <- d1[!training_d1,]

# Training Data set - Severity data set
training_d2 <- d2$accident_month < as.Date("2020-07-31")
d2.train <- d2[training_d2,]
d2.test <- d2[!training_d2,]
