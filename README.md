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

data <- data[!duplicated(data),] # Duplicates removed

# Cleaning for Frequency Data set
d1 <- data
d1$total_claims_cost[is.na(data$total_claims_cost)] <- 0 #Convert NA's to 0
d1$Indicator <- ifelse(!d1$total_claims_cost == 0, 1, 0) #Indicator, 1 if claim, 0 if no claim
d1 <- d1 %>% 
  group_by(policy_id, accident_month) %>%
  mutate(Claim_number = sum(Indicator)) # Calculate total claim number per month per policy
d1 <- d1[!d1$exposure == 0,] # Remove 0 exposure entries

table(d1$Claim_number) #       0       1       2       3 
                       #   1218043    7226     160      5 

# Cleaning for Severity Data set
d2 <- na.omit(data) # Severity data set has no NA's
hist(d2$total_claims_cost, breaks = 50)


# Training and test data set - Frequency data set
training_d1 <- d1$accident_month < as.Date("2020-07-31")
d1.train <- d1[training_d1,]
d1.test <- d1[!training_d1,]

# Training Data set - Severity data set
training_d2 <- d2$accident_month < as.Date("2020-07-31")
d2.train <- d2[training_d2,]
d2.test <- d2[!training_d2,]


# Logistic Regression for Claim Frequency
glm.fit1 <- glm(Indicator ~ exposure, data = d1.train, family = "binomial")
summary(glm.fit1)

glm.fit2 <- glm(Indicator ~ risk_state_name, data = d1.train, family = "binomial", offset = exposure)
summary(glm.fit2)

glm.fit12 <- glm(Indicator ~ exposure + risk_state_name, data = d1.train, family = "binomial")
summary(glm.fit12)



# Poisson Regression for Claim Frequency
fit1 <- glm(Claim_number ~ risk_state_name, data = d1.train, family = "poisson", offset = log(exposure))
summary(fit1)

fit12 <- glm(Claim_number ~ risk_state_name + vehicle_class, data = d1.train, family = "poisson", offset = log(exposure))
summary(fit12)



#### CATHY - Assigning quarterly index values to accident dates ####
install.packages("lubridate")                        
library("lubridate")
d2_date_quarters <- paste0(year(d2$accident_month),         # Convert dates to quarterly
                             "/0",
                             quarter(d2$accident_month))

import_date_quarters <- paste0(year(import.index$Date),         # Convert dates to quarterly
                           "/0",
                           quarter(import.index$Date))
import.index<-import.index%>%
  mutate(Quarters=import_date_quarters)

d2<-d2%>%
  mutate(quarters=d2_date_quarters)
colnames(import.index)<-c("Date","Index.numbers","Quarter")

#while loop
d2$"Import Index" <- 0
i=1
while(i <= length(d2$"Import Index")){
  k=1
  for(j in import.index$Quarter){
    if(j==d2$quarters[i]){
      d2$`Import Index`[i]<-import.index$Index.numbers[k]
    }
    k<-k+1
  }
  i<-i+1
}

#### Adela - time series of Automotive fuel index number ####

p <- ggplot(copy_data, aes(factor(Quart),index_number)) +
  geom_line(aes(group=1),colour="red") + geom_point() + ggtitle("Automotive Fuel \n Index Number") +
  xlab("Time") +ylab("Index Number") +
  theme(plot.title = element_text(hjust = 0.5))
  
#### Adela - claim frequency and Automotive fuel index number ####

x <- as.data.frame(copy_data %>% count(index_number))
index_ <- ggplot(x, aes(index_number,n)) +
  geom_line(aes(group=1),colour="red") + geom_point() + ggtitle("Automotive Fuel Index Number \n v.s Claim frequency") +
  xlab("Index Number") +ylab("Claim Frequency") +
  theme(plot.title = element_text(hjust = 0.5))

#### CATHY - Adding extrapolated variable to d2 data frame ####
# Read import index csv 
import.index<-read.csv("import.index.csv",header=T)
import.index$Date<-as.Date(import.index$Date)
attach(import.index)

# Package for converting dates into desirable format
install.packages("lubridate")                        
library("lubridate")

# A vector containing dates of d2 in the format 1/month/year 
data_months <- paste0(year(d2$accident_month), "/",        
                         month(d2$accident_month),"/1")

# The vector above is added onto d2 as a new column called Monthly_Date
d2 <- d2 %>% mutate(Monthly_Date = as.Date(data_months))    

# Function to extrapolate monthly values from quarterly import index
extrapolate<-function(a,b){
  sequence<-seq(a,b,length.out=4)
  sequence[]
}

# Vector of all the extrapolated monthly import index values
monthly_import<-c() 
i=1
while(i < length(Index.numbers)){
  extrapolate_quarter<-extrapolate(Index.numbers[i],Index.numbers[i+1])
  monthly_import<-append(monthly_import,extrapolate_quarter[-4])
  i<-i+1
}
monthly_import<-append(monthly_import,extrapolate_quarter[4])

# Sequence of months from 1/6/2016 to 1/6/2021 
monthly_dates<-seq(as.Date("2016/6/1"), by = "month", length.out = 61)

# Data frame containing months and corresponding import index values
new_import_monthly<-cbind.data.frame(as.Date(monthly_dates),monthly_import)
colnames(new_import_monthly)<-c("Monthly_Date","Index.numbers")

# Adding on Import index column onto d2 data frame 
d2$"Import Index" <- 0
i=1
while(i <= length(d2$"Import Index")){
  k=1
  for(j in new_import_monthly$Monthly_Date){
    if(j==d2$Monthly_Date[i]){
      d2$`Import Index`[i]<-new_import_monthly$Index.numbers[k]
    }
    k<-k+1
  }
  i<-i+1
}




