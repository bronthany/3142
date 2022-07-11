library(dplyr)
library(ggplot2)
library(lubridate)
library(data.table)
library(tidyr)
library(stringr)
library(Metrics)
library(AER)
library(pscl)
library(MASS)

data <- read.csv("ACTL31425110AssignmentData2022.csv", header = TRUE, stringsAsFactors = TRUE)
importindex <- read.csv("import.index.csv", header = TRUE)
CPI <- read.csv("CPI_data.csv", header = TRUE)
petrol_prices <- read.csv("petrol_price_quarter.csv", header = TRUE)
motor_sales <- read.csv("motor_vehicle_sales.csv", header = TRUE)
gold <- read.csv("GoldData.csv", header = TRUE)
road_deaths <- read.csv("Road_Deaths.csv", header = TRUE)
unemployment <- read.csv("unemploymentrate.csv", header = TRUE)
maintenance <- read.csv("data_petrol.csv", header = TRUE)
USDAUD <- read.csv("PriceHistoryAUDUSD.csv", header = TRUE)
JPYAUD <- read.csv("PriceHistoryJPYAUD.csv", header = TRUE)

data$accident_month <- as.Date(data$accident_month)
data$claim_loss_date <- as.Date(data$claim_loss_date)
data$term_start_date <- as.Date(data$term_start_date)
data$term_expiry_date <- as.Date(data$term_expiry_date)
summary(data)


#### CLEANING ####
data <- data.table(data)
data <- data[!duplicated(data),] # Duplicates removed
data$total_claims_cost[is.na(data$total_claims_cost)] <- 0 #Convert NA's to 0
data[, quarter := year(data$accident_month) + quarter(data$accident_month)*0.1] # Add year.quarter
data <- data %>% mutate(vehicle_risk = case_when(
  vehicle_class %in% c("Class 2", "Class 4", "Class 5", "Class 9", "Class 15") ~ "Low Risk Vehicle",
  vehicle_class %in% c("Class 1", "Class 6", "Class 7", "Class 8", "Class 10") ~ "Medium Risk Vehicle",
  TRUE ~ "High Risk Vehicle"
)) # Add Vehicle Risk Class (i.e. combining Vehicle Classes)
data$vehicle_risk <- as.factor(data$vehicle_risk)


### EXTERNAL DATA ####

# Merge CPI and Import Index
CPI <- rbind(CPI, data.frame(X = 21, Date = "2021-06-01", CPI.Index = 118.8))
external <- merge(CPI, importindex, by = "Date")
cor(external$CPI.Index, external$Index.numbers) # 0.7475

external <- external %>%
  mutate(quarter = year(Date) + quarter(Date)*0.1) %>%
  dplyr :: select(-c(X)) %>%
  rename("import_index" = Index.numbers,
         "CPI_index" = CPI.Index) %>%   # Convert date to year.quarter
  mutate("lag_CPI_index" = lag(CPI_index)) # Add lagged CPI by 1 quarter
 

# Merge Petrol Prices 
names(petrol_prices)[2] = "quarter"
petrol_prices <- petrol_prices %>%
  mutate("lag_petrol_price" = lag(petrol_price)) # Add lagged petrol price by 1 quarter

external <- merge(external, petrol_prices[, -1], by = "quarter")

# Merge Motor Vehicle Sales
motor_sales[c("Month", "Year")] <- str_split_fixed(motor_sales$Date, "-", 2)
motor_sales[c("quarter")] <- ifelse(motor_sales$Month == "Jan" | motor_sales$Month == "Feb" | motor_sales$Month == "Mar", 1,
                                    ifelse(motor_sales$Month == "Apr" | motor_sales$Month == "May" | motor_sales$Month == "Jun", 2,
                                           ifelse(motor_sales$Month == "Jul" | motor_sales$Month == "Aug" | motor_sales$Month == "Sep", 3,
                                                  ifelse(motor_sales$Month == "Oct" | motor_sales$Month == "Nov" | motor_sales$Month == "Dec", 4, NA)))
                                    )
motor_sales[c("quarter")] <- motor_sales$quarter * 0.1 + as.numeric(motor_sales$Year)
  # Data is monthly. Sum motor vehicle sales by quarter to get quarterly value
motor_sales <- motor_sales %>% 
  group_by(quarter) %>%
  summarise(Units = sum(Units)) %>%
  rename("vehicle_sales" = Units) %>%
  mutate("lag_vehicle_sales" = lag(vehicle_sales))
  # Merge into external data set
external <- merge(external, motor_sales, by = "quarter")

# Merge Gold data
gold[c("Day", "Month", "Year")] <- str_split_fixed(gold$Quarter, "/", 3)  # Split date into time periods
gold[c("quarter")] <- as.numeric(gold$Year) + 0.1*(as.numeric(gold$Month)/3) + 2000 # Transform dates to year.quarter
colnames(gold)[2] <- "gold" 
gold[c("lag_gold")] <- lag(gold$gold) # Lag Gold
external <- merge(external, gold[c("quarter", "gold", "lag_gold")], by = "quarter")

# Merge Road Deaths data
road_deaths[c("Day", "Month", "Year")] <- str_split_fixed(road_deaths$Quarter, "/", 3)  # Split date into time periods
road_deaths[c("quarter")] <- as.numeric(road_deaths$Year) + 0.1*(as.numeric(road_deaths$Month)/3) # Transform dates to year.quarter
colnames(road_deaths)[2] <- "road_deaths"
external <- merge(external, road_deaths[c("quarter", "road_deaths")], by = "quarter")

# Merge Unemployment data
unemployment[c("Day", "Month", "Year")] <- str_split_fixed(unemployment$Quarter, "/", 3)  # Split date into time periods
unemployment[c("quarter")] <- as.numeric(unemployment$Year) + 0.1*(as.numeric(unemployment$Month)/3) # Transform dates to year.quarter
colnames(unemployment)[2] <- "unemployment"
unemployment[c("lag_unemployment")] <- lag(unemployment$unemployment) # Lag unemployment
external <- merge(external, unemployment[c("quarter", "unemployment", "lag_unemployment")], by = "quarter")

# Merge Maintenance and Repair Index
maintenance[c("Day", "Month", "Year")] <- str_split_fixed(maintenance$Date, "/", 3)  # Split date into time periods
maintenance[c("quarter")] <- as.numeric(maintenance$Year) + 2000 + 0.1*(as.numeric(maintenance$Month)/3) # Transform dates to year.quarter
colnames(maintenance)[8] <- "maintenance_index"
external <- merge(external, maintenance[c("quarter", "maintenance_index")], by = "quarter")

# Merge USD/AUD exchange rate
USDAUD[c("Month", "Day", "Year")] <- str_split_fixed(USDAUD$Date, "/", 3)  # Split date into time periods
USDAUD[c("quarter")] <- as.numeric(USDAUD$Year) + 2000 + 0.1*(as.numeric(USDAUD$Month)/3) # Transform dates to year.quarter
colnames(USDAUD)[2] <- "USDAUD"
external <- merge(external, USDAUD[c("quarter", "USDAUD")], by = "quarter")

# Merge JPY/AUD exchange rate
JPYAUD[c("Month", "Day", "Year")] <- str_split_fixed(JPYAUD$Date, "/", 3)  # Split date into time periods
JPYAUD[c("quarter")] <- as.numeric(JPYAUD$Year) + 2000 + 0.1*(as.numeric(JPYAUD$Month)/3) # Transform dates to year.quarter
colnames(JPYAUD)[2] <- "JPYAUD"
external <- merge(external, JPYAUD[c("quarter", "JPYAUD")], by = "quarter")

# Frequency vs Severity variables
freq <- c("petrol_price", "lag_petrol_price", "vehicle_sales", "lag_vehicle_sales")
severity <- c("CPI_index", "import_index", "lag_CPI_index")


#### FREQUENCY DATA TRANSFORMATION ####

# Cleaning
d1 <- data
d1$Indicator <- ifelse(!d1$total_claims_cost == 0, 1, 0) # Indicator, 1 if claim, 0 if no claim
d1 <- d1 %>% 
  group_by(policy_id, accident_month) %>%
  mutate(Claim_number = sum(Indicator)) # Calculate total claim number per month per policy
d1 <- d1[!d1$exposure == 0,] # Remove 0 exposure entries

# Transforming frequency dataset to quarterly data
quarterly.d1 <- d1 %>%
  group_by(policy_id, quarter) %>%
  summarise(exposure = sum(exposure), 
            Claim_number = sum(Claim_number),
            Indicator = last(Indicator),
            risk_state_name = last(risk_state_name),
            vehicle_class = last(vehicle_class),
            policy_tenure = last(policy_tenure),
            vehicle_risk = last(vehicle_risk))

hist(quarterly.d1$Claim_number)
table(quarterly.d1$Claim_number)  #  0      1      2      3      4      5      6 
                              # 390180   5481    869     49     43      1      3 
visual.d1 <- quarterly.d1 %>%
  group_by(Date) %>%
  summarise(Claim_number = sum(Claim_number),
            Policy_number = n())
ggplot(visual.d1, aes(Date, Claim_number, group = 1))+
  geom_line() 

# Combining External data -- #
quarterly.d1 <- merge(quarterly.d1, external[, !colnames(external) %in% severity], by = "quarter")


#### SEVERITY DATA TRANSFORMATION ####

# Change Severity Data set into quarters, remove no claim entries
d2 <- data %>% filter(total_claims_cost != 0) # Severity data set only has claims
quarterly.d2 <- d2 %>%
  group_by(policy_id, quarter) %>%
  summarise(Mean_claim_amount = mean(total_claims_cost),
            risk_state_name = last(risk_state_name),
            vehicle_class = last(vehicle_class),
            policy_tenure = last(policy_tenure),
            sum_insured = last(sum_insured),
            year_of_manufacture = last(year_of_manufacture),
            vehicle_risk = last(vehicle_risk))

ggplot(quarterly.d2, aes(x = Mean_claim_amount)) +
  geom_histogram(fill = "black", colour = "black", binwidth = 2000)

# Combining External data -- #
quarterly.d2 <- merge(quarterly.d2, external[, !colnames(external) %in% freq], by = "quarter")
quarterly.d2 <- quarterly.d2 %>% mutate(state_group = case_when(
  risk_state_name == "NSW" ~ "NSW",
  risk_state_name == "VIC" ~ "VIC",
  risk_state_name == "QLD" ~ "QLD",
  TRUE ~ "Other"
)) # Grouping States based off preliminary modelling results



### K-FOLD CV for Time Series Data ###

# Function that calculates RMSE of final prediction
rootmse <- function(model, testset, trainingset){
  
  test <- predict(model, newdata = testset, type = "response")  # Prediction for Test set
  train <- predict(model, newdata = trainingset, type = "response")  # Prediction for Training set
  
  testset <- data.table(testset)
  testset <- testset[, pred := test   # Summarise predictions and actual claims by quarter for validation set
  ][, .(real_claims = sum(Claim_number), pred_claims = sum(pred), Date = last(Date)), by = .(quarter)]
  
  trainingset <- data.table(trainingset)
  trainingset <- trainingset[, pred := train   # Summarise predictions and actual claims by quarter for training set
  ][, .(real_claims = sum(Claim_number), pred_claims = sum(pred), Date = last(Date)), by = .(quarter)]
  
  rmse <- data.table("Training RMSE" = sqrt(mean((trainingset$real_claims - trainingset$pred_claims)^2)),
                     "Test RMSE" = sqrt(mean(testset$real_claims - testset$pred_claims)^2))
  return(rmse)
}

  # Using K = 5, split data set into 5 folds.
Date <- unique(quarterly.d1$Date) # 19 quarters. 
split <- rep(1:5, each = 4, length.out = 19) # Using K = 5, split into 5 groups with the last group consisting of 3 quarters only.
split <- cbind(Date, split)

kfold_freq <- merge(quarterly.d1, split, by = "Date")
kfold_sev <- merge(quarterly.d2, split, by = "Date")


##### FREQUENCY MODELLING #####

# Poisson Regression for Claim Frequency

  # Perform k-fold CV on a rolling basis
poissonCV <- data.table("Training RMSE" = numeric(), "Test RMSE" = numeric()) # Create empty table to store results
for(i in 1:4){
  training <- kfold_freq[which(kfold_freq$split <= i), ] 
  test <- kfold_freq[which(kfold_freq$split == i+1), ]
  
  model <- glm(Claim_number ~ vehicle_risk + lag_petrol_price + state_group + policy_tenure + vehicle_sales, data = training, family = "poisson", offset = log(exposure))
  print(summary(model))
  
  poissonCV <- rbind(poissonCV, rootmse(model, test, training))
}
poissonCV

# AIC: 59405
# Residual deviance: 48153
bestfit_so_far <- glm(Claim_number ~ vehicle_risk + lag_petrol_price + state_group + policy_tenure + vehicle_sales, 
                      data = training, family = "poisson", offset = log(exposure)) # Final model
bestfit_pred <- predict(bestfit_so_far, newdata = d1.test, type = "response") # Prediction
bestfit_train <- predict(bestfit_so_far, newdata = d1.train, type = "response")
d1.pred <- cbind(d1.test, bestfit_pred) 

rmse(actual = d1.pred$Claim_number, predicted = bestfit_pred) # 0.149, i.e on average model is predicting +- 0.149 claims more than actual
rmse(actual = d1.train$Claim_number, predicted = bestfit_train)

  # Visualisation
d1.trainpred <- d1.train %>%
  mutate(pred = bestfit_train) %>%
  group_by(quarter) %>%
  summarise(real_claims = sum(Claim_number),
            pred_claims = sum(pred),
            Date = last(Date))
ggplot(d1.trainpred, aes(Date, real_claims, group = 1)) +
  geom_line() +
  geom_line(aes(y = pred_claims, group = 1), color = "blue")

d1.pred <- d1.pred %>%
  group_by(quarter) %>%
  summarise(real_claims = sum(Claim_number),
            pred_claims = sum(bestfit_pred),
            Date = last(Date))

  # Visualisation of prediction of training + testing
visual <- rbind(d1.trainpred, d1.pred)
ggplot(visual, aes(Date, real_claims, group = 1)) +
  geom_line() +
  geom_line(aes(y = pred_claims, group = 1), color = "blue")


###### SEVERITY MODELLING ######

#Gamma Regression for Claim Severity

fit1 <- glm(Mean_claim_amount ~ state_group + sum_insured + vehicle_risk + CPI_index + import_index, data = d2.train, family = Gamma(link = "log"))
summary(fit1)
fit1pred <- predict(fit1, newdata = d2.test, type = "response") #Prediction
fit1rmse <- rmse(actual = d2.test$Mean_claim_amount, predicted = fit1pred) #RMSE: 9034.15 (after removing outliers)
#effect_plot(fit1, pred = import_index, interval = T) #Check effect of predictors

#cor(model.matrix(fit1)[,-1]) #Check Correlation of predictors

fit2 <- glm(Mean_claim_amount ~ state_group + sum_insured + vehicle_risk, data = d2.train, family = Gamma(link = "log"))
summary(fit2)
fit2pred <- predict(fit2, newdata = d2.test, type = "response") #Prediction
fit2rmse <- rmse(actual = d2.test$Mean_claim_amount, predicted = fit2pred) #RMSE: 8887.90


#Inverse Gaussian Regression for Claim Severity

fit3 <- glm(Mean_claim_amount ~ state_group + sum_insured + vehicle_risk + CPI_index, data = d2.train, family = inverse.gaussian(link = "log"))
summary(fit3)
fit3pred <- predict(fit3, newdata = d2.test, type = "response") #Prediction
fit3rmse <- rmse(actual = d2.test$Mean_claim_amount, predicted = fit3pred) #RMSE: 24163.83

fit4 <- glm(Mean_claim_amount ~ state_group + sum_insured + vehicle_risk, data = d2.train, family = inverse.gaussian(link = "log"))
summary(fit4)
fit4pred <- predict(fit4, newdata = d2.test, type = "response") #Prediction
fit4rmse <- rmse(actual = d2.test$Mean_claim_amount, predicted = fit4pred) #RMSE: 22566.37



#####################################################################
#####################################################################
#####################################################################



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

# Chloe - LOOCV for Poisson

install.packages("caret")
library(caret)

#specify the cross-validation method
ctrl <- trainControl(method = "LOOCV")

#fit a regression model and use LOOCV to evaluate performance
model <- train(Claim_number ~ vehicle_risk + 
                 risk_state_name + policy_tenure, data = quarterly.d1, method = "glm", 
               trControl = ctrl, family = "poisson")

#view summary of LOOCV               
print(model)

#### CATHY - zero claims predictions ####
library(ggplot2)
library(dplyr)
library(class)
library(MASS)
library(caret)
library(devtools)
library(countreg)
library(forcats)
library(AER)
library(pscl)
install.packages("countreg", repos="http://R-Forge.R-project.org")
#Attaching data for modeling
data4 <- d1.train
##data partition - original data
data <- data4
data_partition <- createDataPartition(data$Claim_number, times = 1,p = 0.8,list = FALSE)
str(data_partition)
training <- data[data_partition,]
testing  <- data[-data_partition,]
#Re-sampling
sample1 <- subset(data4, Claim_number!=0)
sample2 <- data4[ sample( which(data4$Claim_number==0), 
                          round(0.9*length(which(data4$Claim_number==0)))), ]
sample3 <- data4[ sample( which(data4$Claim_number==0), 
                          round(0.1*length(which(data4$Claim_number==0)))), ]
y <- rnbinom(n = dim(sample3)[1], mu = 1, size = 3) # n value should be equal to sample 3
sample3$Claim_number <- y
df_sample <- rbind(sample1,sample2,sample3)

##data partition - re-sampled data
data <- data4
data_partition <- createDataPartition(data$Claim_number, times = 1,p = 0.8,list = FALSE)
str(data_partition)
training <- data[data_partition,]
testing  <- data[-data_partition,]

#Poisson model with offset
poissonglm <- glm(Claim_number ~ vehicle_risk + lag_petrol_price +
                    risk_state_name + policy_tenure + lag_petrol_price,
                  data=training, family = "poisson", offset=log(exposure))
summary(poissonglm)

# Test for dispersion
dispersiontest(poissonglm,trafo=1)
#Quasipoisson model with weight
qpoissonglm <- glm(Claim_number ~ vehicle_risk + lag_petrol_price +
                     risk_state_name + policy_tenure + lag_petrol_price,
                   data=training, family = "quasipoisson",weight = exposure)
summary(qpoissonglm)
#Negative Binomial model with offset
nbglm <- glm.nb(Claim_number ~ vehicle_risk + lag_petrol_price +
                  risk_state_name + policy_tenure + lag_petrol_price,
                data=training, offset=log(exposure),control = glm.control(maxit=10000))
summary(nbglm)
#Zero Inflation Poisson model with offset
zip <- zeroinfl(Claim_number ~ vehicle_risk + lag_petrol_price +
                  risk_state_name + policy_tenure + lag_petrol_price,
                data=training, offset=log(exposure),dist = "poisson",link= "logit")
summary(zip)
#Zero Inflation Negative Binomial model with offset
zinb <- zeroinfl(Claim_number ~ vehicle_risk + lag_petrol_price +
                   risk_state_name + policy_tenure + lag_petrol_price,
                 offset=log(exposure),data=training,dist = "negbin",link= "logit")
summary(zinb)
#Hurdle Negative Binomial model with offset
hurdlenb <- hurdle(Claim_number ~ vehicle_risk + lag_petrol_price +
                     risk_state_name + policy_tenure + lag_petrol_price, data=training,
                 offset=log(exposure),dist ="negbin",zero.dist = "negbin",link= "logit")
summary(hurdlenb)
#Hurdle Poisson model with offset
hurdlepoisson <- hurdle(Claim_number ~ vehicle_risk + lag_petrol_price +
                          risk_state_name + policy_tenure + lag_petrol_price,
                        data=training, offset=log(exposure),dist ="poisson",zero.dist = "poisson",link= "logit")
summary(hurdlepoisson)
#Save models
save(poissonglm, file = "poissonglm.rda")
save(nbglm, file = "nbglm.rda")
save(zinb, file = "zinb.rda")
save(zip, file = "zip.rda")
save(hurdlepoisson, file = "hurdlepoisson.rda")
save(hurdlenb, file = "hurdlenb.rda")
#Load Models
load("poissonglm.rda")
load("nbglm.rda")
load("zinb.rda")
load("zip.rda")
load("hurdlepoisson.rda")
load("hurdlenb.rda")
# Codes to predict zero claims:
zero_counts <- data.frame(round(c("Obs" = sum(training$Claim_number < 1),
                                  "poissonglm" = sum(exp(-predict(poissonglm, training, type = "response"))),
                                  "nbglm" = sum(dnbinom(0, mu = fitted(nbglm), size = nbglm$theta)),
                                  "hurdlepoisson" = sum(predict(hurdlepoisson, training, type = "prob")[,1]),
                                  "hurdlenb" = sum(predict(hurdlenb, training,type = "prob")[,1]),
                                  "zip" = sum(predict(zip, training,type = "prob")[,1]),
                                  "zinb" = sum(predict(zinb, training,type = "prob")[,1]))))
# Installing and running rootogram
install.packages("countreg", repos="http://R-Forge.R-project.org")
library(countreg)
par(mfrow = c(1, 2))
rootogram(poissonglm,max = 10,main="Poisson") # fit up to count 10
rootogram(nbglm,max = 10,main="NB") # fit up to count 10
par(mfrow = c(1, 1))
rootogram(zip,max = 10,main="ZIP") # fit up to count 10
rootogram(zinb,max = 10,main="ZINB") # fit up to count 10
par(mfrow = c(1, 1))
rootogram(hurdlepoisson,max = 10,main="Hurdle-P")# fit up to count 10
rootogram(hurdlenb,max = 10,main="Hurdle-NB") # fit up to count 10
par(mfrow = c(1, 1))
#Log likelyhood for all the models
models <- list("Pois" = poissonglm, "NB" = nbglm, "ZIP-POI" = zip,"ZIP-NB" = zinb,"Hurdle-POI" = hurdlepoisson,"Hurdle-NB" = hurdlenb)
df_log <- data.frame(rbind(logLik = sapply(models, function(x) round(logLik(x), digits = 0)),
                           Df = sapply(models, function(x) attr(logLik(x), "df"))))

