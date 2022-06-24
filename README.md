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





