# 3142

#### BRIAN ####
library(dplyr)


data <- read.csv("ACTL31425110AssignmentData2022.csv", header = TRUE, stringsAsFactors = TRUE)
summary(data)
data$ï..accident_month <- as.Date(data$ï..accident_month)
data$claim_loss_date <- as.Date(data$claim_loss_date)

test <- na.omit(data)

class(data$ï..accident_month)
class(data$claim_loss_date)

unique(data$risk_state_name)

info <- data %>% 
  group_by(ï..accident_month) %>%
  summarise(ClaimFrequency = length(na.omit(total_claims_cost)),
            ClaimAmount = sum(na.omit(total_claims_cost)))
par(mfrow = c(1,2))
plot(info$ï..accident_month, info$ClaimFrequency)
plot(info$ï..accident_month, info$ClaimAmount)

####
