library(tidyverse)
library(data.table)
library(vars)

setwd('..')
acme <- read.csv("MarketMetricsAssignmentData2.csv")
acme <- as.data.table(acme)

plot(acme$ACME_TV_Unit_Sales[1:72],acme$Willingness_to_Recommend[1:72])
M.lm3=lm(acme$ACME_TV_Unit_Sales[1:72]~acme$Willingness_to_Recommend[1:72])
summary(M.lm3)
abline(lm(acme$ACME_TV_Unit_Sales[1:72]~acme$Willingness_to_Recommend[1:72]))

sales_recommend <- acme[,c('ACME_TV_Unit_Sales','Willingness_to_Recommend')] #ACME Sales and Willingness to Recommend
sales_attitude <- acme[,c('ACME_TV_Unit_Sales','Brand_Attitude')]
sales_awareness <- acme[,c('ACME_TV_Unit_Sales','Brand_Awareness')]
sales_satisfaction <- acme[,c('ACME_TV_Unit_Sales','Satisfaction')]
sales_usage <- acme[,c('ACME_TV_Unit_Sales','Usage')]

var1sales_recommend1 <- VAR(sales_recommend, p=20) #run a vector auto-regression lagging 10 periods
summary(var1sales_recommend1) #view results of the model

var2sales_satisfaction1 <- VAR(sales_satisfaction, p=15)
summary(var2sales_satisfaction1)

var3sales_awareness1 <- VAR(sales_awareness, p=15)
summary(var3sales_awareness1)

var3sales_attitude1 <- VAR(sales_attitude, p=15)
summary(var3sales_attitude1)

var1sales_usage1 <- VAR(sales_usage, p=12)
summary(var1sales_usage1)


share_recommend <- acme[,c('ACME_Unit_Market_Share','Willingness_to_Recommend')] #ACME Sales and Willingness to Recommend
share_attitude <- acme[,c('ACME_Unit_Market_Share','Brand_Attitude')]
share_awareness <- acme[,c('ACME_Unit_Market_Share','Brand_Awareness')]
share_satisfaction <- acme[,c('ACME_Unit_Market_Share','Satisfaction')]
share_usage <- acme[,c('ACME_Unit_Market_Share','Usage')]

var1share_recommend1 <- VAR(share_recommend, p=20) #run a vector auto-regression lagging 10 periods
summary(var1share_recommend1) #view results of the model

var2share_attitude1 <- VAR(share_attitude, p=10)
summary(var2share_attitude1)
#STRONG indicator 6 month out

plot(acme$Brand_Attitude[1:66],acme$ACME_Unit_Market_Share[7:72])
M.lm3=lm(acme$Brand_Attitude[1:66]~acme$ACME_Unit_Market_Share[7:72])
summary(M.lm3)
abline(lm(acme$Brand_Attitude[1:66]~acme$ACME_Unit_Market_Share[7:72]))

var3share_awareness1 <- VAR(share_awareness, p=15)
summary(var3share_awareness1)
#NO SIGNIF

var3share_satisfaction1 <- VAR(share_satisfaction, p=15)
summary(var3share_satisfaction1)
#NO SIGNIF

var1share_usage1 <- VAR(share_usage, p=12)
summary(var1share_usage1)
