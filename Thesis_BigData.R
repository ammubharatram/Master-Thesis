############################################################
##### Load data, install packages and create functions #####
############################################################

rm(list=ls())

library(PRROC)
library(DMwR)
library(OptimalCutpoints)
library(RWeka)
library(xgboost)
library(ROCR)
library(readr)
library(stringr)
library(caret)
library(car)
library(arm)
library(glmnet)
library(caTools)
library(fmsb)
library(xlsx)
library(RSKC)
library(sparcl)
library(factoextra)
library(cluster)
library(NbClust)
library(ggplot2)
library(GGally)
library(VIM)
library(readr)
library(translate)
library(data.table)
library(plyr)
library(dplyr)
library(compare)
library(reshape2)
library(mice)
library(caret)
library(Boruta)
library(cluster)
library(tibble)
library(bestglm)
library(SDEFSR)
library(rsubgroup)
options(java.parameters = "-Xmx50000m")
# VIF
vif = function(df){diag(solve(cor(df)))}
# Concordance
OptimisedConc=function(model)
{
  Data = cbind(model$y, model$fitted.values) 
  ones = Data[Data[,1] == 1,]
  zeros = Data[Data[,1] == 0,]
  conc=matrix(0, dim(zeros)[1], dim(ones)[1])
  disc=matrix(0, dim(zeros)[1], dim(ones)[1])
  ties=matrix(0, dim(zeros)[1], dim(ones)[1])
  for (j in 1:dim(zeros)[1])
  {
    for (i in 1:dim(ones)[1])
    {
      if (ones[i,2]>zeros[j,2])
      {conc[j,i]=1}
      else if (ones[i,2]<zeros[j,2])
      {disc[j,i]=1}
      else if (ones[i,2]==zeros[j,2])
      {ties[j,i]=1}
    }
  }
  Pairs=dim(zeros)[1]*dim(ones)[1]
  PercentConcordance=(sum(conc)/Pairs)*100
  PercentDiscordance=(sum(disc)/Pairs)*100
  PercentTied=(sum(ties)/Pairs)*100
  PercentConcordance=(sum(conc)/Pairs)*100
  PercentDiscordance=(sum(disc)/Pairs)*100
  PercentTied=(sum(ties)/Pairs)*100
  N<-length(model$y)
  gamma<-(sum(conc)-sum(disc))/Pairs
  Somers_D<-(sum(conc)-sum(disc))/(Pairs-sum(ties))
  k_tau_a<-2*(sum(conc)-sum(disc))/(N*(N-1))
  return(list("Percent Concordance"=PercentConcordance,
              "Percent Discordance"=PercentDiscordance,
              "Percent Tied"=PercentTied,
              "Pairs"=Pairs,
              "Gamma"=gamma,
              "Somers D"=Somers_D,
              "Kendall's Tau A"=k_tau_a))
}
# Hosmer-Lemeshow test
hosmerlem = function(y, yhat, g=10){
  cutyhat = cut(yhat,breaks = quantile(yhat, probs=seq(0,1, 1/g)), include.lowest=TRUE)
  obs = xtabs(cbind(1 - y, y) ~ cutyhat)
  expect = xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
  chisq = sum((obs - expect)^2/expect)
  P = 1 - pchisq(chisq, g - 2)
  return(list(chisq=chisq,p.value=P))
}
# Ridit Function
ridit_scores=function(y){
  x = vector('numeric')
  for (i in 1:length(y)) {
    
    x[i]=(sum(y[1:i])-sum(y[length(y):(i)]))
  }
  return(x)
}
# Ridit Function
ridit_scores=function(y){
  x = vector('numeric')
  for (i in 1:length(y)) {
    
    x[i]=(sum(y[1:i])-sum(y[length(y):(i)]))
  }
  return(x)
}


# Mode Function
Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}
# Density GG Function
density_gg = function (df,a,b){
  ggplot(df,aes(x = a)) + geom_density(aes(group = b, colour = b, fill = b), alpha = 0.3)
}
stacked_gg = function (df,a,b){
  ggplot(df, aes(x = a, fill = b)) + 
    geom_bar(position = "fill") +
    theme(axis.text.x = element_text(angle = 90))
}

# Log adjusted for integer variables Function
log_int = function(x){
  ifelse(x==0,-1,log(x))
}
log_neg = function(x){
  ifelse(x < 1 & x > -1,0,ifelse(x<0,-log(abs(x)),log(x)))
}

############################################################
## Step 3 - Feature Selection and Replacing Missing Values ##
#############################################################

#### Feature Selection ####

# Remove initial redundant variables
bigdata<-subset(bigdata,select=-c(CUSTOMER_TYPE,CAL_QUARTER,RISK_SCORE_2,PEP_FLAG))
#CUSTOMER_TYPE (all are corporate in this case), CAL_quarter (because it doesnt contain as much info as event_month)
#RISK_SCORE_2 is redundant and not reliable as lots of secondpartycountry observations missing
#Upon exploring data visually we found PEP_FLAG and SPECIAL_ATTENTION_FLAG are subset of one another and are exactly the same in the newrefineddata dataset

# Because of so much missingness remove all variables with > 20% missingness
rankmissing<-data.frame(sapply(bigdata,function(x) mean(is.na(x))) %>%
                          sort())
refineddata<-subset(bigdata,select=c(SCENARIO,SC01_Amount_Exceding_250000__HUF_,EVENT_MONTH, RAISED_ON,NEW_ACCOUNT_BALANCE,SPECIAL_ATTENTION_FLAG,COUNTRY_OF_RESIDENCE_abbrev,BUSINESS_TYPE2, CUSTOMER_STATUS, NUM_CASES, NUM_ACCOUNTS, CUSTOMER_FOR_DAYS,CUSTOMER_SEGMENT, TIME_PERIOD, CUSTOMER_REGION, SAR, COUNTRY_OF_ORIGIN,STATUS,ORIGINAL_ACCOUNT_CURRENCY,Customer_Equals_Account,EVENT_DATE))

# Reconsider variables where missingness is meaningful
newrefineddata<-subset(bigdata,select=c(SCENARIO,SC01_Amount_Exceding_250000__HUF_,EVENT_MONTH, RAISED_ON,NEW_ACCOUNT_BALANCE,SPECIAL_ATTENTION_FLAG,COUNTRY_OF_RESIDENCE_abbrev,BUSINESS_TYPE2, CUSTOMER_STATUS, NUM_CASES, NUM_ACCOUNTS, CUSTOMER_FOR_DAYS,CUSTOMER_SEGMENT, TIME_PERIOD, CUSTOMER_REGION, SAR, COUNTRY_OF_ORIGIN,STATUS,ORIGINAL_ACCOUNT_CURRENCY,Customer_Equals_Account,DISTRICT_OF_BUDAPEST,CREDIT_DEBIT_CODE,TXN_TYPE,SCOPE,INSTRUMENT,REGION,BASE_CURRENCY_AMOUNT,EVENT_DATE))

#### Replacing Missing Values ####

# Deal with 'missing for a reason' guys first!

#Need to change NA values in the last 7 vars in above data set to character - deal first with the 6 categorical ones
newrefineddata$DISTRICT_OF_BUDAPEST[is.na(newrefineddata$DISTRICT_OF_BUDAPEST)]<-"OUTSIDE_BUDAPEST"
newrefineddata$CREDIT_DEBIT_CODE[is.na(newrefineddata$CREDIT_DEBIT_CODE)]<-"NOT_A_SINGLE_TRANSACTION"
newrefineddata$TXN_TYPE[is.na(newrefineddata$TXN_TYPE)]<-"NOT_A_SINGLE_TRANSACTION"
newrefineddata$INSTRUMENT[is.na(newrefineddata$INSTRUMENT)]<-"NOT_A_SINGLE_TRANSACTION"
newrefineddata$SCOPE[is.na(newrefineddata$SCOPE)]<-"NOT_A_SINGLE_TRANSACTION"
newrefineddata$REGION[is.na(newrefineddata$REGION)]<-"NOT_A_SINGLE_TRANSACTION"

#How to deal with BASE_CURRENCY_AMOUNT (missingness is for a reason but it is a continuous variable)
#Option 1: Impute missing vals and treat as continuous variable (but way too many imputations)
#Option 2: Delete missing value stuff (but way too many things to delete)
#Option 3: Make variable into categorical and have NA's as a category (need to decide on categories) - best option!!!
newrefineddata$BASE_CURRENCY_AMOUNT <- newrefineddata$BASE_CURRENCY_AMOUNT  %>% cut(c(quantile(newrefineddata$BASE_CURRENCY_AMOUNT,
                                                                                        na.rm = T))) %>% addNA()
newrefineddata$BASE_CURRENCY_AMOUNT<-as.character(newrefineddata$BASE_CURRENCY_AMOUNT)
newrefineddata$BASE_CURRENCY_AMOUNT[is.na(newrefineddata$BASE_CURRENCY_AMOUNT)]<-"NOT_A_SINGLE_TRANSACTION"
#Note: We tried other categories and none were so informative, best we could do with so much missingness

# Now deal with 'not missing for a reason' guys! - look for patterns then impute

## Country_of_origin ##
#see "CODE_FOR_CHECKING_SOME_THINGS for reason why we imputed like this
newrefineddata$COUNTRY_OF_ORIGIN[is.na(newrefineddata$COUNTRY_OF_ORIGIN)]<-"MAGYARORSZÁG"

## STATUS, CUSTOMER_REGION, ORIGINAL_ACC_CURRENCY, CUSTOMER_EQUALS_ACCOUNT, NEW_ACCOUNT_BALANCE ##
# See 'Code_for_checking_some_things' to see nature of missingness of these guys and looking for patterns

#impute new account BALANCE, status, orig acc currency, customer region and customer equals account
newrefineddata[is.na(newrefineddata$NEW_ACCOUNT_BALANCE) == TRUE, c("NEW_ACCOUNT_BALANCE")]  <- mean(as.numeric(newrefineddata[['NEW_ACCOUNT_BALANCE']]), na.rm = T)
# imputed by 3159960
newrefineddata[is.na(newrefineddata$STATUS) == TRUE, "STATUS"]  <- Mode(newrefineddata[['STATUS']])
# imputed by "OPENED"
newrefineddata[is.na(newrefineddata$ORIGINAL_ACCOUNT_CURRENCY) == TRUE, "ORIGINAL_ACCOUNT_CURRENCY"]  <- Mode(newrefineddata[['ORIGINAL_ACCOUNT_CURRENCY']])
# imputed by "HUF"
newrefineddata[is.na(newrefineddata$Customer_Equals_Account) == TRUE, "Customer_Equals_Account"]  <- Mode(newrefineddata[['Customer_Equals_Account']])
# imputed by "0"
newrefineddata[is.na(newrefineddata$CUSTOMER_REGION) == TRUE & (newrefineddata$COUNTRY_OF_RESIDENCE_abbrev=="HU"), "CUSTOMER_REGION"]  <- Mode(newrefineddata[['CUSTOMER_REGION']])
# imputed as Budapest
# call the other NA's "OUTSIDE_HUNGARY"
newrefineddata$CUSTOMER_REGION[is.na(newrefineddata$CUSTOMER_REGION)]<-"OUTSIDE_HUNGARY"

# now convert all character vars to factors and all continuous variables to numeric or int
newrefineddata$SCENARIO<-as.factor(newrefineddata$SCENARIO)
newrefineddata$EVENT_MONTH<-as.factor(newrefineddata$EVENT_MONTH)
newrefineddata$RAISED_ON<-as.factor(newrefineddata$RAISED_ON)
newrefineddata$COUNTRY_OF_RESIDENCE_abbrev<-as.factor(newrefineddata$COUNTRY_OF_RESIDENCE_abbrev)
newrefineddata$BUSINESS_TYPE2<-as.factor(newrefineddata$BUSINESS_TYPE2)
newrefineddata$CUSTOMER_STATUS<-as.factor(newrefineddata$CUSTOMER_STATUS)
newrefineddata$CUSTOMER_SEGMENT<-as.factor(newrefineddata$CUSTOMER_SEGMENT)
newrefineddata$NUM_CASES<-as.numeric(newrefineddata$NUM_CASES)
newrefineddata$TIME_PERIOD<-as.factor(newrefineddata$TIME_PERIOD)
newrefineddata$NUM_ACCOUNTS<-as.numeric(newrefineddata$NUM_ACCOUNTS)
newrefineddata$CUSTOMER_FOR_DAYS<-as.numeric(newrefineddata$CUSTOMER_FOR_DAYS)
newrefineddata$CUSTOMER_REGION<-as.factor(newrefineddata$CUSTOMER_REGION)
newrefineddata$SAR<-as.factor(newrefineddata$SAR)
newrefineddata$ORIGINAL_ACCOUNT_CURRENCY<-as.factor(newrefineddata$ORIGINAL_ACCOUNT_CURRENCY)
newrefineddata$Customer_Equals_Account<-as.factor(newrefineddata$Customer_Equals_Account)
newrefineddata$DISTRICT_OF_BUDAPEST<-as.factor(newrefineddata$DISTRICT_OF_BUDAPEST)
newrefineddata$CREDIT_DEBIT_CODE<-as.factor(newrefineddata$CREDIT_DEBIT_CODE)
newrefineddata$TXN_TYPE<-as.factor(newrefineddata$TXN_TYPE)
newrefineddata$SCOPE<-as.factor(newrefineddata$SCOPE)
newrefineddata$INSTRUMENT<-as.factor(newrefineddata$INSTRUMENT)
newrefineddata$REGION<-as.factor(newrefineddata$REGION)
newrefineddata$BASE_CURRENCY_AMOUNT<-as.factor(newrefineddata$BASE_CURRENCY_AMOUNT)
newrefineddata$COUNTRY_OF_ORIGIN<-as.factor(newrefineddata$COUNTRY_OF_ORIGIN)
newrefineddata$SPECIAL_ATTENTION_FLAG<-as.factor(newrefineddata$SPECIAL_ATTENTION_FLAG)
newrefineddata$STATUS<-as.factor(newrefineddata$STATUS)
newrefineddata$SC01_Amount_Exceding_250000__HUF_<-as.factor(newrefineddata$SC01_Amount_Exceding_250000__HUF_)
newrefineddata$EVENT_DATE<-(as.Date(newrefineddata$EVENT_DATE, format="%m/%d/%Y"))
# We are not doing out-of-time test so we dont need this newrefineddata<-arrange(newrefineddata, EVENT_DATE) #ordered earliest to latest
str(newrefineddata)
names(newrefineddata)

# recheck missingness
rankmissing<-data.frame(sapply(newrefineddata,function(x) mean(is.na(x))))
#all good

#######################################################
## Step 4 - Exploratory Analysis and Standardization ##
#######################################################

#### Exploratory Analysis And Making Data Set Copy ####

## Find variables most related to the response ##

#info gain value
library(FSelector)
infovalue <- gain.ratio(SAR~., newrefineddata)
varnames<-data.frame(row.names(infovalue))
infovalue$varnames<-data.frame(row.names(infovalue))
sortedinfogain<-infovalue[order(infovalue$attr_importance,decreasing=TRUE),c(1,2)]
sortedinfogain<-subset(sortedinfogain,select=c(attr_importance))
# we see that the 3 worst vars here are SC01_, raises on, and instrument

#chi square variable importance
weights <- chi.squared(SAR~., newrefineddata)
varnames<-data.frame(row.names(weights))
weights$varnames<-data.frame(row.names(weights))
sortedchi<-weights[order(weights$attr_importance,decreasing=TRUE),c(1,2)]
sortedchi<-subset(sortedinfogain,select=c(attr_importance))


## Visual display of how good the categorical variables are as predictors of SAR ##

ggplot(newrefineddata, aes(x = SCENARIO, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#SC11 pretty good but not so many obs

ggplot(newrefineddata, aes(x = SC01_Amount_Exceding_250000__HUF_, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#really bad as predictor

ggplot(newrefineddata, aes(x = EVENT_MONTH, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
# pretty okay

ggplot(newrefineddata, aes(x = RAISED_ON, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
# realy bad

ggplot(newrefineddata, aes(x = SPECIAL_ATTENTION_FLAG, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))

ggplot(newrefineddata, aes(x = BASE_CURRENCY_AMOUNT, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
# really bad var

ggplot(newrefineddata, aes(x = COUNTRY_OF_RESIDENCE_abbrev, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
# might be a good idea to group some groups here with very few obs into one group! - good var

ggplot(newrefineddata, aes(x = BUSINESS_TYPE2, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
# good

ggplot(newrefineddata, aes(x = CUSTOMER_STATUS, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
# kind of informative

ggplot(newrefineddata, aes(x = CUSTOMER_SEGMENT, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#very good

ggplot(newrefineddata, aes(x = TIME_PERIOD, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
# good var 

ggplot(newrefineddata, aes(x = CUSTOMER_REGION, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
# quite good 

ggplot(newrefineddata, aes(x = COUNTRY_OF_ORIGIN, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#pretty good

ggplot(newrefineddata, aes(x = STATUS, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
# good

ggplot(newrefineddata, aes(x = ORIGINAL_ACCOUNT_CURRENCY, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#good

ggplot(newrefineddata, aes(x = Customer_Equals_Account, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#not so great

ggplot(newrefineddata, aes(x = DISTRICT_OF_BUDAPEST, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#prett okay

ggplot(newrefineddata, aes(x = CREDIT_DEBIT_CODE, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#not very good

ggplot(newrefineddata, aes(x = TXN_TYPE, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#not great

ggplot(newrefineddata, aes(x = INSTRUMENT, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#bad

ggplot(newrefineddata, aes(x = SCOPE, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
# okay

ggplot(newrefineddata, aes(x = REGION, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
# bad 

###### Sidenote: Create different data set for clustering purposes without ridit scores ######

# Also make a copy of newrefineddata set with original variables included - for future use of interpretting clusters!
newrefineddataoriginal<-newrefineddata
#############################################################################################

#### Standarizing variables ####

## For Categoricals ##

# SCENARIO
tab1=prop.table(table(newrefineddata$SAR,newrefineddata$SCENARIO),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab1) = c("Var1", "SCENARIO", "Freq_SAR")  

tab1_2=prop.table(table(newrefineddata$SCENARIO)) %>%
  data.frame()
colnames(tab1_2) = c('SCENARIO', 'Freq')
joined_tab1=left_join(tab1,tab1_2, by = 'SCENARIO')

ridit_tab1=ridit_scores(joined_tab1$Freq) %>% data.frame()
colnames(ridit_tab1)='Ridit_Scores'
ridit_tab1=bind_cols(joined_tab1,ridit_tab1) %>% select(c('SCENARIO','Ridit_Scores'))
colnames(ridit_tab1)=c('SCENARIO','RS_SCENARIO')
newrefineddata=left_join(newrefineddata,ridit_tab1,by = 'SCENARIO')
newrefineddata<-subset(newrefineddata,select=-c(SCENARIO))# remove original

# EVENT_MONTH
tab2=prop.table(table(newrefineddata$SAR,newrefineddata$EVENT_MONTH),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab2) = c("Var1", "EVENT_MONTH", "Freq_SAR")  

tab2_2=prop.table(table(newrefineddata$EVENT_MONTH)) %>%
  data.frame()
colnames(tab2_2) = c('EVENT_MONTH', 'Freq')
joined_tab2=left_join(tab2,tab2_2, by = 'EVENT_MONTH')

ridit_tab2=ridit_scores(joined_tab2$Freq) %>% data.frame()
colnames(ridit_tab2)='Ridit_Scores'
ridit_tab2=bind_cols(joined_tab2,ridit_tab2) %>% select(c('EVENT_MONTH','Ridit_Scores'))
colnames(ridit_tab2)=c('EVENT_MONTH','RS_EVENT_MONTH')
newrefineddata=left_join(newrefineddata,ridit_tab2,by = 'EVENT_MONTH')
newrefineddata<-subset(newrefineddata,select=-c(EVENT_MONTH))# remove original

# RAISED_ON
tab3=prop.table(table(newrefineddata$SAR,newrefineddata$RAISED_ON),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab3) = c("Var1", "RAISED_ON", "Freq_SAR")  

tab3_2=prop.table(table(newrefineddata$RAISED_ON)) %>%
  data.frame()
colnames(tab3_2) = c('RAISED_ON', 'Freq')
joined_tab3=left_join(tab3,tab3_2, by = 'RAISED_ON')

ridit_tab3=ridit_scores(joined_tab3$Freq) %>% data.frame()
colnames(ridit_tab3)='Ridit_Scores'
ridit_tab3=bind_cols(joined_tab3,ridit_tab3) %>% select(c('RAISED_ON','Ridit_Scores'))
colnames(ridit_tab3)=c('RAISED_ON','RS_RAISED_ON')
newrefineddata=left_join(newrefineddata,ridit_tab3,by = 'RAISED_ON')
newrefineddata<-subset(newrefineddata,select=-c(RAISED_ON))# remove original

# SPECIAL_ATTENTION_FLAG
tab4=prop.table(table(newrefineddata$SAR,newrefineddata$SPECIAL_ATTENTION_FLAG),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab4) = c("Var1", "SPECIAL_ATTENTION_FLAG", "Freq_SAR")  

tab4_2=prop.table(table(newrefineddata$SPECIAL_ATTENTION_FLAG)) %>%
  data.frame()
colnames(tab4_2) = c('SPECIAL_ATTENTION_FLAG', 'Freq')
joined_tab4=left_join(tab4,tab4_2, by = 'SPECIAL_ATTENTION_FLAG')

ridit_tab4=ridit_scores(joined_tab4$Freq) %>% data.frame()
colnames(ridit_tab4)='Ridit_Scores'
ridit_tab4=bind_cols(joined_tab4,ridit_tab4) %>% select(c('SPECIAL_ATTENTION_FLAG','Ridit_Scores'))
colnames(ridit_tab4)=c('SPECIAL_ATTENTION_FLAG','RS_SPECIAL_ATTENTION_FLAG')
newrefineddata=left_join(newrefineddata,ridit_tab4,by = 'SPECIAL_ATTENTION_FLAG')
newrefineddata<-subset(newrefineddata,select=-c(SPECIAL_ATTENTION_FLAG))# remove original


# COUNTRY_OF_RESIDENCE_abbrev
tab5=prop.table(table(newrefineddata$SAR,newrefineddata$COUNTRY_OF_RESIDENCE_abbrev),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab5) = c("Var1", "COUNTRY_OF_RESIDENCE_abbrev", "Freq_SAR")  

tab5_2=prop.table(table(newrefineddata$COUNTRY_OF_RESIDENCE_abbrev)) %>%
  data.frame()
colnames(tab5_2) = c('COUNTRY_OF_RESIDENCE_abbrev', 'Freq')
joined_tab5=left_join(tab5,tab5_2, by = 'COUNTRY_OF_RESIDENCE_abbrev')

ridit_tab5=ridit_scores(joined_tab5$Freq) %>% data.frame()
colnames(ridit_tab5)='Ridit_Scores'
ridit_tab5=bind_cols(joined_tab5,ridit_tab5) %>% select(c('COUNTRY_OF_RESIDENCE_abbrev','Ridit_Scores'))
colnames(ridit_tab5)=c('COUNTRY_OF_RESIDENCE_abbrev','RS_COUNTRY_OF_RESIDENCE_abbrev')
newrefineddata=left_join(newrefineddata,ridit_tab5,by = 'COUNTRY_OF_RESIDENCE_abbrev')
newrefineddata<-subset(newrefineddata,select=-c(COUNTRY_OF_RESIDENCE_abbrev))# remove original

# BUSINESS_TYPE2
tab6=prop.table(table(newrefineddata$SAR,newrefineddata$BUSINESS_TYPE2),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab6) = c("Var1", "BUSINESS_TYPE2", "Freq_SAR")  

tab6_2=prop.table(table(newrefineddata$BUSINESS_TYPE2)) %>%
  data.frame()
colnames(tab6_2) = c('BUSINESS_TYPE2', 'Freq')
joined_tab6=left_join(tab6,tab6_2, by = 'BUSINESS_TYPE2')

ridit_tab6=ridit_scores(joined_tab6$Freq) %>% data.frame()
colnames(ridit_tab6)='Ridit_Scores'
ridit_tab6=bind_cols(joined_tab6,ridit_tab6) %>% select(c('BUSINESS_TYPE2','Ridit_Scores'))
colnames(ridit_tab6)=c('BUSINESS_TYPE2','RS_BUSINESS_TYPE2')
newrefineddata=left_join(newrefineddata,ridit_tab6,by = 'BUSINESS_TYPE2')
newrefineddata<-subset(newrefineddata,select=-c(BUSINESS_TYPE2))# remove original

# CUSTOMER_STATUS
tab7=prop.table(table(newrefineddata$SAR,newrefineddata$CUSTOMER_STATUS),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab7) = c("Var1", "CUSTOMER_STATUS", "Freq_SAR")  

tab7_2=prop.table(table(newrefineddata$CUSTOMER_STATUS)) %>%
  data.frame()
colnames(tab7_2) = c('CUSTOMER_STATUS', 'Freq')
joined_tab7=left_join(tab7,tab7_2, by = 'CUSTOMER_STATUS')

ridit_tab7=ridit_scores(joined_tab7$Freq) %>% data.frame()
colnames(ridit_tab7)='Ridit_Scores'
ridit_tab7=bind_cols(joined_tab7,ridit_tab7) %>% select(c('CUSTOMER_STATUS','Ridit_Scores'))
colnames(ridit_tab7)=c('CUSTOMER_STATUS','RS_CUSTOMER_STATUS')
newrefineddata=left_join(newrefineddata,ridit_tab7,by = 'CUSTOMER_STATUS')
newrefineddata<-subset(newrefineddata,select=-c(CUSTOMER_STATUS))# remove original

# CUSTOMER_SEGMENT
tab8=prop.table(table(newrefineddata$SAR,newrefineddata$CUSTOMER_SEGMENT),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab8) = c("Var1", "CUSTOMER_SEGMENT", "Freq_SAR")  

tab8_2=prop.table(table(newrefineddata$CUSTOMER_SEGMENT)) %>%
  data.frame()
colnames(tab8_2) = c('CUSTOMER_SEGMENT', 'Freq')
joined_tab8=left_join(tab8,tab8_2, by = 'CUSTOMER_SEGMENT')

ridit_tab8=ridit_scores(joined_tab8$Freq) %>% data.frame()
colnames(ridit_tab8)='Ridit_Scores'
ridit_tab8=bind_cols(joined_tab8,ridit_tab8) %>% select(c('CUSTOMER_SEGMENT','Ridit_Scores'))
colnames(ridit_tab8)=c('CUSTOMER_SEGMENT','RS_CUSTOMER_SEGMENT')
newrefineddata=left_join(newrefineddata,ridit_tab8,by = 'CUSTOMER_SEGMENT')
newrefineddata<-subset(newrefineddata,select=-c(CUSTOMER_SEGMENT))# remove original

# TIME_PERIOD
tab9=prop.table(table(newrefineddata$SAR,newrefineddata$TIME_PERIOD),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab9) = c("Var1", "TIME_PERIOD", "Freq_SAR")  

tab9_2=prop.table(table(newrefineddata$TIME_PERIOD)) %>%
  data.frame()
colnames(tab9_2) = c('TIME_PERIOD', 'Freq')
joined_tab9=left_join(tab9,tab9_2, by = 'TIME_PERIOD')

ridit_tab9=ridit_scores(joined_tab9$Freq) %>% data.frame()
colnames(ridit_tab9)='Ridit_Scores'
ridit_tab9=bind_cols(joined_tab9,ridit_tab9) %>% select(c('TIME_PERIOD','Ridit_Scores'))
colnames(ridit_tab9)=c('TIME_PERIOD','RS_TIME_PERIOD')
newrefineddata=left_join(newrefineddata,ridit_tab9,by = 'TIME_PERIOD')
newrefineddata<-subset(newrefineddata,select=-c(TIME_PERIOD))# remove original

# CUSTOMER_REGION
tab10=prop.table(table(newrefineddata$SAR,newrefineddata$CUSTOMER_REGION),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab10) = c("Var1", "CUSTOMER_REGION", "Freq_SAR")  

tab10_2=prop.table(table(newrefineddata$CUSTOMER_REGION)) %>%
  data.frame()
colnames(tab10_2) = c('CUSTOMER_REGION', 'Freq')
joined_tab10=left_join(tab10,tab10_2, by = 'CUSTOMER_REGION')

ridit_tab10=ridit_scores(joined_tab10$Freq) %>% data.frame()
colnames(ridit_tab10)='Ridit_Scores'
ridit_tab10=bind_cols(joined_tab10,ridit_tab10) %>% select(c('CUSTOMER_REGION','Ridit_Scores'))
colnames(ridit_tab10)=c('CUSTOMER_REGION','RS_CUSTOMER_REGION')
newrefineddata=left_join(newrefineddata,ridit_tab10,by = 'CUSTOMER_REGION')
newrefineddata<-subset(newrefineddata,select=-c(CUSTOMER_REGION))# remove original

# COUNTRY_OF_ORIGIN
tab11=prop.table(table(newrefineddata$SAR,newrefineddata$COUNTRY_OF_ORIGIN),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab11) = c("Var1", "COUNTRY_OF_ORIGIN", "Freq_SAR")  

tab11_2=prop.table(table(newrefineddata$COUNTRY_OF_ORIGIN)) %>%
  data.frame()
colnames(tab11_2) = c('COUNTRY_OF_ORIGIN', 'Freq')
joined_tab11=left_join(tab11,tab11_2, by = 'COUNTRY_OF_ORIGIN')

ridit_tab11=ridit_scores(joined_tab11$Freq) %>% data.frame()
colnames(ridit_tab11)='Ridit_Scores'
ridit_tab11=bind_cols(joined_tab11,ridit_tab11) %>% select(c('COUNTRY_OF_ORIGIN','Ridit_Scores'))
colnames(ridit_tab11)=c('COUNTRY_OF_ORIGIN','RS_COUNTRY_OF_ORIGIN')
newrefineddata=left_join(newrefineddata,ridit_tab11,by = 'COUNTRY_OF_ORIGIN')
newrefineddata<-subset(newrefineddata,select=-c(COUNTRY_OF_ORIGIN))# remove original

# STATUS
tab12=prop.table(table(newrefineddata$SAR,newrefineddata$STATUS),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab12) = c("Var1", "STATUS", "Freq_SAR")  

tab12_2=prop.table(table(newrefineddata$STATUS)) %>%
  data.frame()
colnames(tab12_2) = c('STATUS', 'Freq')
joined_tab12=left_join(tab12,tab12_2, by = 'STATUS')

ridit_tab12=ridit_scores(joined_tab12$Freq) %>% data.frame()
colnames(ridit_tab12)='Ridit_Scores'
ridit_tab12=bind_cols(joined_tab12,ridit_tab12) %>% select(c('STATUS','Ridit_Scores'))
colnames(ridit_tab12)=c('STATUS','RS_STATUS')
newrefineddata=left_join(newrefineddata,ridit_tab12,by = 'STATUS')
newrefineddata<-subset(newrefineddata,select=-c(STATUS))# remove original

# ORIGINAL_ACCOUNT_CURRENCY
tab13=prop.table(table(newrefineddata$SAR,newrefineddata$ORIGINAL_ACCOUNT_CURRENCY),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab13) = c("Var1", "ORIGINAL_ACCOUNT_CURRENCY", "Freq_SAR")  

tab13_2=prop.table(table(newrefineddata$ORIGINAL_ACCOUNT_CURRENCY)) %>%
  data.frame()
colnames(tab13_2) = c('ORIGINAL_ACCOUNT_CURRENCY', 'Freq')
joined_tab13=left_join(tab13,tab13_2, by = 'ORIGINAL_ACCOUNT_CURRENCY')

ridit_tab13=ridit_scores(joined_tab13$Freq) %>% data.frame()
colnames(ridit_tab13)='Ridit_Scores'
ridit_tab13=bind_cols(joined_tab13,ridit_tab13) %>% select(c('ORIGINAL_ACCOUNT_CURRENCY','Ridit_Scores'))
colnames(ridit_tab13)=c('ORIGINAL_ACCOUNT_CURRENCY','RS_ORIGINAL_ACCOUNT_CURRENCY')
newrefineddata=left_join(newrefineddata,ridit_tab13,by = 'ORIGINAL_ACCOUNT_CURRENCY')
newrefineddata<-subset(newrefineddata,select=-c(ORIGINAL_ACCOUNT_CURRENCY))# remove original

# CUSTOMER_EQUALS_ACCOUNT
tab14=prop.table(table(newrefineddata$SAR,newrefineddata$Customer_Equals_Account),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab14) = c("Var1", "Customer_Equals_Account", "Freq_SAR")  

tab14_2=prop.table(table(newrefineddata$Customer_Equals_Account)) %>%
  data.frame()
colnames(tab14_2) = c('Customer_Equals_Account', 'Freq')
joined_tab14=left_join(tab14,tab14_2, by = 'Customer_Equals_Account')

ridit_tab14=ridit_scores(joined_tab14$Freq) %>% data.frame()
colnames(ridit_tab14)='Ridit_Scores'
ridit_tab14=bind_cols(joined_tab14,ridit_tab14) %>% select(c('Customer_Equals_Account','Ridit_Scores'))
colnames(ridit_tab14)=c('Customer_Equals_Account','RS_Customer_Equals_Account')
newrefineddata=left_join(newrefineddata,ridit_tab14,by = 'Customer_Equals_Account')
newrefineddata<-subset(newrefineddata,select=-c(Customer_Equals_Account))# remove original

# DISTRICT_OF_BUDAPEST
tab15=prop.table(table(newrefineddata$DISTRICT_OF_BUDAPEST,newrefineddata$DISTRICT_OF_BUDAPEST),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab15) = c("Var1", "DISTRICT_OF_BUDAPEST", "Freq_SAR")  

tab15_2=prop.table(table(newrefineddata$DISTRICT_OF_BUDAPEST)) %>%
  data.frame()
colnames(tab15_2) = c('DISTRICT_OF_BUDAPEST', 'Freq')
joined_tab15=left_join(tab15,tab15_2, by = 'DISTRICT_OF_BUDAPEST')

ridit_tab15=ridit_scores(joined_tab15$Freq) %>% data.frame()
colnames(ridit_tab15)='Ridit_Scores'
ridit_tab15=bind_cols(joined_tab15,ridit_tab15) %>% select(c('DISTRICT_OF_BUDAPEST','Ridit_Scores'))
colnames(ridit_tab15)=c('DISTRICT_OF_BUDAPEST','RS_DISTRICT_OF_BUDAPEST')
newrefineddata=left_join(newrefineddata,ridit_tab15,by = 'DISTRICT_OF_BUDAPEST')
newrefineddata<-subset(newrefineddata,select=-c(DISTRICT_OF_BUDAPEST))# remove original

# CREDI_DEBIT_CODE
tab16=prop.table(table(newrefineddata$SAR,newrefineddata$CREDIT_DEBIT_CODE),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab16) = c("Var1", "CREDIT_DEBIT_CODE", "Freq_SAR")  

tab16_2=prop.table(table(newrefineddata$CREDIT_DEBIT_CODE)) %>%
  data.frame()
colnames(tab16_2) = c('CREDIT_DEBIT_CODE', 'Freq')
joined_tab16=left_join(tab16,tab16_2, by = 'CREDIT_DEBIT_CODE')

ridit_tab16=ridit_scores(joined_tab16$Freq) %>% data.frame()
colnames(ridit_tab16)='Ridit_Scores'
ridit_tab16=bind_cols(joined_tab16,ridit_tab16) %>% select(c('CREDIT_DEBIT_CODE','Ridit_Scores'))
colnames(ridit_tab16)=c('CREDIT_DEBIT_CODE','RS_CREDIT_DEBIT_CODE')
newrefineddata=left_join(newrefineddata,ridit_tab16,by = 'CREDIT_DEBIT_CODE')
newrefineddata<-subset(newrefineddata,select=-c(CREDIT_DEBIT_CODE))# remove original

# TXN_TYPE
tab17=prop.table(table(newrefineddata$SAR,newrefineddata$TXN_TYPE),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab17) = c("Var1", "TXN_TYPE", "Freq_SAR")  

tab17_2=prop.table(table(newrefineddata$TXN_TYPE)) %>%
  data.frame()
colnames(tab17_2) = c('TXN_TYPE', 'Freq')
joined_tab17=left_join(tab17,tab17_2, by = 'TXN_TYPE')

ridit_tab17=ridit_scores(joined_tab17$Freq) %>% data.frame()
colnames(ridit_tab17)='Ridit_Scores'
ridit_tab17=bind_cols(joined_tab17,ridit_tab17) %>% select(c('TXN_TYPE','Ridit_Scores'))
colnames(ridit_tab17)=c('TXN_TYPE','RS_TXN_TYPE')
newrefineddata=left_join(newrefineddata,ridit_tab17,by = 'TXN_TYPE')
newrefineddata<-subset(newrefineddata,select=-c(TXN_TYPE))# remove original

# INSTRUMENT
tab18=prop.table(table(newrefineddata$SAR,newrefineddata$INSTRUMENT),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab18) = c("Var1", "INSTRUMENT", "Freq_SAR")  

tab18_2=prop.table(table(newrefineddata$INSTRUMENT)) %>%
  data.frame()
colnames(tab18_2) = c('INSTRUMENT', 'Freq')
joined_tab18=left_join(tab18,tab18_2, by = 'INSTRUMENT')

ridit_tab18=ridit_scores(joined_tab18$Freq) %>% data.frame()
colnames(ridit_tab18)='Ridit_Scores'
ridit_tab18=bind_cols(joined_tab18,ridit_tab18) %>% select(c('INSTRUMENT','Ridit_Scores'))
colnames(ridit_tab18)=c('INSTRUMENT','RS_INSTRUMENT')
newrefineddata=left_join(newrefineddata,ridit_tab18,by = 'INSTRUMENT')
newrefineddata<-subset(newrefineddata,select=-c(INSTRUMENT))# remove original

# SCOPE
tab19=prop.table(table(newrefineddata$SAR,newrefineddata$SCOPE),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab19) = c("Var1", "SCOPE", "Freq_SAR")  

tab19_2=prop.table(table(newrefineddata$SCOPE)) %>%
  data.frame()
colnames(tab19_2) = c('SCOPE', 'Freq')
joined_tab19=left_join(tab19,tab19_2, by = 'SCOPE')

ridit_tab19=ridit_scores(joined_tab19$Freq) %>% data.frame()
colnames(ridit_tab19)='Ridit_Scores'
ridit_tab19=bind_cols(joined_tab19,ridit_tab19) %>% select(c('SCOPE','Ridit_Scores'))
colnames(ridit_tab19)=c('SCOPE','RS_SCOPE')
newrefineddata=left_join(newrefineddata,ridit_tab19,by = 'SCOPE')
newrefineddata<-subset(newrefineddata,select=-c(SCOPE))# remove original

# REGION
tab20=prop.table(table(newrefineddata$SAR,newrefineddata$REGION),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab20) = c("Var1", "REGION", "Freq_SAR")  

tab20_2=prop.table(table(newrefineddata$REGION)) %>%
  data.frame()
colnames(tab20_2) = c('REGION', 'Freq')
joined_tab20=left_join(tab20,tab20_2, by = 'REGION')

ridit_tab20=ridit_scores(joined_tab20$Freq) %>% data.frame()
colnames(ridit_tab20)='Ridit_Scores'
ridit_tab20=bind_cols(joined_tab20,ridit_tab20) %>% select(c('REGION','Ridit_Scores'))
colnames(ridit_tab20)=c('REGION','RS_REGION')
newrefineddata=left_join(newrefineddata,ridit_tab20,by = 'REGION')
newrefineddata<-subset(newrefineddata,select=-c(REGION))# remove original

# BASE_CURRENCY_AMOUNT
tab21=prop.table(table(newrefineddata$SAR,newrefineddata$BASE_CURRENCY_AMOUNT),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab21) = c("Var1", "BASE_CURRENCY_AMOUNT", "Freq_SAR")  

tab21_2=prop.table(table(newrefineddata$BASE_CURRENCY_AMOUNT)) %>%
  data.frame()
colnames(tab21_2) = c('BASE_CURRENCY_AMOUNT', 'Freq')
joined_tab21=left_join(tab21,tab21_2, by = 'BASE_CURRENCY_AMOUNT')

ridit_tab21=ridit_scores(joined_tab21$Freq) %>% data.frame()
colnames(ridit_tab21)='Ridit_Scores'
ridit_tab21=bind_cols(joined_tab21,ridit_tab21) %>% select(c('BASE_CURRENCY_AMOUNT','Ridit_Scores'))
colnames(ridit_tab21)=c('BASE_CURRENCY_AMOUNT','RS_BASE_CURRENCY_AMOUNT')
newrefineddata=left_join(newrefineddata,ridit_tab21,by = 'BASE_CURRENCY_AMOUNT')
newrefineddata<-subset(newrefineddata,select=-c(BASE_CURRENCY_AMOUNT))# remove original

# SC01
tab22=prop.table(table(newrefineddata$SAR,newrefineddata$SC01_Amount_Exceding_250000__HUF_),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab22) = c("Var1", "SC01_Amount_Exceding_250000__HUF_", "Freq_SAR")  

tab22_2=prop.table(table(newrefineddata$SC01_Amount_Exceding_250000__HUF_)) %>%
  data.frame()
colnames(tab22_2) = c('SC01_Amount_Exceding_250000__HUF_', 'Freq')
joined_tab22=left_join(tab22,tab22_2, by = 'SC01_Amount_Exceding_250000__HUF_')

ridit_tab22=ridit_scores(joined_tab22$Freq) %>% data.frame()
colnames(ridit_tab22)='Ridit_Scores'
ridit_tab22=bind_cols(joined_tab22,ridit_tab22) %>% select(c('SC01_Amount_Exceding_250000__HUF_','Ridit_Scores'))
colnames(ridit_tab22)=c('SC01_Amount_Exceding_250000__HUF_','RS_SC01_Amount_Exceding_250000__HUF_')
newrefineddata=left_join(newrefineddata,ridit_tab22,by = 'SC01_Amount_Exceding_250000__HUF_')
newrefineddata<-subset(newrefineddata,select=-c(SC01_Amount_Exceding_250000__HUF_))# remove original

rm(list=ls(pattern=c('tab')))

# A check to see does the variable important change much
infovalue <- gain.ratio(SAR~., newrefineddata)
varnames<-data.frame(row.names(infovalue))
infovalue$varnames<-data.frame(row.names(infovalue))
sortedinfogain2<-infovalue[order(infovalue$attr_importance,decreasing=TRUE),c(1,2)]
sortedinfogain2<-subset(sortedinfogain2,select=c(attr_importance))
sortedinfogain2 # once again- num_cases, country of residence, origin, customer for days among top info gain.
## For Continuous ##

# we have variables with different ranges so its important to standardise for clustering!

#investigate std of vars
newrefined_num_per=select_if(newrefineddata, is.numeric)
string_vector <- names(newrefined_num_per)
standdev<-sapply(newrefined_num_per[string_vector], sd)
range(standdev) #really wide range, standardization maybe required!

# Scale all variables before clustering by Euclidean dist - This is because  if one of the variables has a much larger variance than the others it will dominate the distance measure 
newrefined_num_per=select_if(newrefineddata, is.numeric)
newrefined_num_per=scale(newrefined_num_per) %>% data.frame()

########################################
#### Step 5 - Bottom Up Clustering #####
########################################

# Initial idea of how many clusters to choose (carlos ran on his computer and gave me image to load and result says 2 clusters)
set.seed(123); smpl_newrefined_num_per=newrefined_num_per[sample(1:nrow(newrefined_num_per),10000),]
set.seed(123); clest = Clest(as.matrix(smpl_newrefined_num_per), maxK = 5, alpha = 0.1, B = 15, B0 = 5, nstart = 1000)
#clest suggest k=2 as optimal by Classification error rate(CER)
cluster_df=newrefined_num_per
corbigdata<-data.frame(cor(newrefined_num_per));corbigdata
#very high correlations between TXN_TYPE,SCOPE,INSTRUMENT REGION (the 4 labelable clustering variables!)


# Spare K-means - tuning paramter 2.528244 #
#set.seed(123);kperm=KMeansSparseCluster.permute(newrefined_num_per, K=2, nperms = 5)#gets optimum tuning param L1
#km_cl2_big = KMeansSparseCluster(newrefined_num_per, K = 2, wbounds = 2.528244)
#km_cl2_big[[1]]$ws
#fviz_cluster(list(data = newrefined_num_per, cluster = km_cl2_big[[1]]$Cs ),stand = FALSE, geom = "point",
             #frame.type = "norm")
#cluster_df$labels_skm_k2=km_cl2_big[[1]]$Cs
#ggplot(cluster_df, aes(x = as.factor(cluster_df$labels_skm_k2), fill = newrefineddata$SAR)) + 
#geom_bar(position = "fill") + theme(axis.text.x = element_text(angle = 90))
#km_cl2_big
#table_km_k2 = table(newrefineddata$SAR,cluster_df$labels_skm_k2) %>%
# prop.table(2)
#cluster_vars = data.frame(unlist(attributes(km_cl2_big[[1]]$ws)),km_cl2_big[[1]]$ws) %>% 
# filter(km_cl2_big[[1]]$ws >0)
#colnames(cluster_vars)=c('Feature','Importance')

# RS Kmeans #
# I did Clest with carlos and found 2 clusters was best!
set.seed(123); rskm_cl2_big = RSKC(d = newrefined_num_per, ncl = 2, alpha = 0.1, L1 = 2.528244)
fviz_cluster(list(data = newrefined_num_per, cluster = rskm_cl2_big$labels ),stand = FALSE, geom = "point",
             frame.type = "norm")
newrefineddata$labels_rskm_k2=rskm_cl2_big$labels
ggplot(cluster_df, aes(x = as.factor(cluster_df$labels_rskm_k2), fill = newrefineddata$SAR)) + 
  geom_bar(position = "fill") + theme(axis.text.x = element_text(angle = 90))
cluster_vars2 = data.frame(unlist(attributes(rskm_cl2_big$weights)),rskm_cl2_big$weights) %>%
  filter(rskm_cl2_big$weights >0) %>% arrange(desc(rskm_cl2_big.weights))
#26 clustering vars - a lot (no noise vars)
colnames(cluster_vars2)=c('Feature','Importance')
table_rskm_k2 = table(newrefineddata$SAR,cluster_df$labels_rskm_k2) %>%
  prop.table(2)

## Evaluation of Clusters - Numeric ##
diss <- as.matrix(dist(newrefined_num_per, method= "euclidean"))
# minowski distance computes a dissimilarity matrix with distance measure taking into acc that we only are using 7 important variables

# Sparse K Means
#sil<-silhouette(km_cl2_big[[1]]$Cs,diss)
#silcoef<-mean(sil[,3])#0.2565338

#dunspm<-dunn(diss, km_cl2_big[[1]]$Cs, Data = newrefined_num_per, method = "euclidean")

# Robust Sparse K Means
library(clValid)
sil<-silhouette(rskm_cl2_big$labels,diss)
silcoef<-mean(sil[,3])#0.2565338
#I think rskm is same as sparse - must be not so many outliers for 2 clusters

# Clara
#sil<-silhouette(cl2_big)
#silcoef<-mean(sil[,3])#0.1774191

## Evaluation of Clusters - Interpretation ##

# Most Relevant Vars for Clustering CLARA - 
newrefineddataoriginal$cl2_clara = cl2_big$clustering %>% factor()
cluster_vars2
stacked_gg(newrefineddataoriginal,newrefineddataoriginal$cl2_clara,newrefineddataoriginal$SAR)
stacked_gg(newrefineddataoriginal,newrefineddataoriginal$cl2_clara,newrefineddataoriginal$INSTRUMENT)
stacked_gg(newrefineddataoriginal,newrefineddataoriginal$cl2_clara,newrefineddataoriginal$SCOPE)
stacked_gg(newrefineddataoriginal,newrefineddataoriginal$cl2_clara,newrefineddataoriginal$TXN_TYPE)
stacked_gg(newrefineddataoriginal,newrefineddataoriginal$cl2_clara,newrefineddataoriginal$REGION)
stacked_gg(newrefineddataoriginal,newrefineddataoriginal$cl2_clara,newrefineddataoriginal$TIME_PERIOD)
stacked_gg(newrefineddataoriginal,newrefineddataoriginal$cl2_clara,newrefineddataoriginal$BASE_CURRENCY_AMOUNT)
#these are the top 6 vars to label with but all 26 vars were used in RSKM algorithm

# Most Relevant Vars for Clustering SKM - 
newrefineddataoriginal$km_k2 = km_cl2_big[[1]]$Cs %>% factor()
stacked_gg(newrefineddataoriginal,newrefineddataoriginal$km_k2,newrefineddataoriginal$SAR)
stacked_gg(newrefineddataoriginal,newrefineddataoriginal$km_k2,newrefineddataoriginal$TXN_TYPE)
stacked_gg(newrefineddataoriginal,newrefineddataoriginal$km_k2,newrefineddataoriginal$SCOPE)
stacked_gg(newrefineddataoriginal,newrefineddataoriginal$km_k2,newrefineddataoriginal$INSTRUMENT)
stacked_gg(newrefineddataoriginal,newrefineddataoriginal$km_k2,newrefineddataoriginal$REGION)
stacked_gg(newrefineddataoriginal,newrefineddataoriginal$km_k2,newrefineddataoriginal$TIME_PERIOD)
stacked_gg(newrefineddataoriginal,newrefineddataoriginal$km_k2,newrefineddataoriginal$BASE_CURRENCY_AMOUNT)

# Conclusion: I see RSKM (and also SKM) clearly wins on ease of interpretation and also silhouette coeff




# Most Relevant Vars for Clustering RSKM -
cluster_vars2
newrefineddata$rskm_k2 = rskm_cl2_big$labels #for when i want to split up dataset
newrefineddataoriginal$rskm_k2 = rskm_cl2_big$labels %>% factor()
stacked_gg(newrefineddataoriginal,newrefineddataoriginal$SAR,newrefineddataoriginal$rskm_k2)
stacked_gg(newrefineddataoriginal,newrefineddataoriginal$INSTRUMENT,newrefineddataoriginal$rskm_k2)#perfect
# if Wire then cluster 1
stacked_gg(newrefineddataoriginal,newrefineddataoriginal$SCOPE,newrefineddataoriginal$rskm_k2)#easy
# If Client or External then 1
stacked_gg(newrefineddataoriginal,newrefineddataoriginal$TXN_TYPE,newrefineddataoriginal$rskm_k2)#perfect
# If kp or not a single transaction then 2 
stacked_gg(newrefineddataoriginal,newrefineddataoriginal$REGION,newrefineddataoriginal$rskm_k2)#easy
# If D or I, then 1

stacked_gg(newrefineddataoriginal,newrefineddataoriginal$TIME_PERIOD,newrefineddataoriginal$rskm_k2)#easy
# If For each transaction record, then 1

stacked_gg(newrefineddataoriginal,newrefineddataoriginal$BASE_CURRENCY_AMOUNT,newrefineddataoriginal$rskm_k2)#okay
# If not a single transaction, then 1

##### Until here Variable importance > 10%. 6 variables should be effective enough to NAify 2 clusters
# Hence we profile based on above variables first and see if they are sufficient already
# Other Possible Vars for Clustering
stacked_gg(newrefineddataoriginal,newrefineddataoriginal$rskm_k2,newrefineddataoriginal$SCENARIO)
#unsure
stacked_gg(newrefineddataoriginal,newrefineddataoriginal$rskm_k2,newrefineddataoriginal$CREDIT_DEBIT_CODE)
#If Not a single transaction, then 2
stacked_gg(newrefineddataoriginal,newrefineddataoriginal$rskm_k2,newrefineddataoriginal$RAISED_ON)
# not good
stacked_gg(newrefineddataoriginal,newrefineddataoriginal$rskm_k2,newrefineddataoriginal$STATUS)
#not labelable
stacked_gg(newrefineddataoriginal,newrefineddataoriginal$rskm_k2,newrefineddataoriginal$CUSTOMER_SEGMENT)
# not labelable
stacked_gg(newrefineddataoriginal,newrefineddataoriginal$rskm_k2,newrefineddataoriginal$ORIGINAL_ACCOUNT_CURRENCY)
# not labelable
density_gg(newrefineddataoriginal,newrefineddataoriginal$rskm_k2,newrefineddataoriginal$CUSTOMER_FOR_DAYS)
# not labelable
stacked_gg(newrefineddataoriginal,newrefineddataoriginal$rskm_k2,newrefineddataoriginal$EVENT_MONTH)
# not labelable
density_gg(newrefineddataoriginal,newrefineddataoriginal$rskm_k2,newrefineddataoriginal$NUM_ACCOUNTS)
# not labelable
density_gg(newrefineddataoriginal,newrefineddataoriginal$rskm_k2,newrefineddataoriginal$NUM_CASES)
# not labelable
stacked_gg(newrefineddataoriginal,newrefineddataoriginal$rskm_k2,newrefineddataoriginal$BUSINESS_TYPE2)
# not labelable
stacked_gg(newrefineddataoriginal,newrefineddataoriginal$rskm_k2,newrefineddataoriginal$Customer_Equals_Account)
# not labelable 
stacked_gg(newrefineddataoriginal,newrefineddataoriginal$rskm_k2,newrefineddataoriginal$CUSTOMER_STATUS)
# sort of labelable
stacked_gg(newrefineddataoriginal,newrefineddataoriginal$rskm_k2,newrefineddataoriginal$COUNTRY_OF_ORIGIN)
# not labelable
stacked_gg(newrefineddataoriginal,newrefineddataoriginal$rskm_k2,newrefineddataoriginal$CUSTOMER_REGION)
# not labelable
stacked_gg(newrefineddataoriginal,newrefineddataoriginal$rskm_k2,newrefineddataoriginal$COUNTRY_OF_RESIDENCE_abbrev)
# not labelable
stacked_gg(newrefineddataoriginal,newrefineddataoriginal$rskm_k2,newrefineddataoriginal$DISTRICT_OF_BUDAPEST)
# not labelable
density_gg(newrefineddataoriginal,newrefineddataoriginal$rskm_k2,newrefineddataoriginal$NEW_ACCOUNT_BALANCE)
# not labelable
stacked_gg(newrefineddataoriginal,newrefineddataoriginal$rskm_k2,newrefineddataoriginal$SPECIAL_ATTENTION_FLAG)
#sort of labelable
stacked_gg(newrefineddataoriginal,newrefineddataoriginal$rskm_k2,newrefineddataoriginal$SC01_Amount_Exceding_250000__HUF_)
#good for labelling
#If exceeding25k = 1, then cluster 2

#could be an issue as these vars are all used but lots are difficult to label!

# Add EVENT_DATE as column in dataframe so we can sort it based on this later
newrefineddata$EVENT_DATE <- newrefineddataoriginal$EVENT_DATE

# Therefore variables for clustering are: 

# Creating different datasets for each cluster


# variables in the algorithm in ascending order of importance so that splits suggested
# by variables of higher importance replace the previous splits by variables of lower importance 
### Bharat's Algorithm ####
newrefineddataoriginal$clusterprofile_label_big<- rep(3,nrow(newrefineddataoriginal))
for (i in 1:nrow(newrefineddataoriginal))
{ if(newrefineddataoriginal[i,]$SC01_Amount_Exceding_250000__HUF_==1)
{newrefineddataoriginal[i,]$clusterprofile_label_big=2
}else if(newrefineddataoriginal[i,]$CREDIT_DEBIT_CODE== "NOT_A_SINGLE_TRANSACTION")
{newrefineddataoriginal[i,]$clusterprofile_label_big=2
}else if(newrefineddataoriginal[i,]$BASE_CURRENCY_AMOUNT!= "NOT_A_SINGLE_TRANSACTION")
{newrefineddataoriginal[i,]$clusterprofile_label_big=1
}else if(newrefineddataoriginal[i,]$TIME_PERIOD== "For each Transaction Record") 
{newrefineddataoriginal[i,]$clusterprofile_label_big=1
}else if(newrefineddataoriginal[i,]$REGION== "D" | newrefineddataoriginal[i,]$REGION=="I")
{newrefineddataoriginal[i,]$clusterprofile_label_big=1
}else if(newrefineddataoriginal[i,]$TXN_TYPE== "NOT_A_SINGLE_TRANSACTION"|newrefineddataoriginal[i,]$TXN_TYPE== "kp")
{newrefineddataoriginal[i,]$clusterprofile_label_big=2
}else if(newrefineddataoriginal[i,]$SCOPE=="CLIENT" | newrefineddataoriginal[i,]$SCOPE=="EXTERNAL")
{newrefineddataoriginal[i,]$clusterprofile_label_big=1  
}else if(newrefineddataoriginaloriginal[i,]$INSTRUMENT=="WIRE")
{newrefineddataoriginal[i,]$clusterprofile_label_big=1
}else {newrefineddataoriginal[i,]$clusterprofile_label_big=3}
  print(newrefineddataoriginal[i,]$clusterprofile_label_big)
}


newrefineddata$clusterprofile_label_big<-newrefineddataoriginal$clusterprofile_label_big

table(newrefineddata$clusterprofile_label_big)
table(newrefineddata$rskm_k2)
exceptions<-newrefineddata[newrefineddata$clusterprofile_label_big!=newrefineddata$rskm_k2,]

######### Step 6  Model Building #############
####Creating different datasets for each cluster ########
cluster1_big = newrefineddata %>% filter(labels_rskm_k2 == 1) %>%
  select(SAR, NUM_CASES, NUM_ACCOUNTS, NEW_ACCOUNT_BALANCE, CUSTOMER_FOR_DAYS, contains('RS_'))
# Wire Transactions

cluster2_big = newrefineddata %>% filter(labels_rskm_k2 == 2) %>%
  select(SAR, NUM_CASES, NUM_ACCOUNTS, NEW_ACCOUNT_BALANCE, CUSTOMER_FOR_DAYS, contains('RS_'))


####################################################################
#### Step 6 - Data Split, NA Imbalance and Feature Selection ####
####################################################################



## Getting the training and test set ##

set.seed(123); cl1_index = createDataPartition(cluster1_big$SAR,p=.70, list = FALSE)
c1_rskm_data_train = cluster1_big[unlist(cl1_index),]
c1_rskm_data_test = cluster1_big[-unlist(cl1_index),]

## NA imbalance issue ##

## 4b(2) Using subgroup.discovery

library(subgroup.discovery) #this doesn't suffice, load the package as functions in global environment
names(c1_rskm_data_train)
table(c1_rskm_data_train$SAR)

c1_rskm_data_train_sd<- c1_rskm_data_train
c1_rskm_data_train_sd$SAR<- as.numeric(c1_rskm_data_train_sd$SAR)


#run function prim.peel separately
names(c1_rskm_data_train_sd)
table(c1_rskm_data_train_sd$SAR)
peel_c1 <- prim.peel(X=c1_rskm_data_train_sd[,-1],y=c1_rskm_data_train_sd$SAR,N=nrow(c1_rskm_data_train_sd),peeling.quantile = 0.05,max.peel=0.1,min.support = 0.1,quality.function = 'mean')

peel_c1$supports
peel_c1$box.qualities #each box cover around 10% of the SAR's in the whole small data

#get rules
peel_c1<-as.data.frame(cbind(as.character(peel_c1$rule.names),noquote(as.character(peel_c1$rule.operators)),peel_c1$rule.values))
rules_c1<-do.call(paste, c(as.list(peel_c1), sep = ""))
rules_c1
rules_c1_ridit<- paste0('c1_rskm_data_train_sd$',rules_c1) #add dataframe reference as prefix
rules_c1_ridit#rules of SD for higher SAR level defined for our dataset

#mrefined_small_riditnew<-mrefined_small_ridit
#for (i in 1:length(rules_small_ridit)){
#mrefined_small_riditnew<- mutate(mrefined_small_riditnew, eval(parse(match(rules_small[i],rules_small))) = ifelse(eval(parse(text=paste(rules_small[i]))),1,0))}

#featurize these rules as subgroup features
for (i in 1:length(rules_c1_ridit)){
  assign(paste("subgroup",i,sep="_"),ifelse(eval(parse(text=paste(rules_c1_ridit[i]))),1,0))  
} #creates 60 new vectors which can be used as features




paste((sprintf("subgroup_%s",seq(1:52))),collapse = ',')#copy paste this list of subgroup features to bind with dataframe

#join these feature vectors into the dataframe!
c1_rskm_data_train<- cbind(c1_rskm_data_train,subgroup_1,subgroup_2,subgroup_3,subgroup_4,subgroup_5,subgroup_6,subgroup_7,subgroup_8,subgroup_9,subgroup_10,subgroup_11,subgroup_12,subgroup_13,subgroup_14,subgroup_15,subgroup_16,subgroup_17,subgroup_18,subgroup_19,subgroup_20,subgroup_21,subgroup_22,subgroup_23,subgroup_24,subgroup_25,subgroup_26,subgroup_27,subgroup_28,subgroup_29,subgroup_30,subgroup_31,subgroup_32,subgroup_33,subgroup_34,subgroup_35,subgroup_36,subgroup_37,subgroup_38,subgroup_39,subgroup_40,subgroup_41,subgroup_42,subgroup_43,subgroup_44,subgroup_45,subgroup_46,subgroup_47,subgroup_48,subgroup_49,subgroup_50,subgroup_51,subgroup_52)


names(c1_rskm_data_train)


#Anomaly detection using Isolation forest for each cluster to identify grouped anomalies ###
#Isolation Forest 
library(IsolationForest)
iforest = IsolationForest::IsolationTrees(c1_rskm_data_train,rFactor=1,nRowSamp =256,ntree = 100) 
# sub sampling size of 256 and ntree=10 recommended by paper from iforest
# reference: https://cs.nju.edu.cn/zhouzh/zhouzh.files/publication/icdm08b.pdf

c1_rskm_data_train$anomaly_scorec1 = AnomalyScore(c1_rskm_data_train,iforest)$outF #score !!!

names(c1_rskm_data_train)

## Feature Reduction C1 - Boruta ####

# Note: Boruta is affected a lot by class imbalance issue so we need to account for it before Boruta! #


# Boruta: Graph - SMOTE
set.seed(123); c1_boruta = Boruta(c1_rskm_data_train$SAR~., data = c1_rskm_data_train, doTrace = 2)
print(c1_boruta)
plot(c1_boruta, xlab = "", xaxt = "n")
lz=lapply(1:ncol(c1_boruta$ImpHistory),function(i)
  c1_boruta$ImpHistory[is.finite(c1_boruta$ImpHistory[,i]),i])
names(lz) = colnames(c1_boruta$ImpHistory)
Labels = sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(c1_boruta$ImpHistory), cex.axis = 0.7)
c1_boruta_formula=getConfirmedFormula(c1_boruta)
# The ranked list of most relevant features
c1_featlist = attStats(c1_boruta) %>%
  rownames_to_column(var = 'Features')
c1featlist_ordered = attStats(c1_boruta) %>%
  rownames_to_column(var = 'Features') %>% arrange(desc(medianImp))  %>% select(c('Features','medianImp','decision'))

topfeaturesc1<-c1featlist_ordered[c1featlist_ordered$decision =='Confirmed',1] # just select confirmed features
topfeaturesc1
SAR<-c1_rskm_data_train$SAR
c1_rskm_data_trainold<-subset(c1_rskm_data_train,select=topfeaturesc1)
c1_rskm_data_train_boruta<-cbind(c1_rskm_data_trainold,SAR)
### Class Imbalance C1 - SMOTE & Down Samp ####
# Downsample - Not advised, sample reduced to around 100.
# SMOTE - balances clsmotedatac1.standasses and also doesnt take that much time to run boruta and also not so many observations thrown away
set.seed(123); smotedatac1 = SMOTE(SAR ~ ., c1_rskm_data_train, perc.over = 400, perc.under=125)
table(smotedatac1$SAR)



## Getting the training and test set ##

set.seed(123); cl2_index = createDataPartition(cluster2_big$SAR,p=.70)
c2_rskm_data_train = cluster2_big[unlist(cl2_index),]
c2_rskm_data_test = cluster2_big[-unlist(cl2_index),]

## 4b(2) Using subgroup.discovery

library(subgroup.discovery) #this doesn't suffice, load the package as functions in global environment
names(c2_rskm_data_train)
table(c2_rskm_data_train$SAR)

c2_rskm_data_train_sd<- c2_rskm_data_train
c2_rskm_data_train_sd$SAR<- as.numeric(c2_rskm_data_train_sd$SAR)


#run function prim.peel separately
names(c2_rskm_data_train_sd)
table(c2_rskm_data_train_sd$SAR)
peel_c2 <- prim.peel(X=c2_rskm_data_train_sd[,-1],y=c2_rskm_data_train_sd$SAR,N=nrow(c2_rskm_data_train_sd),peeling.quantile = 0.05,max.peel=0.1,min.support = 0.1,quality.function = 'mean')

peel_c2$supports
peel_c2$box.qualities #each box cover around 10% of the SAR's in the whole small data

#get rules
peel_c2<-as.data.frame(cbind(as.character(peel_c2$rule.names),noquote(as.character(peel_c2$rule.operators)),peel_c2$rule.values))
rules_c2<-do.call(paste, c(as.list(peel_c2), sep = ""))
rules_c2
rules_c2_ridit<- paste0('c2_rskm_data_train_sd$',rules_c2) #add dataframe reference as prefix
rules_c2_ridit#rules of SD for higher SAR level defined for our dataset

#mrefined_small_riditnew<-mrefined_small_ridit
#for (i in 1:length(rules_small_ridit)){
#mrefined_small_riditnew<- mutate(mrefined_small_riditnew, eval(parse(match(rules_small[i],rules_small))) = ifelse(eval(parse(text=paste(rules_small[i]))),1,0))}

#featurize these rules as subgroup features
for (i in 1:length(rules_c2_ridit)){
  assign(paste("subgroup",i,sep="_"),ifelse(eval(parse(text=paste(rules_c2_ridit[i]))),1,0))  
} #creates 60 new vectors which can be used as features




paste((sprintf("subgroup_%s",seq(1:52))),collapse = ',')#copy paste this list of subgroup features to bind with dataframe

#join these feature vectors into the dataframe!
c2_rskm_data_train<- cbind(c2_rskm_data_train,subgroup_1,subgroup_2,subgroup_3,subgroup_4,subgroup_5,subgroup_6,subgroup_7,subgroup_8,subgroup_9,subgroup_10,subgroup_11,subgroup_12,subgroup_13,subgroup_14,subgroup_15,subgroup_16,subgroup_17,subgroup_18,subgroup_19,subgroup_20,subgroup_21,subgroup_22,subgroup_23,subgroup_24,subgroup_25,subgroup_26,subgroup_27,subgroup_28,subgroup_29,subgroup_30,subgroup_31,subgroup_32,subgroup_33,subgroup_34,subgroup_35,subgroup_36,subgroup_37,subgroup_38,subgroup_39,subgroup_40,subgroup_41,subgroup_42,subgroup_43,subgroup_44,subgroup_45,subgroup_46,subgroup_47,subgroup_48,subgroup_49,subgroup_50,subgroup_51,subgroup_52)


names(c2_rskm_data_train)


#Anomaly detection using Isolation forest for each cluster to identify grouped anomalies ###
#Isolation Forest 
library(IsolationForest)
iforest = IsolationForest::IsolationTrees(c2_rskm_data_train,rFactor=1,nRowSamp =256,ntree = 100) 
# sub sampling size of 256 and ntree=10 recommended by paper from iforest
# reference: https://cs.nju.edu.cn/zhouzh/zhouzh.files/publication/icdm08b.pdf

c2_rskm_data_train$anomaly_scorec2 = AnomalyScore(c2_rskm_data_train,iforest)$outF #score !!!

names(c2_rskm_data_train)

## Feature Reduction c2 - Boruta ####

# Note: Boruta is affected a lot by class imbalance issue so we need to account for it before Boruta! #


# Boruta: Graph - SMOTE
set.seed(123); c2_boruta = Boruta(c2_rskm_data_train$SAR~., data = c2_rskm_data_train, doTrace = 2)
print(c2_boruta)
plot(c2_boruta, xlab = "", xaxt = "n")
lz=lapply(1:ncol(c2_boruta$ImpHistory),function(i)
  c2_boruta$ImpHistory[is.finite(c2_boruta$ImpHistory[,i]),i])
names(lz) = colnames(c2_boruta$ImpHistory)
Labels = sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(c2_boruta$ImpHistory), cex.axis = 0.7)
c2_boruta_formula=getConfirmedFormula(c2_boruta)
# The ranked list of most relevant features
c2_featlist = attStats(c2_boruta) %>%
  rownames_to_column(var = 'Features')
c2featlist_ordered = attStats(c2_boruta) %>%
  rownames_to_column(var = 'Features') %>% arrange(desc(medianImp))  %>% select(c('Features','medianImp','decision'))

topfeaturesc2<-c2featlist_ordered[c2featlist_ordered$decision =='Confirmed',1] # just select confirmed features
topfeaturesc2
SAR<-c2_rskm_data_train$SAR
c2_rskm_data_trainold<-subset(c2_rskm_data_train,select=topfeaturesc2)
c2_rskm_data_train_boruta<-cbind(c2_rskm_data_trainold,SAR)
### Class Imbalance c2 - SMOTE & Down Samp ####
# Downsample - Not advised, sample reduced to around 100.
# SMOTE - balances clsmotedatac2.standasses and also doesnt take that much time to run boruta and also not so many observations thrown away
set.seed(123); smotedatac2 = SMOTE(SAR ~ ., c2_rskm_data_train, perc.over = 400, perc.under=125)
table(smotedatac2$SAR)

#################################
#### Step 7 - Model Building ####
#################################

## Logistic Regression - Cluster 1 ####

#need to do this first to get syntactically correct level names for my factor variables (only SAR)
feature.names=names(smotedatac1)

for (f in feature.names) {
  if (class(smotedatac1[[f]])=="factor") {
    levels <- unique(c(smotedatac1[[f]]))
    smotedatac1[[f]] <- factor(smotedatac1[[f]],
                               labels=make.names(levels))
  }
}

feature.names=names(smotedatac2)

for (f in feature.names) {
  if (class(smotedatac2[[f]])=="factor") {
    levels <- unique(c(smotedatac2[[f]]))
    smotedatac2[[f]] <- factor(smotedatac2[[f]],
                               labels=make.names(levels))
  }
}


## Brief Data Exploration ##
# Is scale imprtant? Not for logistic apparently - 
#https://stats.stackexchange.com/questions/48360/is-standardization-needed-before-fitting-logistic-regression
summary(smotedatac1)
sd(smotedatac1$NUM_CASES)
sd(smotedatac1$CUSTOMER_FOR_DAYS)#SD is a lot larger
sd(smotedatac1$NUM_ACCOUNTS)
sd(smotedatac1$RS_SCENARIO)



# All subsets regression
allsubset_df_c1 = cbind(smotedatac1[c1featlist_ordered$Features[1:10]],smotedatac1$SAR)

subset_c1 = bestglm(Xy = allsubset_df_c1,
          family = binomial,
          IC = "AIC",                
          method = "exhaustive")

subset_c1$BestModels

# We specify the type of evaluation we want to do; 10 fold cross validation,saves probs and preds predicted by 10 models
train_control <- trainControl(method="cv",number=10,savePredictions=TRUE,summaryFunction=twoClassSummary,
                              classProbs=TRUE)

b = paste(c1featlist_ordered$Features[1:10], collapse = ' + '); formula_b = paste('SAR ~ ',b,sep = '');b

# Models proposed; Model 1 & 2 come from the allsubset reg , Model 3 comes from Stepwise AIC. Model 1 & 2 performs similar.
set.seed(123); model1<-train(SAR ~ NUM_CASES + CUSTOMER_FOR_DAYS  + subgroup_50+ NEW_ACCOUNT_BALANCE  + RS_TXN_TYPE + RS_DISTRICT_OF_BUDAPEST + RS_EVENT_MONTH + RS_CREDIT_DEBIT_CODE, data = smotedatac1, method = "glm", family="binomial",
                            trControl=train_control,metric='ROC')

set.seed(123); model2 = train(SAR~ NUM_CASES + CUSTOMER_FOR_DAYS + RS_EVENT_MONTH + RS_REGION + 
                              RS_DISTRICT_OF_BUDAPEST + RS_SCENARIO + NUM_ACCOUNTS + NEW_ACCOUNT_BALANCE + 
                              RS_CREDIT_DEBIT_CODE + RS_TXN_TYPE, data = smotedatac1, method = "glm", family="binomial",
                              trControl=train_control,metric='ROC')

set.seed(123); model3 = train(SAR~ CUSTOMER_FOR_DAYS + NUM_CASES + RS_DISTRICT_OF_BUDAPEST + RS_EVENT_MONTH + 
                              NEW_ACCOUNT_BALANCE + RS_BUSINESS_TYPE2, data = smotedatac1, method = "glm", family="binomial",
                              trControl=train_control,metric='ROC')

model1 #0.91 
summary(model1)
summary(model2)
summary(model3)

#Robbie: no separation because coeffs and std errors are all okay!

#Robbie : tried different link functions but very little difference in AIC, BIC and AUC so for interpretability reasons just deal with logit link

## Some Comments: ##
## 4. If want to know what models avail in caret use names(getModelInfo())
## 5. So the model that is chosen is the one with mtry=5 when we set our seed to 123, this gives most accurate results in terms of accuracy
## 6. For us we want AUC
## 8. AUC using 10fold cross validation and mtry=4 and AUC is 97% for cluster 1

# Evaluation

# Calculate AUC and rank
AUC_logistic<-cbind(model1$results[2],model2$results[2],model3$results[2])
names(AUC_logistic)<-cbind("model1","model2","model3")
AUC_logistic<-sort(AUC_logistic,decreasing=TRUE);AUC_logistic
#Robbie: Best Model in terms of performance and parsimony has AUC  = 89.6% ####
#Robbie (paraphrasis): Including interactions might be worthy but Ridge is in place given high SE.

## Decision Tree - Cluster 1 ####

#train model
set.seed(123); dec_tree1 = train(SAR ~ NUM_CASES + CUSTOMER_FOR_DAYS  +  NEW_ACCOUNT_BALANCE + RS_TXN_TYPE + RS_DISTRICT_OF_BUDAPEST + RS_EVENT_MONTH + RS_CREDIT_DEBIT_CODE,data=smotedatac1,method='rpart',trControl=train_control,tuneLength = 10,
                                metric='ROC');dec_tree1 # 98.13% AUC is best cp=0 ####
#0.97 AUC

library(rattle)
#fancyRpartPlot(C,uniform=TRUE, main="Best Tree")
# http://rstatistics.net/decision-trees-with-r/ - see this for info
# note that this lower cp tree actually is the same as dectree1 so just continue with this from now on


#interpretting
dec_tree1$finalModel
#Splits: 1) Num_cases<0.5, Scenarios, New account balance > 9527705, RS_eventmonth -ve, Customerfordays>3205,....
table(cluster1_big$RS_EVENT_MONTH)
varimp1 = varImp(dec_tree1);varimp1 #Num_cases, Region, Scenario, District, Customer for Days, Event Month

# Cond tree ####
library(party)
#train model

set.seed(123); cond_tree1 = train(SAR ~ NUM_CASES + CUSTOMER_FOR_DAYS   + NEW_ACCOUNT_BALANCE + RS_TXN_TYPE + RS_DISTRICT_OF_BUDAPEST + RS_EVENT_MONTH + RS_CREDIT_DEBIT_CODE,data=smotedatac1,method='ctree',trControl=train_control,tuneLength = 10,
                                 metric='ROC');cond_tree1 # 98.13% AUC is best cp=0 ####
#0.97 AUC
# 

## Random Forest - Cluster 1 ####

#now to see what params to tune for random forest
paramgrid<-expand.grid(mtry=c(2:10),splitrule = "gini"
                       ,min.node.size = 1) #10 is the number of vars we are using so can't be greater than this

#train model
set.seed(123); ran_forestc1 = train(SAR ~ ., data = smotedatac1, 
                                   method='ranger', trControl=train_control, tuneGrid=paramgrid, metric = 'ROC')
# here we can select only a few predictor vars if we want, 
# ...we specify we want to evaluate using 10 fold cross validation and tune the mtry param
ran_forestc1
# AUC is 98.91% when mtry = 9 ####


## XGBoost - Cluster 1 ####
xgb_grid_c1 = expand.grid(
  nrounds = 1000,
  eta = c(0.01,0.1,0.2),
  max_depth = c(4,6,8),
  gamma = c(0,0.5,1),
  colsample_bytree = 0.75,
  min_child_weight = c(0, 2, 5),
  subsample = 0.5
)
#where eta is learning rate, max_depth(of tree), gamma=


xgb_c1 = train(smotedatac1[,-1], smotedatac1$SAR, method = "xgbTree",
               trControl = train_control, tuneGrid = xgb_grid_c1)

xgb_c1$finalModel  #AUC 0.99 ####
# eta = "0.1", max_depth = "8", gamma = "1", colsample_bytree = "0.75", min_child_weight = "0", subsample = "0.5"

## Logistic Regression - Cluster 2 ##

b = paste(c2featlist_ordered$Features[1:10], collapse = ' + '); formula_b = paste('SAR ~ ',b,sep = '');b

# All subsets regression
allsubset_df_c2 = cbind(smotedatac2[c2featlist_ordered$Features[1:10]],smotedatac2$SAR)

subset_c2 = bestglm(Xy = allsubset_df_c2,
                    family = binomial,
                    IC = "AIC",                
                    method = "exhaustive")

subset_c2$BestModels

# Logistic Reg Models
set.seed(123); model1_c2<-train(SAR ~ NUM_CASES + anomaly_scorec2 + CUSTOMER_FOR_DAYS +  NUM_ACCOUNTS  + RS_TIME_PERIOD + subgroup_20 + subgroup_21 + RS_COUNTRY_OF_RESIDENCE_abbrev
                                , data = smotedatac2, method = "glm", 
                                family="binomial", trControl=train_control,metric='ROC')
#without Raised On

set.seed(123); model2_c2 = train(SAR ~ NUM_CASES + RS_SC01_Amount_Exceding_250000__HUF_ + 
                                 CUSTOMER_FOR_DAYS + RS_TIME_PERIOD + RS_ORIGINAL_ACCOUNT_CURRENCY + 
                                 NUM_ACCOUNTS + RS_DISTRICT_OF_BUDAPEST + RS_CUSTOMER_REGION +
                                 RS_RAISED_ON + NEW_ACCOUNT_BALANCE, data = smotedatac2, method = "glm", 
                                 family="binomial", trControl=train_control,metric='ROC')
#full model

set.seed(123); model3_c2 = train(SAR ~ RS_SC01_Amount_Exceding_250000__HUF_ + 
                                 CUSTOMER_FOR_DAYS + RS_TIME_PERIOD + RS_ORIGINAL_ACCOUNT_CURRENCY + 
                                 NUM_ACCOUNTS + RS_DISTRICT_OF_BUDAPEST + RS_CUSTOMER_REGION +
                                 NEW_ACCOUNT_BALANCE, data = smotedatac2, method = "glm", 
                                 family="binomial", trControl=train_control,metric='ROC')
#without Num_cases and Raised on
model1_c2
summary(model1)
summary(model2)
summary(model3)


# Calculate AUC and rank
AUC_logistic_c2<-cbind(model1_c2$results[2],model2_c2$results[2],model3_c2$results[2])
names(AUC_logistic_c2)<-cbind("model1","model2","model3")
AUC_logistic_2c<-sort(AUC_logistic_c2,decreasing=TRUE);AUC_logistic_2c
# model 1 and 3 with AUC 84% ####


## Brief Data Exploration ##
# Is scale imprtant? Not for logistic apparently - https://stats.stackexchange.com/questions/48360/is-standardization-needed-before-fitting-logistic-regression
summary(smotedatac2)
sd(smotedatac2$NUM_CASES)
sd(smotedatac2$CUSTOMER_FOR_DAYS)#SD is a lot larger
sd(smotedatac2$NUM_ACCOUNTS)
sd(smotedatac2$RS_SCENARIO)
#Robbie : tree based models and c2gression - scale doesnt matter for contin vars


## Decision Tree - Cluster 2 ####

#train model
set.seed(123); dec_tree1_c2 = train(SAR ~ NUM_CASES + anomaly_scorec2 + CUSTOMER_FOR_DAYS +  NUM_ACCOUNTS  + RS_TIME_PERIOD + subgroup_20 + subgroup_21 + RS_COUNTRY_OF_RESIDENCE_abbrev,data=smotedatac2,method='rpart',trControl=train_control,
                                 tuneLength = 10, metric='ROC') # 0.9612096 AUC is best cp=0 ####
dec_tree1_c2

# http://rstatistics.net/decision-trees-with-r/ - see this for info
# note that this lower cp tree actually is the same as dectree1 so just continue with this from now on
# best dec tree has AUC=95.12% ####

#interpretting
varImp(dec_tree1_c2)
#Num_cases,Region,Scenario,District of Budapest,Customer for Days

# Cond tree
#train model
set.seed(123); cond_tree2 = train(SAR ~ NUM_CASES + anomaly_scorec2 + CUSTOMER_FOR_DAYS +  NUM_ACCOUNTS  + RS_TIME_PERIOD + subgroup_20 + subgroup_21 + RS_COUNTRY_OF_RESIDENCE_abbrev,data=smotedatac2,method='ctree',trControl=train_control,tuneLength = 10,
                                  metric='ROC');cond_tree1 # 98.13% AUC is best cp=0 ####
#0.98 AUC


## Random Forest - Cluster 2 ####

#train model
paramgrid<-expand.grid(mtry=c(2:10),splitrule = "gini"
                       ,min.node.size = 1)
set.seed(123); ran_forest_c2 = train(SAR ~ ., data = smotedatac2,method='ranger',
                                     trControl=train_control, tuneGrid=paramgrid, metric='ROC')

# here we can select only a few predictor vars if we want, we specify we want to evaluate using 10 fold cross validation and tune the mtry param
ran_forest_c2# AUC is 0.986% ####
## XGBoost - Cluster 2 ####

xgb_c2 = train(smotedatac2[,-1], smotedatac2$SAR, method = "xgbTree",
               trControl = train_control, tuneGrid = xgb_grid_c1)

xgb_c2 # xgboost 0.9859 ####
#The final values used for the model were nrounds = 1000, max_depth = 6, eta = 0.01, gamma = 0, colsample_bytree =
#0.75, min_child_weight = 2 and subsample = 0.5

# Conclusion - Random Forest is best model for cluster 1 and 2 so use it on test now

AUC_models_c1<-cbind(model1$results[2], dec_tree1$results[1,2], ran_forestc1$results[1,2], 
                     LMT_c1$results[2,2], logitboost_c1$results[6,2])
names(AUC_models_c1)<-cbind("logistic reg.",'decision tree',"random forest","logistic model tree", "logitboost")
AUC_models_c1 = sort(AUC_models_c1,decreasing=TRUE);AUC_models_c1


AUC_models_c2<-cbind(model1_c2$results[2], dec_tree1_c2$results[1,2], ran_forest_c2$results[7,2], 
                       LMT_c2$results[2,2], logitboost_c2$results[6,2])
names(AUC_models_c2) = cbind('logistic','decision tree',"random forest","logistic model tree", "logitboost")
AUC_models_c2 = sort(AUC_models_c2,decreasing=TRUE);AUC_models_c2





######################################
#### Step 8 - Evaluation: Scoring ####
######################################

# Idea: 1) So Matthias says we no longer are going to autoclose the alerts, just risk score!
#          So he wants us to sort all the alerts and say how much 
#          I goggled some things about risk scoring Random Forests and here is a link https://stats.stackexchange.com/questions/56895/do-the-predictions-of-a-random-forest-model-have-a-prediction-interval
#          
## Final Evaluation - Test Set ####


####Preprocessing test datasets ####

#Cluster 1 ####
## 4b(2) Applying subgroups to test dataset


rules_c1_ridit<- paste0('c1_rskm_data_test_sd$',rules_c1) #add dataframe reference as prefix
rules_c1_ridit#rules of SD for higher SAR level defined for our dataset

#mrefined_small_riditnew<-mrefined_small_ridit
#for (i in 1:length(rules_small_ridit)){
#mrefined_small_riditnew<- mutate(mrefined_small_riditnew, eval(parse(match(rules_small[i],rules_small))) = ifelse(eval(parse(text=paste(rules_small[i]))),1,0))}

#featurize these rules as subgroup features
for (i in 1:length(rules_c1_ridit)){
  assign(paste("subgroup",i,sep="_"),ifelse(eval(parse(text=paste(rules_c1_ridit[i]))),1,0))  
} #creates 60 new vectors which can be used as features




paste((sprintf("subgroup_%s",seq(1:52))),collapse = ',')#copy paste this list of subgroup features to bind with dataframe

#join these feature vectors into the dataframe!
c1_rskm_data_test<- cbind(c1_rskm_data_test,subgroup_1,subgroup_2,subgroup_3,subgroup_4,subgroup_5,subgroup_6,subgroup_7,subgroup_8,subgroup_9,subgroup_10,subgroup_11,subgroup_12,subgroup_13,subgroup_14,subgroup_15,subgroup_16,subgroup_17,subgroup_18,subgroup_19,subgroup_20,subgroup_21,subgroup_22,subgroup_23,subgroup_24,subgroup_25,subgroup_26,subgroup_27,subgroup_28,subgroup_29,subgroup_30,subgroup_31,subgroup_32,subgroup_33,subgroup_34,subgroup_35,subgroup_36,subgroup_37,subgroup_38,subgroup_39,subgroup_40,subgroup_41,subgroup_42,subgroup_43,subgroup_44,subgroup_45,subgroup_46,subgroup_47,subgroup_48,subgroup_49,subgroup_50,subgroup_51,subgroup_52)


names(c1_rskm_data_test)


#Anomaly detection using Isolation forest for each cluster to identify grouped anomalies ###
#Isolation Forest 
library(IsolationForest)
iforest = IsolationForest::IsolationTrees(c1_rskm_data_test,rFactor=1,nRowSamp =256,ntree = 100) 
# sub sampling size of 256 and ntree=10 recommended by paper from iforest
# reference: https://cs.nju.edu.cn/zhouzh/zhouzh.files/publication/icdm08b.pdf

c1_rskm_data_test$anomaly_scorec1 = AnomalyScore(c1_rskm_data_test,iforest)$outF #score !!!

names(c1_rskm_data_test)


##Cluster 2 ####
## 4b(2) Applying subgroups to test dataset


rules_c2_ridit<- paste0('c2_rskm_data_test_sd$',rules_c2) #add dataframe reference as prefix
rules_c2_ridit#rules of SD for higher SAR level defined for our dataset

#mrefined_small_riditnew<-mrefined_small_ridit
#for (i in 1:length(rules_small_ridit)){
#mrefined_small_riditnew<- mutate(mrefined_small_riditnew, eval(parse(match(rules_small[i],rules_small))) = ifelse(eval(parse(text=paste(rules_small[i]))),1,0))}

#featurize these rules as subgroup features
for (i in 1:length(rules_c2_ridit)){
  assign(paste("subgroup",i,sep="_"),ifelse(eval(parse(text=paste(rules_c2_ridit[i]))),1,0))  
} #creates 60 new vectors which can be used as features




paste((sprintf("subgroup_%s",seq(1:55))),collapse = ',')#copy paste this list of subgroup features to bind with dataframe

#join these feature vectors into the dataframe!
c2_rskm_data_test<- cbind(c2_rskm_data_test,subgroup_1,subgroup_2,subgroup_3,subgroup_4,subgroup_5,subgroup_6,subgroup_7,subgroup_8,subgroup_9,subgroup_10,subgroup_11,subgroup_12,subgroup_13,subgroup_14,subgroup_15,subgroup_16,subgroup_17,subgroup_18,subgroup_19,subgroup_20,subgroup_21,subgroup_22,subgroup_23,subgroup_24,subgroup_25,subgroup_26,subgroup_27,subgroup_28,subgroup_29,subgroup_30,subgroup_31,subgroup_32,subgroup_33,subgroup_34,subgroup_35,subgroup_36,subgroup_37,subgroup_38,subgroup_39,subgroup_40,subgroup_41,subgroup_42,subgroup_43,subgroup_44,subgroup_45,subgroup_46,subgroup_47,subgroup_48,subgroup_49,subgroup_50,subgroup_51,subgroup_52,subgroup_53,subgroup_54,subgroup_55)

names(c2_rskm_data_test)


#Anomaly detection using Isolation forest for each cluster to identify grouped anomalies ###
#Isolation Forest 
library(IsolationForest)
iforest = IsolationForest::IsolationTrees(c2_rskm_data_test,rFactor=1,nRowSamp =256,ntree = 100) 
# sub sampling size of 256 and ntree=10 recommended by paper from iforest
# reference: https://cs.nju.edu.cn/zhouzh/zhouzh.files/publication/icdm08b.pdf

c2_rskm_data_test$anomaly_scorec2 = AnomalyScore(c2_rskm_data_test,iforest)$outF #score !!!

names(c2_rskm_data_test)


## Final Evaluation - Test Set ####

# Cluster 1 ####
pred_logit_c1 = predict(model1, newdata=c1_rskm_data_test, type="prob")
pred_rf_c1 = predict(ran_forestc1, newdata=c1_rskm_data_test, type="prob")
pred_cond_c1 = predict(cond_tree1, newdata=c1_rskm_data_test, type="prob")
pred_dec_c1 = predict(dec_tree1, newdata=c1_rskm_data_test, type="prob")
pred_xgb_c1 = predict(xgb_c1 , newdata = c1_rskm_data_test, type = 'prob')

pred_c1 = cbind(pred_logit_c1[,2], pred_dec_c1[,2], pred_cond_c1[,2],pred_rf_c1[,2],pred_xgb_c1[,2])
pred_c1 = data.frame(pred_c1)
colnames(pred_c1) = cbind('logistic','decision_tree',"cond_tree","random_forest",'XGB')
colAUC(pred_c1,c1_rskm_data_test$SAR, plotROC = T)


# That's the frame for calculating PR Sumamry
Eval_Fun_c1 = function(pred_vector, df = c1_rskm_data_test){
  df_test = data.frame(obs = df$SAR, YES = pred_vector)
  df_test$obs = relevel(df_test$obs,'1')
  levels(df_test$obs) = c('YES','NO')
  df_test$NO = 1 - df_test$YES
  df_test$pred = factor(ifelse(df_test$YES >= .5, "YES", "NO"))
  df_test$obs = relevel(df_test$obs,'YES')
  df_test$pred = relevel(df_test$pred,'YES')
  PR = prSummary(df_test, lev = levels(df_test$obs))
  ROC = twoClassSummary(df_test, lev = levels(df_test$obs))
  return(c(PR,ROC))
}
apply(pred_c1,2,Eval_Fun_c1)

# Loop for creating the list of df 
list_lift_c1 = list()
for (i in names(pred_c1)){
  list_lift_c1[[i]] = data.frame(Class = as.numeric(as.character(c1_rskm_data_test$SAR)), data.frame(pred_c1), cum = 1, perpop = 1)
  list_lift_c1[[i]] = list_lift_c1[[i]][order(list_lift_c1[[i]][[i]], decreasing = T),]
  list_lift_c1[[i]][['cum']] = 100*cumsum(list_lift_c1[[i]][['Class']])/sum(list_lift_c1[[i]][['Class']])
  list_lift_c1[[i]][['perpop']] = (seq(nrow(list_lift_c1[[i]]))/nrow(list_lift_c1[[i]]))*100
}
# Ploting Skimming Plot
skimming_c1 = ggplot(data = NULL, aes(cum, perpop, color = Models)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line(data = list_lift_c1[['logistic']],  aes(color = 'Logistic')) +
  geom_line(data = list_lift_c1[['decision_tree']], aes(color = 'decision_tree')) +
  geom_line(data = list_lift_c1[['cond_tree']], aes(color = 'cond_tree')) +
  geom_line(data = list_lift_c1[['random_forest']], aes( color = 'RF')) +
  geom_line(data = list_lift_c1[['XGB']], aes(color = 'XGB')) +
  labs(title = "Skimming-the-cream Plot", x ="% of SARs", y="% of Alerts") +
  theme(legend.position = c(0.2,0.65),legend.direction = 'vertical') +
  scale_y_continuous(breaks=seq(0,60,20), expand = c(0,0)) +
  scale_x_continuous(breaks=seq(0,100,20)) +
  geom_vline(xintercept = 95, linetype = 'dotdash' ) +
  annotate("text", x = 98.5, y = 59, label = "95%", size = 4) +
  geom_vline(xintercept = 85, linetype = 'dotdash' ) +
  annotate("text", x = 88.5, y = 59, label = "85%", size = 4) +
  coord_cartesian(ylim = c(0, 60))

###Skim plot the ultimate result plot for cluster 1 big data####
skimming_c1



# Cluster 2
pred_logit_c2 = predict(model1_c2, newdata=c2_rskm_data_test, type="prob")
pred_rf_c2 = predict(ran_forest_c2, newdata=c2_rskm_data_test, type="prob")
pred_cond_c2 = predict(cond_tree2, newdata=c2_rskm_data_test, type="prob")
pred_dec_c2 = predict(dec_tree1_c2, newdata=c2_rskm_data_test, type="prob")

pred_xgb_c2 = predict(xgb_c2 , newdata = c2_rskm_data_test, type = 'prob')
pred_c2 = cbind(pred_logit_c2[,2], pred_dec_c2[,2], pred_cond_c2[,2], pred_rf_c2[,2],
                 pred_xgb_c2[,2])
colnames(pred_c2) = cbind('logistic','decision_tree','cond_tree',"random_forest",
                          'XGB')
colAUC(pred_c2,c2_rskm_data_test$SAR, plotROC = T)
pred_c2 = data.frame(pred_c2)

Eval_Fun_c2 = function(pred_vector, df = c2_rskm_data_test){
  df_test = data.frame(obs = df$SAR, YES = pred_vector)
  df_test$obs = relevel(df_test$obs,'1')
  levels(df_test$obs) = c('YES','NO')
  df_test$NO = 1 - df_test$YES
  df_test$pred = factor(ifelse(df_test$YES >= .5, "YES", "NO"))
  df_test$obs = relevel(df_test$obs,'YES')
  df_test$pred = relevel(df_test$pred,'YES')
  PR = prSummary(df_test, lev = levels(df_test$obs))
  ROC = twoClassSummary(df_test, lev = levels(df_test$obs))
  return(c(PR,ROC))
}
apply(pred_c2,2,Eval_Fun_c2)
# For Loop
list_lift_c2 = list()
for (i in names(pred_c2)){
  list_lift_c2[[i]] = data.frame(Class = as.numeric(as.character(c2_rskm_data_test$SAR)), data.frame(pred_c2), cum = 1, perpop = 1)
  list_lift_c2[[i]] = list_lift_c2[[i]][order(list_lift_c2[[i]][[i]], decreasing = T),]
  list_lift_c2[[i]][['cum']] = 100*cumsum(list_lift_c2[[i]][['Class']])/sum(list_lift_c2[[i]][['Class']])
  list_lift_c2[[i]][['perpop']] = (seq(nrow(list_lift_c2[[i]]))/nrow(list_lift_c2[[i]]))*100
}


# Ploting Skimming Plot
skimming_c2 = ggplot(data = NULL, aes(cum, perpop, color = Models)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line(data = list_lift_c2[['logistic']],  aes(color = 'Logistic')) +
  geom_line(data = list_lift_c2[['cond_tree']], aes(color = 'cond_tree')) +
  geom_line(data = list_lift_c2[['random_forest']], aes( color = 'RF')) +
  geom_line(data = list_lift_c2[['XGB']], aes(color = 'XGB')) +
  geom_line(data = list_lift_c2[['decision_tree']], aes(color = 'decision_tree')) +
  labs(title = "Skimming-the-cream Plot", x ="% of SARs", y="% of Transactions") +
  theme(legend.position = c(0.2,0.65),legend.direction = 'vertical') +
  scale_y_continuous(breaks=seq(0,60,20), expand = c(0,0)) +
  scale_x_continuous(breaks=seq(0,100,20)) +
  geom_vline(xintercept = 95, linetype = 'dotdash' ) +
  annotate("text", x = 98.5, y = 59, label = "95%", size = 4) +
  geom_vline(xintercept = 85, linetype = 'dotdash' ) +
  annotate("text", x = 88.5, y = 59, label = "85%", size = 4) +
  coord_cartesian(ylim = c(0, 75))

# Skim plot the ultimate result plot for cluster 2 big data#### 
skimming_c2
# decision tree not so good for cluster 2

############################
#### Subgroup Discovery ####
############################



