#### Small Data ####


## Libraries to load ####
library(Boruta)
library(MLmetrics)
library(xgboost)
library(xlsx)
library(boot)
library(DMwR)
require(ResourceSelection)
require(arm)
library(FSelector)
library(RSKC)
library(caTools)
library(bestglm)
library(caret)
library(rpart)
library(ROSE)
library(car)
library(ROCR)
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
library(clValid)
library(reshape2)
library(mice)
library(party)
library(tibble)
options(java.parameters = "-Xmx50000m")
if(!require(devtools)) install.packages("devtools")
library(factoextra)
library(fpc)

#Functions ####

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
# VIF
vif = function(df){diag(solve(cor(df)))}
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

#############################################################
## Step 3 - Feature Selection and Replacing Missing Values ####
##    Continue from small data part of other 2V2 Rfile     ##
#############################################################

#### Feature Selection ####
table(smalldata$REGION)
# Remove initial redundant variables
smalldata<-subset(smalldata,select=-c(CAL_QUARTER,RISK_SCORE_2))
#CAL_quarter (because it doesnt contain as much info as event_month)
#RISK_SCORE_2 is redundant and not reliable as lots of secondpartycountry observations missing

# A check to see are these variables equal - they are not
ggplot(smalldata, aes(x = PEP_FLAG, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
table(smalldata$PEP_FLAG) #3439

ggplot(smalldata, aes(x = SPECIAL_ATTENTION_FLAG, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))

table(smalldata$SPECIAL_ATTENTION_FLAG) #337



# Deal with 'missing for a reason' guys first!
  
# some variables' NA are actually informative, for example these actually below aren't missing information, they are indicating values..
smalldata$DISTRICT_OF_BUDAPEST[is.na(smalldata$DISTRICT_OF_BUDAPEST)] = "OUTSIDE_BUDAPEST"
smalldata$CREDIT_DEBIT_CODE[is.na(smalldata$CREDIT_DEBIT_CODE)] = "NOT_A_TRANSACTION"
smalldata$TXN_TYPE[is.na(smalldata$TXN_TYPE)] = "NOT_A_TRANSACTION"
smalldata$INSTRUMENT[is.na(smalldata$INSTRUMENT)] = "NOT_A_TRANSACTION"
smalldata$SCOPE[is.na(smalldata$SCOPE)] = "NOT_A_TRANSACTION"
smalldata$REGION[is.na(smalldata$REGION)] = "NOT_A_TRANSACTION"
smalldata$BASE_CURRENCY_AMOUNT[is.na(smalldata$BASE_CURRENCY_AMOUNT)] = "NOT_A_TRANSACTION"

# Because of so much missingness remove all variables with > 20% missingness
rankmissing = (sapply(smalldata,function(x) mean(is.na(x))) %>%
                 sort())
rankmissing

names_miss=names(rankmissing[which(rankmissing < 0.20)])
refined_small= subset(smalldata, select = names_miss)
names(refined_small)

# Also consider variables where missingness is meaningful
#refined_small<-subset(smalldata,select=c(SCENARIO,EVENT_MONTH,EVENT_DATE,RAISED_ON,
                                          #PEP_FLAG,SPECIAL_ATTENTION_FLAG,               
                                          #BUSINESS_TYPE2,CUSTOMER_TYPE,CUSTOMER_STATUS,                
                                          #NUM_CASES,NUM_ACCOUNTS,CUSTOMER_FOR_DAYS,CUSTOMER_SEGMENT,TIME_PERIOD,SC01_Amount_Exceding_250000__HUF_,
                                          #SAR,COUNTRY_OF_RESIDENCE_abbrev,CUSTOMER_REGION,NEW_ACCOUNT_BALANCE,ORIGINAL_ACCOUNT_CURRENCY,Customer_Equals_Account,          
                                          #STATUS,DISTRICT_OF_BUDAPEST,CREDIT_DEBIT_CODE,TXN_TYPE,SCOPE,INSTRUMENT,REGION,BASE_CURRENCY_AMOUNT))

#### Replacing Missing Values ####

#How to deal with BASE_CURRENCY_AMOUNT (missingness is for a reason but it is a continuous variable)
#Option 1: Impute missing vals and treat as continuous variable (but way too many imputations)
#Option 2: Delete missing value stuff (but way too many things to delete)
#Option 3: Make variable into categorical and have NA's as a category (need to decide on categories) - best option!!!
refined_small$BASE_CURRENCY_AMOUNT[which(refined_small$BASE_CURRENCY_AMOUNT=='NOT_A_TRANSACTION')]=NA
refined_small$BASE_CURRENCY_AMOUNT=as.numeric(refined_small$BASE_CURRENCY_AMOUNT)
unique(refined_small$BASE_CURRENCY_AMOUNT)
refined_small$BASE_CURRENCY_AMOUNT=addNA(refined_small$BASE_CURRENCY_AMOUNT)
sum(is.na(refined_small$BASE_CURRENCY_AMOUNT)) #No NA's, perfect!
class(refined_small$BASE_CURRENCY_AMOUNT) #converting NA's into Not a transaction
levels(refined_small$BASE_CURRENCY_AMOUNT)[is.na(levels(refined_small$BASE_CURRENCY_AMOUNT))] = "NOT_A_TRANSACTION" 
table(refined_small$BASE_CURRENCY_AMOUNT)
# Now deal with 'not missing for a reason' guys! - look for patterns then impute

mrefined_small=refined_small# so don't have to change names when copying carlos code

## COUNTRY_OF_RESIDENCE_abbrev ##
countryres<-mrefined_small[which(is.na(mrefined_small$COUNTRY_OF_RESIDENCE_abbrev)),] #1599 NA's - tough to see pattern
mrefined_small[which(is.na(mrefined_small$COUNTRY_OF_RESIDENCE_abbrev)),'COUNTRY_OF_RESIDENCE_abbrev']<- 'HU'

## impute NEW_ACCOUNT_BALANCE, STATUS, ORIGINAL_ACC_CURRENCY and CUSTOMER_EQUALS_ACCOUNT ##

mrefined_small[is.na(mrefined_small$NEW_ACCOUNT_BALANCE) == TRUE, c("NEW_ACCOUNT_BALANCE")]  <- mean(as.numeric(mrefined_small[['NEW_ACCOUNT_BALANCE']]), na.rm = T)
# imputed by 13365946

sum(is.na(mrefined_small$STATUS))
mrefined_small[is.na(mrefined_small$STATUS) == TRUE, "STATUS"]  <- Mode(mrefined_small[['STATUS']])
# imputed by "OPENED"

sum(is.na(mrefined_small$ORIGINAL_ACCOUNT_CURRENCY))
mrefined_small[is.na(mrefined_small$ORIGINAL_ACCOUNT_CURRENCY) == TRUE, "ORIGINAL_ACCOUNT_CURRENCY"]  <- Mode(mrefined_small[['ORIGINAL_ACCOUNT_CURRENCY']])
# imputed by "HUF"

sum(is.na(mrefined_small$Customer_Equals_Account)) #NA's same as Original Account Currency
mrefined_small[is.na(mrefined_small$Customer_Equals_Account) == TRUE, "Customer_Equals_Account"]  <- Mode(mrefined_small[['Customer_Equals_Account']])
# imputed by "0"

## Impute customer region ##
sum(is.na(mrefined_small$CUSTOMER_REGION)) #3346 missing values
customerreg<- mrefined_small[which(is.na(mrefined_small$CUSTOMER_REGION)),]
table(customerreg$COUNTRY_OF_RESIDENCE_abbrev) #240 HU residents, impute these guys with mode of customer region

# FOr HU residents, good idea to impute with mode of customer region
mrefined_small[which((is.na(mrefined_small$CUSTOMER_REGION)) & (mrefined_small$COUNTRY_OF_RESIDENCE_abbrev=="HU")),'CUSTOMER_REGION']  <- Mode(mrefined_small[['CUSTOMER_REGION']])
sum(is.na(mrefined_small$CUSTOMER_REGION))#now 3106
table(mrefined_small[which(is.na(mrefined_small$CUSTOMER_REGION)),'COUNTRY_OF_RESIDENCE_abbrev'] ) #no HU residents among missing cust regions!

# call the other NA's "OUTSIDE_HUNGARY"
mrefined_small$CUSTOMER_REGION[is.na(mrefined_small$CUSTOMER_REGION)]<-"OUTSIDE_HUNGARY"
sum(is.na(mrefined_small$CUSTOMER_REGION))#now 0
sum(is.na(mrefined_small))
# now convert all character vars to factors and all continuous variables to numeric or int
lapply(mrefined_small, class)#obtained all classes, now check what we need to be factors and numeric!
colnames(mrefined_small) #see column indices for specifying class types

# now convert all character vars to factors and all continuous variables to numeric or int 

vector_attr = names(select_if(mrefined_small,is.integer))
mrefined_small = mrefined_small %>% mutate_if(names(mrefined_small) %in% vector_attr,as.numeric)
mrefined_small = mrefined_small %>% mutate_if(is.character,as.factor) %>% mutate_if(is.logical,as.factor)


#mrefined_small[,c(1,2,4,5,6,7,8,9,13,14,15,16,17,18,20,21,22,23,24,25,26,27,28,29)]<-as.data.frame(sapply(mrefined_small[,c(1,2,4,5,6,7,8,9,13,14,15,16,17,18,20,21,22,23,24,25,26,27,28,29)], as.character))
#mrefined_small[,c(10,11,12,19)] <- as.data.frame(sapply(mrefined_small[,c(10,11,12,19)], as.numeric))
#mrefined_small$EVENT_DATE<-(as.Date(mrefined_small$EVENT_DATE, format="%m/%d/%Y"))
#mrefined_small<-arrange(mrefined_small, EVENT_DATE)#ordered earliest to latest
# as we are not doing out of time test dataset as suggested by Quantexa guy as we had overfitting
str(mrefined_small)

#######################################################
## Step 4 - Exploratory Analysis and Standardization ####
#######################################################

#### Exploratory Analysis And Making Data Set Copy ####

## Find variables most related to the response ##

#info gain value
infovalue <- gain.ratio(SAR~., mrefined_small)
varnames<-data.frame(row.names(infovalue))
infovalue$varnames<-data.frame(row.names(infovalue))
sortedinfogain<-infovalue[order(infovalue$attr_importance,decreasing=TRUE),c(1,2)] %>% select(attr_importance)

#NUM_CASES,CUSTOMER_TYPE,CUSTOMER_SEGMENT,BUSINESS_TYPE2,CUSTOMER_STATUS are 5 best acc to this

# Also make a copy of newrefineddata set with original variables included - for future use of interpretting clusters!
mrefined_small_ridit<-mrefined_small

#### Standarizing variables using Ridit scores ####

## For Categoricals ##


# 1.SCENARIO
tab1=prop.table(table(mrefined_small_ridit$SAR,mrefined_small_ridit$SCENARIO),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab1) = c("Var1", "SCENARIO", "Freq_SAR")  

tab1_2=prop.table(table(mrefined_small_ridit$SCENARIO)) %>%
  data.frame()
colnames(tab1_2) = c('SCENARIO', 'Freq')
joined_tab1=left_join(tab1,tab1_2, by = 'SCENARIO')

ridit_tab1=ridit_scores(joined_tab1$Freq) %>% data.frame()
colnames(ridit_tab1)='Ridit_Scores'
ridit_tab1=bind_cols(joined_tab1,ridit_tab1) %>% select(c('SCENARIO','Ridit_Scores')) #joined riditscores and joined scores but selected only 2 columns- scenarios and Ridit scores
colnames(ridit_tab1)=c('SCENARIO','RS_SCENARIO')
mrefined_small_ridit<-left_join(mrefined_small_ridit,ridit_tab1,by = 'SCENARIO')

# 2.EVENT_MONTH
tab2=prop.table(table(mrefined_small_ridit$SAR,mrefined_small_ridit$EVENT_MONTH),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab2) = c("Var1", "EVENT_MONTH", "Freq_SAR")  

tab2_2=prop.table(table(mrefined_small_ridit$EVENT_MONTH)) %>%
  data.frame()
colnames(tab2_2) = c('EVENT_MONTH', 'Freq')
joined_tab2=left_join(tab2,tab2_2, by = 'EVENT_MONTH')

ridit_tab2=ridit_scores(joined_tab2$Freq) %>% data.frame()
colnames(ridit_tab2)='Ridit_Scores'
ridit_tab2=bind_cols(joined_tab2,ridit_tab2) %>% select(c('EVENT_MONTH','Ridit_Scores'))
colnames(ridit_tab2)=c('EVENT_MONTH','RS_EVENT_MONTH')
mrefined_small_ridit<-left_join(mrefined_small_ridit,ridit_tab2,by = 'EVENT_MONTH')

# 8.RAISED_ON Ridit Scores for tab8
tab8=prop.table(table(mrefined_small_ridit$SAR,mrefined_small_ridit$RAISED_ON),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab8) = c("Var1", "RAISED_ON", "Freq_SAR")  

tab8_2=prop.table(table(mrefined_small_ridit$RAISED_ON)) %>%
  data.frame()
colnames(tab8_2) = c('RAISED_ON', 'Freq')
joined_tab8=left_join(tab8,tab8_2, by = 'RAISED_ON')
tab8
tab8_2
ridit_tab8=ridit_scores(joined_tab8$Freq) %>% data.frame()
colnames(ridit_tab8)='Ridit_Scores'
ridit_tab8=bind_cols(joined_tab8,ridit_tab8) %>% select(c('RAISED_ON','Ridit_Scores'))
colnames(ridit_tab8)=c('RAISED_ON','RS_RAISED_ON')
mrefined_small_ridit<-left_join(mrefined_small_ridit,ridit_tab8,by = 'RAISED_ON')

# 9.PEP_FLAG Ridit Scores for tab9
tab9=prop.table(table(mrefined_small_ridit$SAR,mrefined_small_ridit$PEP_FLAG),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab9) = c("Var1", "PEP_FLAG", "Freq_SAR")  

tab9_2=prop.table(table(mrefined_small_ridit$PEP_FLAG)) %>%
  data.frame()
colnames(tab9_2) = c('PEP_FLAG', 'Freq')
joined_tab9=left_join(tab9,tab9_2, by = 'PEP_FLAG')
tab9
tab9_2
ridit_tab9=ridit_scores(joined_tab9$Freq) %>% data.frame()
colnames(ridit_tab9)='Ridit_Scores'
ridit_tab9=bind_cols(joined_tab9,ridit_tab9) %>% select(c('PEP_FLAG','Ridit_Scores'))
colnames(ridit_tab9)=c('PEP_FLAG','RS_PEP_FLAG')
mrefined_small_ridit<-left_join(mrefined_small_ridit,ridit_tab9,by = 'PEP_FLAG')


# 10.SPECIAL_ATTENTION_FLAG Ridit Scores for tab10
tab10=prop.table(table(mrefined_small_ridit$SAR,mrefined_small_ridit$SPECIAL_ATTENTION_FLAG),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab10) = c("Var1", "SPECIAL_ATTENTION_FLAG", "Freq_SAR")  

tab10_2=prop.table(table(mrefined_small_ridit$SPECIAL_ATTENTION_FLAG)) %>%
  data.frame()
colnames(tab10_2) = c('SPECIAL_ATTENTION_FLAG', 'Freq')
joined_tab10=left_join(tab10,tab10_2, by = 'SPECIAL_ATTENTION_FLAG')
tab10
tab10_2
ridit_tab10=ridit_scores(joined_tab10$Freq) %>% data.frame()
colnames(ridit_tab10)='Ridit_Scores'
ridit_tab10=bind_cols(joined_tab10,ridit_tab10) %>% select(c('SPECIAL_ATTENTION_FLAG','Ridit_Scores'))
colnames(ridit_tab10)=c('SPECIAL_ATTENTION_FLAG','RS_SPECIAL_ATTENTION_FLAG')
mrefined_small_ridit<-left_join(mrefined_small_ridit,ridit_tab10,by = 'SPECIAL_ATTENTION_FLAG')

# 11.BUSINESS_TYPE2 Ridit Scores for tab11
tab11=prop.table(table(mrefined_small_ridit$SAR,mrefined_small_ridit$BUSINESS_TYPE2),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab11) = c("Var1", "BUSINESS_TYPE2", "Freq_SAR")  

tab11_2=prop.table(table(mrefined_small_ridit$BUSINESS_TYPE2)) %>%
  data.frame()
colnames(tab11_2) = c('BUSINESS_TYPE2', 'Freq')
joined_tab11=left_join(tab11,tab11_2, by = 'BUSINESS_TYPE2')
tab11
tab11_2
ridit_tab11=ridit_scores(joined_tab11$Freq) %>% data.frame()
colnames(ridit_tab11)='Ridit_Scores'
ridit_tab11=bind_cols(joined_tab11,ridit_tab11) %>% select(c('BUSINESS_TYPE2','Ridit_Scores'))
colnames(ridit_tab11)=c('BUSINESS_TYPE2','RS_BUSINESS_TYPE2')
mrefined_small_ridit<-left_join(mrefined_small_ridit,ridit_tab11,by = 'BUSINESS_TYPE2')

# 12.CUSTOMER_TYPE Ridit Scores for tab12
tab12=prop.table(table(mrefined_small_ridit$SAR,mrefined_small_ridit$CUSTOMER_TYPE),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab12) = c("Var1", "CUSTOMER_TYPE", "Freq_SAR")  

tab12_2=prop.table(table(mrefined_small_ridit$CUSTOMER_TYPE)) %>%
  data.frame()
colnames(tab12_2) = c('CUSTOMER_TYPE', 'Freq')
joined_tab12=left_join(tab12,tab12_2, by = 'CUSTOMER_TYPE')
tab12
tab12_2
ridit_tab12=ridit_scores(joined_tab12$Freq) %>% data.frame()
colnames(ridit_tab12)='Ridit_Scores'
ridit_tab12=bind_cols(joined_tab12,ridit_tab12) %>% select(c('CUSTOMER_TYPE','Ridit_Scores'))
colnames(ridit_tab12)=c('CUSTOMER_TYPE','RS_CUSTOMER_TYPE')
mrefined_small_ridit<-left_join(mrefined_small_ridit,ridit_tab12,by = 'CUSTOMER_TYPE')



# 13.CUSTOMER_STATUS Ridit Scores for tab13
tab13=prop.table(table(mrefined_small_ridit$SAR,mrefined_small_ridit$CUSTOMER_STATUS),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab13) = c("Var1", "CUSTOMER_STATUS", "Freq_SAR")  

tab13_2=prop.table(table(mrefined_small_ridit$CUSTOMER_STATUS)) %>%
  data.frame()
colnames(tab13_2) = c('CUSTOMER_STATUS', 'Freq')
joined_tab13=left_join(tab13,tab13_2, by = 'CUSTOMER_STATUS')
tab13
tab13_2
ridit_tab13=ridit_scores(joined_tab13$Freq) %>% data.frame()
colnames(ridit_tab13)='Ridit_Scores'
ridit_tab13=bind_cols(joined_tab13,ridit_tab13) %>% select(c('CUSTOMER_STATUS','Ridit_Scores'))
colnames(ridit_tab13)=c('CUSTOMER_STATUS','RS_CUSTOMER_STATUS')
mrefined_small_ridit<-left_join(mrefined_small_ridit,ridit_tab13,by = 'CUSTOMER_STATUS')

#14 CUSTOMER_SEGMENT Ridit Scores for tab14 
tab14=prop.table(table(mrefined_small_ridit$SAR,mrefined_small_ridit$CUSTOMER_SEGMENT),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab14) = c("Var1", "CUSTOMER_SEGMENT", "Freq_SAR")  

tab14_2=prop.table(table(mrefined_small_ridit$CUSTOMER_SEGMENT)) %>%
  data.frame()
colnames(tab14_2) = c('CUSTOMER_SEGMENT', 'Freq')
joined_tab14=left_join(tab14,tab14_2, by = 'CUSTOMER_SEGMENT')
tab14
tab14_2
ridit_tab14=ridit_scores(joined_tab14$Freq) %>% data.frame()
colnames(ridit_tab14)='Ridit_Scores'
ridit_tab14=bind_cols(joined_tab14,ridit_tab14) %>% select(c('CUSTOMER_SEGMENT','Ridit_Scores'))
colnames(ridit_tab14)=c('CUSTOMER_SEGMENT','RS_CUSTOMER_SEGMENT')
mrefined_small_ridit<-left_join(mrefined_small_ridit,ridit_tab14,by = 'CUSTOMER_SEGMENT')



#15 TIME_PERIOD Ridit Scores for tab15 =
tab15=prop.table(table(mrefined_small_ridit$SAR,mrefined_small_ridit$TIME_PERIOD),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab15) = c("Var1", "TIME_PERIOD", "Freq_SAR")  

tab15_2=prop.table(table(mrefined_small_ridit$TIME_PERIOD)) %>%
  data.frame()
colnames(tab15_2) = c('TIME_PERIOD', 'Freq')
joined_tab15=left_join(tab15,tab15_2,by='TIME_PERIOD')
tab15
tab15_2
ridit_tab15=ridit_scores(joined_tab15$Freq) %>% data.frame()
colnames(ridit_tab15)='Ridit_Scores'
ridit_tab15=bind_cols(joined_tab15,ridit_tab15) %>% select(c('TIME_PERIOD','Ridit_Scores'))
colnames(ridit_tab15)=c('TIME_PERIOD','RS_TIME_PERIOD')
mrefined_small_ridit<-left_join(mrefined_small_ridit,ridit_tab15,by ='TIME_PERIOD')


# 17.SC01_Amount_Exceding_250000__HUF_ Ridit Scores for tab17
mrefined_small_ridit$SC01_Amount_Exceding_250000__HUF_<- as.factor(mrefined_small_ridit$SC01_Amount_Exceding_250000__HUF_)
tab17=prop.table(table(mrefined_small_ridit$SAR,mrefined_small_ridit$SC01_Amount_Exceding_250000__HUF_),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab17) = c("Var1", "SC01_Amount_Exceding_250000__HUF_", "Freq_SAR")  

tab17_2=prop.table(table(mrefined_small_ridit$SC01_Amount_Exceding_250000__HUF_)) %>%
  data.frame()
colnames(tab17_2) = c('SC01_Amount_Exceding_250000__HUF_', 'Freq')
joined_tab17=left_join(tab17,tab17_2, by = 'SC01_Amount_Exceding_250000__HUF_')
tab17
tab17_2
ridit_tab17=ridit_scores(joined_tab17$Freq) %>% data.frame()
colnames(ridit_tab17)='Ridit_Scores'
ridit_tab17=bind_cols(joined_tab17,ridit_tab17) %>% select(c('SC01_Amount_Exceding_250000__HUF_','Ridit_Scores'))
colnames(ridit_tab17)=c('SC01_Amount_Exceding_250000__HUF_','RS_SC01_Amount_Exceding_250000__HUF_')
mrefined_small_ridit<-left_join(mrefined_small_ridit,ridit_tab17,by = 'SC01_Amount_Exceding_250000__HUF_')


# 18.COUNTRY_OF_RESIDENCE_abbrev Ridit Scores for tab18
tab18=prop.table(table(mrefined_small_ridit$SAR,mrefined_small_ridit$COUNTRY_OF_RESIDENCE_abbrev),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab18) = c("Var1", "COUNTRY_OF_RESIDENCE_abbrev", "Freq_SAR")  

tab18_2=prop.table(table(mrefined_small_ridit$COUNTRY_OF_RESIDENCE_abbrev)) %>%
  data.frame()
colnames(tab18_2) = c('COUNTRY_OF_RESIDENCE_abbrev', 'Freq')
joined_tab18=left_join(tab18,tab18_2, by = 'COUNTRY_OF_RESIDENCE_abbrev')
tab18
tab18_2
ridit_tab18=ridit_scores(joined_tab18$Freq) %>% data.frame()
colnames(ridit_tab18)='Ridit_Scores'
ridit_tab18=bind_cols(joined_tab18,ridit_tab18) %>% select(c('COUNTRY_OF_RESIDENCE_abbrev','Ridit_Scores'))
colnames(ridit_tab18)=c('COUNTRY_OF_RESIDENCE_abbrev','RS_COUNTRY_OF_RESIDENCE_abbrev')
mrefined_small_ridit<-left_join(mrefined_small_ridit,ridit_tab18,by = 'COUNTRY_OF_RESIDENCE_abbrev')


# 19.CUSTOMER_REGION Ridit Scores for tab19
tab19=prop.table(table(mrefined_small_ridit$SAR,mrefined_small_ridit$CUSTOMER_REGION),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab19) = c("Var1", "CUSTOMER_REGION", "Freq_SAR")  

tab19_2=prop.table(table(mrefined_small_ridit$CUSTOMER_REGION)) %>%
  data.frame()
colnames(tab19_2) = c('CUSTOMER_REGION', 'Freq')
joined_tab19=left_join(tab19,tab19_2, by = 'CUSTOMER_REGION')
tab19
tab19_2
ridit_tab19=ridit_scores(joined_tab19$Freq) %>% data.frame()
colnames(ridit_tab19)='Ridit_Scores'
ridit_tab19=bind_cols(joined_tab19,ridit_tab19) %>% select(c('CUSTOMER_REGION','Ridit_Scores'))
colnames(ridit_tab19)=c('CUSTOMER_REGION','RS_CUSTOMER_REGION')
mrefined_small_ridit<-left_join(mrefined_small_ridit,ridit_tab19,by = 'CUSTOMER_REGION')


# 20.ORIGINAL_ACCOUNT_CURRENCY Ridit Scores for tab20
tab20=prop.table(table(mrefined_small_ridit$SAR,mrefined_small_ridit$ORIGINAL_ACCOUNT_CURRENCY),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab20) = c("Var1", "ORIGINAL_ACCOUNT_CURRENCY", "Freq_SAR")  

tab20_2=prop.table(table(mrefined_small_ridit$ORIGINAL_ACCOUNT_CURRENCY)) %>%
  data.frame()
colnames(tab20_2) = c('ORIGINAL_ACCOUNT_CURRENCY', 'Freq')
joined_tab20=left_join(tab20,tab20_2, by = 'ORIGINAL_ACCOUNT_CURRENCY')
tab20
tab20_2
ridit_tab20=ridit_scores(joined_tab20$Freq) %>% data.frame()
colnames(ridit_tab20)='Ridit_Scores'
ridit_tab20=bind_cols(joined_tab20,ridit_tab20) %>% select(c('ORIGINAL_ACCOUNT_CURRENCY','Ridit_Scores'))
colnames(ridit_tab20)=c('ORIGINAL_ACCOUNT_CURRENCY','RS_ORIGINAL_ACCOUNT_CURRENCY')
mrefined_small_ridit<-left_join(mrefined_small_ridit,ridit_tab20,by = 'ORIGINAL_ACCOUNT_CURRENCY')


# 21.Customer_Equals_Account Ridit Scores for tab21
mrefined_small_ridit$Customer_Equals_Account<-as.factor(mrefined_small_ridit$Customer_Equals_Account)

tab21=prop.table(table(mrefined_small_ridit$SAR,mrefined_small_ridit$Customer_Equals_Account),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab21) = c("Var1", "Customer_Equals_Account", "Freq_SAR")  

tab21_2=prop.table(table(mrefined_small_ridit$Customer_Equals_Account)) %>%
  data.frame()
colnames(tab21_2) = c('Customer_Equals_Account', 'Freq')
joined_tab21=left_join(tab21,tab21_2, by = 'Customer_Equals_Account')
tab21
tab21_2
ridit_tab21=ridit_scores(joined_tab21$Freq) %>% data.frame()
colnames(ridit_tab21)='Ridit_Scores'
ridit_tab21=bind_cols(joined_tab21,ridit_tab21) %>% select(c('Customer_Equals_Account','Ridit_Scores'))
colnames(ridit_tab21)=c('Customer_Equals_Account','RS_Customer_Equals_Account')
mrefined_small_ridit<-left_join(mrefined_small_ridit,ridit_tab21,by = 'Customer_Equals_Account')


# 22.STATUS Ridit Scores for tab22
tab22=prop.table(table(mrefined_small_ridit$SAR,mrefined_small_ridit$STATUS),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab22) = c("Var1", "STATUS", "Freq_SAR")  

tab22_2=prop.table(table(mrefined_small_ridit$STATUS)) %>%
  data.frame()
colnames(tab22_2) = c('STATUS', 'Freq')
joined_tab22=left_join(tab22,tab22_2, by = 'STATUS')
tab22
tab22_2
ridit_tab22=ridit_scores(joined_tab22$Freq) %>% data.frame()
colnames(ridit_tab22)='Ridit_Scores'
ridit_tab22=bind_cols(joined_tab22,ridit_tab22) %>% select(c('STATUS','Ridit_Scores'))
colnames(ridit_tab22)=c('STATUS','RS_STATUS')
mrefined_small_ridit<-left_join(mrefined_small_ridit,ridit_tab22,by = 'STATUS')

# 16.DISTRICT_OF_BUDAPEST Ridit Scores for tab16
tab16=prop.table(table(mrefined_small_ridit$SAR,mrefined_small_ridit$DISTRICT_OF_BUDAPEST),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab16) = c("Var1", "DISTRICT_OF_BUDAPEST", "Freq_SAR")  

tab16_2=prop.table(table(mrefined_small_ridit$DISTRICT_OF_BUDAPEST)) %>%
  data.frame()
colnames(tab16_2) = c('DISTRICT_OF_BUDAPEST', 'Freq')
joined_tab16=left_join(tab16,tab16_2, by = 'DISTRICT_OF_BUDAPEST')
tab16
tab16_2
ridit_tab16=ridit_scores(joined_tab16$Freq) %>% data.frame()
colnames(ridit_tab16)='Ridit_Scores'
ridit_tab16=bind_cols(joined_tab16,ridit_tab16) %>% select(c('DISTRICT_OF_BUDAPEST','Ridit_Scores'))
colnames(ridit_tab16)=c('DISTRICT_OF_BUDAPEST','RS_DISTRICT_OF_BUDAPEST')
mrefined_small_ridit<-left_join(mrefined_small_ridit,ridit_tab16,by = 'DISTRICT_OF_BUDAPEST')




# 22. CREDIT_DEBIT_CODE Ridit Scores for tab23
tab23=prop.table(table(mrefined_small_ridit$SAR,mrefined_small_ridit$CREDIT_DEBIT_CODE),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab23) = c("Var1", "CREDIT_DEBIT_CODE", "Freq_SAR")  

tab23_2=prop.table(table(mrefined_small_ridit$CREDIT_DEBIT_CODE)) %>%
  data.frame()
colnames(tab23_2) = c('CREDIT_DEBIT_CODE', 'Freq')
joined_tab23=left_join(tab23,tab23_2, by = 'CREDIT_DEBIT_CODE')
tab23
tab23_2
ridit_tab23=ridit_scores(joined_tab23$Freq) %>% data.frame()
colnames(ridit_tab23)='Ridit_Scores'
ridit_tab23=bind_cols(joined_tab23,ridit_tab23) %>% select(c('CREDIT_DEBIT_CODE','Ridit_Scores'))
colnames(ridit_tab23)=c('CREDIT_DEBIT_CODE','RS_CREDIT_DEBIT_CODE')
mrefined_small_ridit<-left_join(mrefined_small_ridit,ridit_tab23,by = 'CREDIT_DEBIT_CODE')
names(mrefined_small_ridit)

# 3.TXN_TYPE Ridit Scores for tab3
tab3=prop.table(table(mrefined_small_ridit$SAR,mrefined_small_ridit$TXN_TYPE),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab3) = c("Var1", "TXN_TYPE", "Freq_SAR")  

tab3_2=prop.table(table(mrefined_small_ridit$TXN_TYPE)) %>%
  data.frame()
colnames(tab3_2) = c('TXN_TYPE', 'Freq')
joined_tab3=left_join(tab3,tab3_2, by = 'TXN_TYPE')
tab3
tab3_2
ridit_tab3=ridit_scores(joined_tab3$Freq) %>% data.frame()
colnames(ridit_tab3)='Ridit_Scores'
ridit_tab3=bind_cols(joined_tab3,ridit_tab3) %>% select(c('TXN_TYPE','Ridit_Scores'))
colnames(ridit_tab3)=c('TXN_TYPE','RS_TXN_TYPE')
mrefined_small_ridit<-left_join(mrefined_small_ridit,ridit_tab3,by = 'TXN_TYPE')


# 4.INSTRUMENT Ridit Scores for tab4
tab4=prop.table(table(mrefined_small_ridit$SAR,mrefined_small_ridit$INSTRUMENT),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab4) = c("Var1", "INSTRUMENT", "Freq_SAR")  

tab4_2=prop.table(table(mrefined_small_ridit$INSTRUMENT)) %>%
  data.frame()
colnames(tab4_2) = c('INSTRUMENT', 'Freq')
joined_tab4=left_join(tab4,tab4_2, by = 'INSTRUMENT')
tab4
tab4_2
ridit_tab4=ridit_scores(joined_tab4$Freq) %>% data.frame()
colnames(ridit_tab4)='Ridit_Scores'
ridit_tab4=bind_cols(joined_tab4,ridit_tab4) %>% select(c('INSTRUMENT','Ridit_Scores'))
colnames(ridit_tab4)=c('INSTRUMENT','RS_INSTRUMENT')
mrefined_small_ridit<-left_join(mrefined_small_ridit,ridit_tab4,by = 'INSTRUMENT')


# 5.SCOPE Ridit Scores for tab5
tab5=prop.table(table(mrefined_small_ridit$SAR,mrefined_small_ridit$SCOPE),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab5) = c("Var1", "SCOPE", "Freq_SAR")  

tab5_2=prop.table(table(mrefined_small_ridit$SCOPE)) %>%
  data.frame()
colnames(tab5_2) = c('SCOPE', 'Freq')
joined_tab5=left_join(tab5,tab5_2, by = 'SCOPE')
tab5
tab5_2
ridit_tab5=ridit_scores(joined_tab5$Freq) %>% data.frame()
colnames(ridit_tab5)='Ridit_Scores'
ridit_tab5=bind_cols(joined_tab5,ridit_tab5) %>% select(c('SCOPE','Ridit_Scores'))
colnames(ridit_tab5)=c('SCOPE','RS_SCOPE')
mrefined_small_ridit<-left_join(mrefined_small_ridit,ridit_tab5,by = 'SCOPE')



# 6.REGION Ridit Scores for tab6
tab6=prop.table(table(mrefined_small_ridit$SAR,mrefined_small_ridit$REGION),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab6) = c("Var1", "REGION", "Freq_SAR")  

tab6_2=prop.table(table(mrefined_small_ridit$REGION)) %>%
  data.frame()
colnames(tab6_2) = c('REGION', 'Freq')
joined_tab6=left_join(tab6,tab6_2, by = 'REGION')
tab6
tab6_2
ridit_tab6=ridit_scores(joined_tab6$Freq) %>% data.frame()
colnames(ridit_tab6)='Ridit_Scores'
ridit_tab6=bind_cols(joined_tab6,ridit_tab6) %>% select(c('REGION','Ridit_Scores'))
colnames(ridit_tab6)=c('REGION','RS_REGION')
mrefined_small_ridit<-left_join(mrefined_small_ridit,ridit_tab6,by = 'REGION')


# 7.BASE_CURRENCY_AMOUNT Ridit Scores for tab7
tab7=prop.table(table(mrefined_small_ridit$SAR,mrefined_small_ridit$BASE_CURRENCY_AMOUNT),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab7) = c("Var1", "BASE_CURRENCY_AMOUNT", "Freq_SAR")  

tab7_2=prop.table(table(mrefined_small_ridit$BASE_CURRENCY_AMOUNT)) %>%
  data.frame()
colnames(tab7_2) = c('BASE_CURRENCY_AMOUNT', 'Freq')
joined_tab7=left_join(tab7,tab7_2, by = 'BASE_CURRENCY_AMOUNT')
tab7
tab7_2
ridit_tab7=ridit_scores(joined_tab7$Freq) %>% data.frame()
colnames(ridit_tab7)='Ridit_Scores'
ridit_tab7=bind_cols(joined_tab7,ridit_tab7) %>% select(c('BASE_CURRENCY_AMOUNT','Ridit_Scores'))
colnames(ridit_tab7)=c('BASE_CURRENCY_AMOUNT','RS_BASE_CURRENCY_AMOUNT')
mrefined_small_ridit<-left_join(mrefined_small_ridit,ridit_tab7,by = 'BASE_CURRENCY_AMOUNT')


names(mrefined_num_small)


rm(list=ls(pattern=c('tab')))
#remove all the useless variables from mrefined_small_ridit
#mrefined_small_ridit<-subset(mrefined_small_ridit,select=c(3,10,11,12,16,19,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52))# remove original


## For Continuous ##
# we have variables with different ranges so its important to standardise for clustering!

#investigate std of vars
mrefined_num_small=select_if(mrefined_small_ridit, is.numeric)#not event_date or SAR
string_vector <- names(mrefined_num_small)
standdev<-sapply(mrefined_num_small[string_vector], sd)
range(standdev) #LARGE RANGE OF STDDEV!!

# Scale all variables before clustering by Euclidean dist - This is because  if one of the variables has a much larger variance than the others it will dominate the distance measure 
mrefined_num_small=scale(mrefined_num_small) %>% data.frame() #scale all variables before clustering by Euclidean dist
set.seed(123);smple_mrefined_num_small<-mrefined_num_small[sample(1:nrow(mrefined_num_small),10000),]

#%#######################################%
#### Step 5 - Bottom Up Clustering ####
########################################%

## How many clusters? - GAP Statistic, Answer = 3
set.seed(123); clest = Clest(as.matrix(smple_mrefined_num_small), maxK = 5, alpha = 0.1, B = 15, B0 = 5, nstart = 1000)
cluster_df_small=mrefined_num_small

## RS Kmeans
# I did Clest on subsample of 15000 with carlos and found 3 clusters was best! - did it with alpha=0.1
set.seed(123);kperm=KMeansSparseCluster.permute(smple_mrefined_num_small, K=3, nperms = 5)#gets optimum tuning param L1= 2.971735


set.seed(123); rskm_cl3_small = RSKC(d = mrefined_num_small, ncl = 3, alpha = 0.1, L1 = 2.971735)
fviz_cluster(list(data = mrefined_num_small, cluster = rskm_cl3_small$labels ),stand = FALSE, geom = "point",
             frame.type = "norm")
mrefined_small_ridit$rskm_k3=rskm_cl3_small$labels
cluster_vars_small = data.frame(unlist(attributes(rskm_cl3_small$weights)),rskm_cl3_small$weights) %>%
filter(rskm_cl3_small$weights >0) %>% arrange(desc(rskm_cl3_small.weights))
colnames(cluster_vars_small)=c('Feature','Importance')

###mrefined_small$clusterprofile_label_small<-ifelse(mrefined_small$CUSTOMER_TYPE== "Corporate",mrefined_small$clusterprofile_label_small== 2 || mrefined_small$clusterprofile_label_small == 3,
# mrefined_small$clusterprofile_label_small== 1 || mrefined_small$clusterprofile_label_small == 3)




######### Step 6  Model Building #############

####Creating different datasets for each cluster ##
cluster1_small = mrefined_small_ridit %>% filter(rskm_k3 == 1) %>%
  select(SAR, NUM_CASES, NUM_ACCOUNTS, NEW_ACCOUNT_BALANCE, CUSTOMER_FOR_DAYS, contains('RS_'))
cluster2_small = mrefined_small_ridit %>% filter(rskm_k3 == 2) %>%
  select(SAR, NUM_CASES, NUM_ACCOUNTS, NEW_ACCOUNT_BALANCE, CUSTOMER_FOR_DAYS, contains('RS_'))
cluster3_small = mrefined_small_ridit %>% filter(rskm_k3== 3) %>%
  select(SAR, NUM_CASES, NUM_ACCOUNTS, NEW_ACCOUNT_BALANCE, CUSTOMER_FOR_DAYS, contains('RS_'))

#to be able to explain this tree

## Bharat's Tree ##
treectrl = trainControl(method = "cv", number = 10)


set.seed(123); tree_RSKM = train(rskm_k3 ~., data = mrefined_small_ridit, method = "rpart",
                                 trControl=treectrl,
                                 tuneLength = 10)

rpart.plot::prp(tree_RSKM$finalModel, box.palette = "Reds", tweak = 1.2)

####################################################################%
#### Step 6 - Data Split, Class Imbalance and Feature Selection ####
####################################################################%

### Cluster 1 ####

## Getting the training and test set ##
set.seed(123); cl1_index = createDataPartition(cluster1_small$SAR,p=.70)
c1_rskm_data_train = cluster1_small[unlist(cl1_index),]

c1_rskm_data_test = cluster1_small[-unlist(cl1_index),]

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




paste((sprintf("subgroup_%s",seq(1:39))),collapse = ',')#copy paste this list of subgroup features to bind with dataframe

#join these feature vectors into the dataframe!
c1_rskm_data_train<- cbind(c1_rskm_data_train,subgroup_1,subgroup_2,subgroup_3,subgroup_4,subgroup_5,subgroup_6,subgroup_7,subgroup_8,subgroup_9,subgroup_10,subgroup_11,subgroup_12,subgroup_13,subgroup_14,subgroup_15,subgroup_16,subgroup_17,subgroup_18,subgroup_19,subgroup_20,subgroup_21,subgroup_22,subgroup_23,subgroup_24,subgroup_25,subgroup_26,subgroup_27,subgroup_28,subgroup_29,subgroup_30,subgroup_31,subgroup_32,subgroup_33,subgroup_34,subgroup_35,subgroup_36,subgroup_37,subgroup_38,subgroup_39)


names(c1_rskm_data_train)


#Anomaly detection using Isolation forest for each cluster to identify grouped anomalies ###
#Isolation Forest 
library(IsolationForest)
iforest = IsolationForest::IsolationTrees(c1_rskm_data_train,rFactor=1,nRowSamp =256,ntree = 100) 
# sub sampling size of 256 and ntree=10 recommended by paper from iforest
# reference: https://cs.nju.edu.cn/zhouzh/zhouzh.files/publication/icdm08b.pdf

c1_rskm_data_train$anomaly_scorec1 = AnomalyScore(c1_rskm_data_train,iforest)$outF #score !!!

names(c1_rskm_data_train)

## Feature Reduction ####
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

c1featlist_ordered = attStats(c1_boruta) %>%
  rownames_to_column(var = 'Features') %>% arrange(desc(medianImp))  %>% select(c('Features','medianImp','decision'))

topfeaturesc1<-c1featlist_ordered[c1featlist_ordered$decision =='Confirmed',1] # just select confirmed features
topfeaturesc1
SAR<-c1_rskm_data_train$SAR
c1_rskm_data_trainold<-subset(c1_rskm_data_train,select=topfeaturesc1)
c1_rskm_data_train_boruta<-cbind(c1_rskm_data_trainold,SAR)
c1_rskm_data_train_levelnamesunchanged<-c1_rskm_data_train_boruta # for XG Boost
correlationc1<-data.frame(cor(c1_rskm_data_train_boruta[,-11]))#all good
correlationc1
#High correlation variables:
#Num_cases and Customer for Days
#Num_cases vs District of Budapest
# Check for these in models (especially logistic)



names_importantfeatures_c1_rskm_data_train <- c(c1featlist_ordered[c1featlist_ordered$decision=="Confirmed",'Features'],'SAR') # features important are these!
c1_rskm_data_train_boruta<-subset(c1_rskm_data_train,select= names_importantfeatures_c1_rskm_data_train)
lapply(c1_rskm_data_train_boruta,class)
correlation_smotec1<-data.frame(cor(c1_rskm_data_train_boruta[,names(c1_rskm_data_train_boruta)!='SAR']))
correlation_smotec1
# out of the 19 variables selected watch out for high correl in numaccs, customer for days, numcases and district of budapest
# just select top 10


## Class imbalance issue ##



# SMOTE
table(c1_rskm_data_train$SAR)
set.seed(123); smotedatac1 <- SMOTE(SAR ~ ., c1_rskm_data_train_boruta, perc.over = 400, perc.under=125)#balances classes and also doesnt take that much time to run boruta and also not so many observations thrown away
table(smotedatac1$SAR)
names(smotedatac1)

### Cluster 2 ####

## Getting the training and test set ##

set.seed(123); cl2_index = createDataPartition(cluster2_small$SAR,p=.70)
c2_rskm_data_train = cluster2_small[unlist(cl2_index),]

c2_rskm_data_test = cluster2_small[-unlist(cl2_index),]


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




paste((sprintf("subgroup_%s",seq(1:60))),collapse = ',')#copy paste this list of subgroup features to bind with dataframe

#join these feature vectors into the dataframe!
c2_rskm_data_train<- cbind(c2_rskm_data_train,subgroup_1,subgroup_2,subgroup_3,subgroup_4,subgroup_5,subgroup_6,subgroup_7,subgroup_8,subgroup_9,subgroup_10,subgroup_11,subgroup_12,subgroup_13,subgroup_14,subgroup_15,subgroup_16,subgroup_17,subgroup_18,subgroup_19,subgroup_20,subgroup_21,subgroup_22,subgroup_23,subgroup_24,subgroup_25,subgroup_26,subgroup_27,subgroup_28,subgroup_29,subgroup_30,subgroup_31,subgroup_32,subgroup_33,subgroup_34,subgroup_35,subgroup_36,subgroup_37,subgroup_38,subgroup_39,subgroup_40,subgroup_41,subgroup_42,subgroup_43,subgroup_44,subgroup_45,subgroup_46,subgroup_47,subgroup_48,subgroup_49,subgroup_50,subgroup_51,subgroup_52,subgroup_53,subgroup_54,subgroup_55,subgroup_56,subgroup_57,subgroup_58,subgroup_59,subgroup_60)


names(c2_rskm_data_train)


#Anomaly detection using Isolation forest for each cluster to identify grouped anomalies ###
#Isolation Forest 
library(IsolationForest)
iforest = IsolationForest::IsolationTrees(c2_rskm_data_train,rFactor=1,nRowSamp =256,ntree = 100) 
# sub sampling size of 256 and ntree=10 recommended by paper from iforest
# reference: https://cs.nju.edu.cn/zhouzh/zhouzh.files/publication/icdm08b.pdf

c2_rskm_data_train$anomaly_scorec2 = AnomalyScore(c2_rskm_data_train,iforest)$outF #score !!!

names(c2_rskm_data_train)

## Feature Reduction ##


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

c2featlist_ordered = attStats(c2_boruta) %>%
  rownames_to_column(var = 'Features') %>% arrange(desc(medianImp))  %>% select(c('Features','medianImp','decision'))

topfeaturesc2<-c2featlist_ordered[c2featlist_ordered$decision =='Confirmed',1] # just select confirmed features
topfeaturesc2
SAR<-c2_rskm_data_train$SAR
c2_rskm_data_trainold<-subset(c2_rskm_data_train,select=topfeaturesc2)
c2_rskm_data_train_boruta<-cbind(c2_rskm_data_trainold,SAR)
c2_rskm_data_train_levelnamesunchanged<-c2_rskm_data_train_boruta # for XG Boost
correlationc2<-data.frame(cor(c2_rskm_data_train_boruta[,-70]))#all good
correlationc2
#High correlation variables:
#Num_cases and Customer for Days
#Num_cases vs District of Budapest
# Check for these in models (especially logistic)




## Class imbalance issue ##



# SMOTE
table(c2_rskm_data_train$SAR)
set.seed(123); smotedatac2 <- SMOTE(SAR ~ ., c2_rskm_data_train_boruta, perc.over = 400, perc.under=125)#balances classes and also doesnt take that much time to run boruta and also not so many observations thrown away
table(smotedatac2$SAR)




### Cluster 3 ####

## Getting the training and test set ##
set.seed(123); cl3_index = createDataPartition(cluster3_small$SAR,p=.70)
c3_rskm_data_train = cluster3_small[unlist(cl3_index),]

c3_rskm_data_test = cluster3_small[-unlist(cl3_index),]


## 4b(2) Using subgroup.discovery

library(subgroup.discovery) #this doesn't suffice, load the package as functions in global environment
names(c3_rskm_data_train)
table(c3_rskm_data_train$SAR)

c3_rskm_data_train_sd<- c3_rskm_data_train
c3_rskm_data_train_sd$SAR<- as.numeric(c3_rskm_data_train_sd$SAR)


#run function prim.peel separately
names(c3_rskm_data_train_sd)
table(c3_rskm_data_train_sd$SAR)
peel_c3 <- prim.peel(X=c3_rskm_data_train_sd[,-1],y=c3_rskm_data_train_sd$SAR,N=nrow(c3_rskm_data_train_sd),peeling.quantile = 0.05,max.peel=0.1,min.support = 0.1,quality.function = 'mean')

peel_c3$supports
peel_c3$box.qualities #each box cover around 10% of the SAR's in the whole small data

#get rules
peel_c3<-as.data.frame(cbind(as.character(peel_c3$rule.names),noquote(as.character(peel_c3$rule.operators)),peel_c3$rule.values))
rules_c3<-do.call(paste, c(as.list(peel_c3), sep = ""))
rules_c3
rules_c3_ridit<- paste0('c3_rskm_data_train_sd$',rules_c3) #add dataframe reference as prefix
rules_c3_ridit#rules of SD for higher SAR level defined for our dataset

#mrefined_small_riditnew<-mrefined_small_ridit
#for (i in 1:length(rules_small_ridit)){
#mrefined_small_riditnew<- mutate(mrefined_small_riditnew, eval(parse(match(rules_small[i],rules_small))) = ifelse(eval(parse(text=paste(rules_small[i]))),1,0))}

#featurize these rules as subgroup features
for (i in 1:length(rules_c3_ridit)){
  assign(paste("subgroup",i,sep="_"),ifelse(eval(parse(text=paste(rules_c3_ridit[i]))),1,0))  
} #creates 61 new vectors which can be used as features




paste((sprintf("subgroup_%s",seq(1:60))),collapse = ',')#copy paste this list of subgroup features to bind with dataframe

#join these feature vectors into the dataframe!
c3_rskm_data_train<- cbind(c3_rskm_data_train,subgroup_1,subgroup_2,subgroup_3,subgroup_4,subgroup_5,subgroup_6,subgroup_7,subgroup_8,subgroup_9,subgroup_10,subgroup_11,subgroup_12,subgroup_13,subgroup_14,subgroup_15,subgroup_16,subgroup_17,subgroup_18,subgroup_19,subgroup_20,subgroup_21,subgroup_22,subgroup_23,subgroup_24,subgroup_25,subgroup_26,subgroup_27,subgroup_28,subgroup_29,subgroup_30,subgroup_31,subgroup_32,subgroup_33,subgroup_34,subgroup_35,subgroup_36,subgroup_37,subgroup_38,subgroup_39,subgroup_40,subgroup_41,subgroup_42,subgroup_43,subgroup_44,subgroup_45,subgroup_46,subgroup_47,subgroup_48,subgroup_49,subgroup_50,subgroup_51,subgroup_52,subgroup_53,subgroup_54,subgroup_55,subgroup_56,subgroup_57,subgroup_58,subgroup_59,subgroup_60)


names(c3_rskm_data_train)


#Anomaly detection using Isolation forest for each cluster to identify grouped anomalies ###
#Isolation Forest 
library(IsolationForest)
iforest = IsolationForest::IsolationTrees(c3_rskm_data_train,rFactor=1,nRowSamp =256,ntree = 100) 
# sub sampling size of 256 and ntree=10 recommended by paper from iforest
# reference: https://cs.nju.edu.cn/zhouzh/zhouzh.files/publication/icdm08b.pdf

c3_rskm_data_train$anomaly_scorec3 = AnomalyScore(c3_rskm_data_train,iforest)$outF #score !!!

names(c3_rskm_data_train)

## Feature Reduction ##


# Boruta: Graph - SMOTE
set.seed(123); c3_boruta = Boruta(c3_rskm_data_train$SAR~., data = c3_rskm_data_train, doTrace = 2)
print(c3_boruta)
plot(c3_boruta, xlab = "", xaxt = "n")
lz=lapply(1:ncol(c3_boruta$ImpHistory),function(i)
  c3_boruta$ImpHistory[is.finite(c3_boruta$ImpHistory[,i]),i])
names(lz) = colnames(c3_boruta$ImpHistory)
Labels = sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(c3_boruta$ImpHistory), cex.axis = 0.7)
c3_boruta_formula=getConfirmedFormula(c3_boruta)
# The ranked list of most relevant features

c3featlist_ordered = attStats(c3_boruta) %>%
  rownames_to_column(var = 'Features') %>% arrange(desc(medianImp))  %>% select(c('Features','medianImp','decision'))

topfeaturesc3<-c3featlist_ordered[c3featlist_ordered$decision =='Confirmed',1] # just select confirmed features
topfeaturesc3
SAR<-c3_rskm_data_train$SAR
c3_rskm_data_trainold<-subset(c3_rskm_data_train,select=topfeaturesc3)
c3_rskm_data_train_boruta<-cbind(c3_rskm_data_trainold,SAR)
c3_rskm_data_train_levelnamesunchanged<-c3_rskm_data_train_boruta # for XG Boost
correlationc3<-data.frame(cor(c3_rskm_data_train_boruta[,-70]))#all good
correlationc3
#High correlation variables:
#Num_cases and Customer for Days
#Num_cases vs District of Budapest
# Check for these in models (especially logistic)




## Class imbalance issue ##



# SMOTE
table(c3_rskm_data_train$SAR)
set.seed(123); smotedatac3 <- SMOTE(SAR ~ ., c3_rskm_data_train_boruta, perc.over = 400, perc.under=125)#balances classes and also doesnt take that much time to run boruta and also not so many observations thrown away
table(smotedatac3$SAR)

#################################%
#### Step 7 - Model Building ####
#################################%


## a)Logistic Regression - Cluster 1 ####

#need to do this first to get syntactically correct level names for my factor variables (only SAR)
feature.names=names(smotedatac1)

# for smotedatac1_boruta with 10 variables
for (f in feature.names) {
  if (class(smotedatac1[[f]])=="factor") {
    levels <- unique(c(smotedatac1[[f]]))
    smotedatac1[[f]] <- factor(smotedatac1[[f]],
                               labels=make.names(levels))
  }
}






b = paste(c1featlist_ordered$Features[1:10], collapse = ' + '); formula_b = paste('SAR ~ ',b,sep = '');b
# shortcut for variables to be added into model for lm statement!


# We specify the type of evaluation we want to do
train_control <- trainControl(method="cv",number=10,savePredictions=TRUE,summaryFunction=twoClassSummary,classProbs=TRUE)#10 fold cross validation,saves probs and preds predicted by 10 models
#summaryFunction argument makes it possible to choose mtry based on AUC (use metric="ROC")


## Brief Data Exploration ##
# Is scale imprtant? Not for logistic apparently - https://stats.stackexchange.com/questions/48360/is-standardization-needed-before-fitting-logistic-regression
summary(smotedatac1)
sd(smotedatac1$NUM_CASES)
sd(smotedatac1$CUSTOMER_FOR_DAYS)#SD is a lot larger, still not a concern for logistic model building!
sd(smotedatac1$NUM_ACCOUNTS)
sd(smotedatac1$RS_SCENARIO)
names(smotedatac1)
# i)All subsets regression ####
allsubset_df_c1_small = cbind(smotedatac1[names_importantfeatures_c1_rskm_data_train[1:10]],smotedatac1$SAR)
names(smotedatac1)
subset_c1_small = bestglm(Xy = allsubset_df_c1_small,
                    family = binomial,
                    IC = "AIC",                
                    method = "exhaustive")

subset_c1_small$BestModels


#Model 2 suggests removing Raised on, num_accounts and New_account_balance and still AIC good enough (5203.528)
#hence by simplicity, go for model2 from above
table(smotedatac1$SAR)
#MOdel cluster1 
model_allsubsets_cl1_small<- train(SAR ~ subgroup_37 + RS_SCENARIO + CUSTOMER_FOR_DAYS + RS_TIME_PERIOD + NUM_ACCOUNTS + NEW_ACCOUNT_BALANCE  + RS_DISTRICT_OF_BUDAPEST ,data= smotedatac1, method = "glm", family='binomial',
                         trControl=train_control,metric='ROC')
vif(smotedatac1[,names(smotedatac1)!='SAR'])

#Multicollinearity does not seem detrimental to interpretation of regression coeff's
model_allsubsets_cl1_small #0.935 ROC on 10 fold CV!




# ii)Stepwise selection ####
null <- glm(SAR ~ 1,family=binomial, data = smotedatac1)
full <- glm(SAR ~ .,family=binomial, data = smotedatac1)
step_null_both<-stepAIC(null, scope=list(upper = ~ NUM_CASES + subgroup_37 + RS_SCENARIO + CUSTOMER_FOR_DAYS + RS_TIME_PERIOD + NUM_ACCOUNTS + NEW_ACCOUNT_BALANCE + RS_PEP_FLAG + subgroup_39 + RS_DISTRICT_OF_BUDAPEST,lower = ~ 1), direction = "both")
#suggests removing raised on and num_accounts

step_null_for<-stepAIC(null, scope=list(upper = ~ NUM_CASES + subgroup_37 + RS_SCENARIO + CUSTOMER_FOR_DAYS + RS_TIME_PERIOD + NUM_ACCOUNTS + NEW_ACCOUNT_BALANCE + RS_PEP_FLAG + subgroup_39 + RS_DISTRICT_OF_BUDAPEST, lower = ~ 1), direction = "forward")
#suggests removing raised on and num_accounts

step_null_both$anova

step_null_for$anova


model2_stepwise_cl1_small<- train(SAR ~  subgroup_37 + CUSTOMER_FOR_DAYS + RS_SCENARIO + RS_DISTRICT_OF_BUDAPEST + 
                                    NUM_ACCOUNTS + RS_TIME_PERIOD + NEW_ACCOUNT_BALANCE,data= smotedatac1, method = "glm", family='binomial',
                                  trControl=train_control,metric='ROC')

model2_stepwise_cl1_small

#Time period is the variable which is removed extra by all subsets regression compared to stepwise selection regression

# AUC 0.935% ####




model1_allsubsets<- glm(SAR ~ NUM_CASES + RS_SC01_Amount_Exceding_250000__HUF_ + RS_SCENARIO + CUSTOMER_FOR_DAYS +  RS_PEP_FLAG +RS_DISTRICT_OF_BUDAPEST + RS_CUSTOMER_REGION,data= smotedatac1, family='binomial')
model2_stepwise<- glm(SAR ~ NUM_CASES + RS_SC01_Amount_Exceding_250000__HUF_ + RS_SCENARIO + CUSTOMER_FOR_DAYS +  RS_PEP_FLAG +RS_DISTRICT_OF_BUDAPEST + RS_CUSTOMER_REGION + RS_TIME_PERIOD ,data= smotedatac1, family='binomial')

#compare two models by anova
anova(model1_allsubsets, model2_stepwise)


#lets check by LRT if it is significant
library(lmtest)
lrtest(model1_allsubsets, model2_stepwise)
AIC(model1_allsubsets, model2_stepwise)

#Not really significant, performance wise (in terms of ROC and Sensitivity) they perform almost same
#still to be sure, let's check if there is correlation of time_period with other variables

correlation_smotec1<-data.frame(cor(smotedatac1[,names(smotedatac1)!='SAR']))
#as time period doesnt have substantial correlation with other variables, it is safer to be included in the model

#Hence model2_stepwise_cl1_small is final logistic model
summary(model2_stepwise)#final

car::vif(model2_stepwise) #multicollinearity not an issue as all VIF's less than 5!

#no separation because coeffs and std errors are all okay!



## Some Comments: ##
## 4. If want to know what models avail in caret use names(getModelInfo())
## 5. So the model that is chosen is the one with mtry=5 when we set our seed to 123, this gives most accurate results in terms of accuracy
## 6. For us we want AUC
## 8. AUC using 10fold cross validation and mtry=4 and AUC is 97% for cluster 1

# Evaluation

# Calculate AUC and rank logistic cluster 1####
AUC_logistic<-model2_stepwise_cl1_small$results[2];AUC_logistic#kfold cv AUC = 92% - pretty good!

########################################################################%
#### Sidenote: Maybe in future I can use this to get improved model 
########################################################################%

# Can i improve model2_stepwise (final model) with higher order terms or interactions (only for numeric predictors)? ####
#Devicance residual vs each continuous predictor
r.dev <- residuals(model2_stepwise, type = "deviance")
par(mfrow=c(1,2))

# Customer for days
plot(smotedatac1$CUSTOMER_FOR_DAYS,r.dev,xlab="CUSTOMER_FOR_DAYS",ylab="Deviance residual", cex.lab=1.5,cex.axis=1.3, ylim=c(-3,3))
loess.dev <- loess(r.dev~smotedatac1$CUSTOMER_FOR_DAYS)
lo.pred <- predict(loess.dev, se=T)
ordercust <- order(smotedatac1$CUSTOMER_FOR_DAYS)
lines(smotedatac1$CUSTOMER_FOR_DAYS[ordercust],lo.pred$fit[ordercust],col="blue",lwd=3)
lines(smotedatac1$CUSTOMER_FOR_DAYS[ordercust],lo.pred$fit[ordercust]+2*lo.pred$s[ordercust], lty=2,col="red")
lines(smotedatac1$CUSTOMER_FOR_DAYS[ordercust],lo.pred$fit[ordercust]-2*lo.pred$s[ordercust], lty=2,col="red")
#maybe quadratic



# NUM_CASES
plot(smotedatac1$NUM_CASES,r.dev,xlab="NUM_CASES",ylab="Deviance residual", cex.lab=1.5,cex.axis=1.3, ylim=c(-3,3))
loess.dev <- loess(r.dev~smotedatac1$NUM_CASES)
lo.pred <- predict(loess.dev, se=T)
orderacc <- order(smotedatac1$NUM_CASES)
lines(smotedatac1$NUM_CASES[orderacc],lo.pred$fit[orderacc],col="blue",lwd=3)
lines(smotedatac1$NUM_CASES[orderacc],lo.pred$fit[orderacc]+2*lo.pred$s[orderacc], lty=2,col="red")
lines(smotedatac1$NUM_CASES[orderacc],lo.pred$fit[orderacc]-2*lo.pred$s[orderacc], lty=2,col="red")
#maybe quadrtatic


# some new models to try but first standardise (use SAR level names unchanged smote data)
n<-dim(smotedatac1)[1]
smotedatac1.stand<-scale(smotedatac1[,names(smotedatac1)!='SAR'],center=TRUE,scale=TRUE)/sqrt(n - 1) #except SAR, standardize others
smotedatac1.stand<-data.frame(smotedatac1.stand)
smotedatac1.stand<-cbind(smotedatac1$SAR,smotedatac1.stand)

model2_stepwise_highordterms_cl1_small<-glm(SAR ~ NUM_CASES + RS_SC01_Amount_Exceding_250000__HUF_ + RS_SCENARIO + CUSTOMER_FOR_DAYS +  RS_PEP_FLAG +RS_DISTRICT_OF_BUDAPEST + RS_CUSTOMER_REGION + RS_TIME_PERIOD
            + I(CUSTOMER_FOR_DAYS^2) +I(NUM_CASES^2),family=binomial, data = smotedatac1.stand)
summary(model2_stepwise_highordterms_cl1_small)
car::vif(model2_stepwise_highordterms_cl1_small)#no of cases inflated, so lets avoid it and see how it goes


model2_stepwise_highordterms2_cl1_small<-glm(SAR ~ NUM_CASES + RS_SC01_Amount_Exceding_250000__HUF_ + RS_SCENARIO + CUSTOMER_FOR_DAYS +  RS_PEP_FLAG +RS_DISTRICT_OF_BUDAPEST + RS_CUSTOMER_REGION + RS_TIME_PERIOD
                                            + I(CUSTOMER_FOR_DAYS^2),family=binomial, data = smotedatac1.stand)
car::vif(model2_stepwise_highordterms2_cl1_small) # still num_cases inflated (>10). So ridge 

summary(model2_stepwise_highordterms2_cl1_small)#very large standard deviations and High AIC! separation?


AIC(model2_stepwise_highordterms_cl1_small)
#AIC for model above is promising and substantially lower than the other models even though more params, therefore I should explore possibility of ridge here
#after trying AUC it performs awfully but may as well try ridge!


## Ridge Regression for model above ##
library(ridge)

#model2_stepwise_highordterms_cl1_small_ridge <- logisticRidge(smotedatac1$SAR ~ NUM_CASES + RS_SC01_Amount_Exceding_250000__HUF_ + RS_SCENARIO + CUSTOMER_FOR_DAYS +  RS_PEP_FLAG +RS_DISTRICT_OF_BUDAPEST + RS_CUSTOMER_REGION + RS_TIME_PERIOD
                                                              #+ I(CUSTOMER_FOR_DAYS^2) ,data = smotedatac1.stand, lambda = "automatic",nPCs = null)
matsmote<-data.matrix(smotedatac1[,-1])
mattrain<-data.matrix(c1_rskm_data_train[,-1])
#glmnet prefers data.matrix over as.matrix! https://stackoverflow.com/questions/8458233/r-glmnet-as-matrix-error-message
library(glmnet)
cv.ridge <- cv.glmnet(matsmote, smotedatac1$SAR, family='binomial', alpha=0, standardize=TRUE, type.measure='auc')
plot(cv.ridge$glmnet.fit, label = T)
coef(cv.ridge, s = "lambda.min")

# ROC Curve Evaluation using training dataset
pred_stepwise = predict(model2_stepwise,c1_rskm_data_train, type = "response")
pred_ridge = predict(cv.ridge,mattrain,s="lambda.min", type = "response")
par(mfrow=c(1,1))
colAUC(cbind(pred_stepwise,pred_ridge), c1_rskm_data_train$SAR, plotROC = TRUE)
#model4 is best now - however its best to use training dataset not smoted data set to evaluate as at least we get little bit of idea 
# of how it would perform out of sample
# return to this if find out how to evaluate ridge model using training data set

#### Continuation ###

## b) Decision Tree - Cluster 1 ####

#train model
set.seed(123);dec_tree1<-train(SAR ~ .,data=smotedatac1,method='rpart',trControl=train_control,tuneLength = 10,metric='ROC')
dec_tree1
#96.89% AUC ####
library(rpart)
par(mfrow=c(1,2))
fancyRpartPlot(dec_tree1$finalModel,uniform=TRUE, main="Best Tree")
# http://rstatistics.net/decision-trees-with-r/ - see this for info

#interpretting
varimp1<-varImp(dec_tree1);varimp1


#Conditional Inference
set.seed(123);cond_tree1<-train(SAR ~ .,data=smotedatac1,method='ctree',trControl=train_control,tuneLength = 10,metric='ROC')#97.1% AUC is best cp=0
par(mfrow=c(1,1))
cond_tree1 #98.12 AUC
cond_tree1$finalModel

## c) Random Forest - Cluster 1 ####

#now to see what params to tune for random forest
paramgrid<-expand.grid(mtry=c(2:10) ,splitrule = "gini"
                       ,min.node.size = 1) #10 is the number of vars we are using so can't be greater than this
# 1 is the default min node size for classfication= hence we go with that!
#train model
set.seed(123)#to reproduce results
ran_forestc1<-train(SAR ~ ., data = smotedatac1,method='ranger',trControl=train_control,tuneGrid=paramgrid,metric = 'ROC')
ran_forestc1# selected only a few features here but takes a long time for this - too long to select features we had before Boruta!
# AUC = 0.9853  ####



## f) XGBoost- Cluster 1 ####

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


xgb_c1 = train(smotedatac1[,-45], smotedatac1$SAR, method = "xgbTree",
               trControl = train_control, tuneGrid = xgb_grid_c1)

xgb_c1$finalModel  #AUC 0.9854840 ####
# max_depth = 6, eta = 0.01, gamma = 0, colsample_bytree =
# 0.75, min_child_weight = 2 and subsample = 0.5.



# Conclusion cluster 1 ####
#according to all models LMT and RF 
# while decision tree (96.88%!) was still equal as above models!


#models for Cluster 2 ####


## a) Logistic Regression - Cluster 2 ####

#need to do this first to get syntactically correct level names for my factor variables (only SAR)
feature.names2=names(smotedatac2)

# for smotedatac2 with 10 variables
for (f in feature.names2) {
  if (class(smotedatac2[[f]])=="factor") {
    levels <- unique(c(smotedatac2[[f]]))
    smotedatac2[[f]] <- factor(smotedatac2[[f]],
                                      labels=make.names(levels))
  }
}


b2 = paste(c2featlist_ordered$Features[1:10], collapse = ' + '); formula_b = paste('SAR ~ ',b,sep = '');b2
# shortcut for variables to be added into model for lm statement!


# We specify the type of evaluation we want to do
train_control <- trainControl(method="cv",number=10,savePredictions=TRUE,summaryFunction=twoClassSummary,classProbs=TRUE)#10 fold cross validation,saves probs and preds predicted by 10 models
#summaryFunction argument makes it possible to choose mtry based on AUC (use metric="ROC")


## Brief Data Exploration ##
# Is scale imprtant? Not for logistic apparently - https://stats.stackexchange.com/questions/48360/is-standardization-needed-before-fitting-logistic-regression
summary(smotedatac2)
sd(smotedatac2$NUM_CASES)
sd(smotedatac2$CUSTOMER_FOR_DAYS)#SD is a lot larger, still not a concern for logistic model building!
sd(smotedatac2$NUM_ACCOUNTS)
sd(smotedatac2$RS_SCENARIO)


# i)All subsets regression ####
allsubset_df_c2_small = cbind(smotedatac2[c2featlist_ordered$Features[1:10]],smotedatac2$SAR)

subset_c2_small = bestglm(Xy = allsubset_df_c2_small,
                          family = binomial,
                          IC = "AIC",                
                          method = "exhaustive")

subset_c2_small$BestModels

#Model 2 suggests removing New account balance and rs_customer region and still AIC good enough (10336.27)
#hence by simplicity, go for model2 from above

#MOdel cluster1 
model_allsubsets_cl2_small <- train(SAR ~NUM_CASES + CUSTOMER_FOR_DAYS + anomaly_scorec2 + NEW_ACCOUNT_BALANCE + NUM_ACCOUNTS + RS_EVENT_MONTH + RS_DISTRICT_OF_BUDAPEST + RS_CUSTOMER_REGION + RS_ORIGINAL_ACCOUNT_CURRENCY + subgroup_28,data= smotedatac2, method = "glm", family='binomial',
                                   trControl=train_control,metric='ROC')
model_cl2_small_glm<- glm(SAR ~ NUM_CASES + CUSTOMER_FOR_DAYS + anomaly_scorec2 + NEW_ACCOUNT_BALANCE + NUM_ACCOUNTS + RS_EVENT_MONTH + RS_DISTRICT_OF_BUDAPEST + RS_CUSTOMER_REGION + RS_ORIGINAL_ACCOUNT_CURRENCY + subgroup_28
                          ,data= smotedatac2,family='binomial')

model_allsubsets_cl2_small #0.65 ROC on 10 fold CV!







# Stepwise selection cluster 2 ####
nullc2 <- glm(SAR ~ 1,family=binomial, data = smotedatac2)
fullc2 <- glm(SAR ~ .,family=binomial, data = smotedatac2)

detach(data)
detach(smotedatac2)
attach(smotedatac2)


step_null_bothc2<-step(nullc2, scope=list(lower=nullc2, upper=fullc2), direction="both")

step_null_forc2<-step(nullc2, scope=list(lower=nullc2, upper=fullc2), direction="forward")


step_null_bothc2$formula


step_null_forc2$formula

# Model extra proposed = adding 'RS_Customerregion' to the model from all subets above
model_stepwise_cl2_small<-train(SAR ~ RS_SC01_Amount_Exceding_250000__HUF_ + NUM_ACCOUNTS + NUM_CASES + 
                  RS_DISTRICT_OF_BUDAPEST + RS_SCENARIO + RS_EVENT_MONTH + 
                  CUSTOMER_FOR_DAYS + RS_RAISED_ON + RS_CUSTOMER_REGION,data=smotedatac2,method="glm", family="binomial",trControl=train_control,metric='ROC')

summary(model_stepwise_cl2_small) #final logistic
#ROC 0.84 on 10 fold CV for logistic ####

#model all subsets is simpler and similar in performance (Both AIC and ROC), hence select it
# Multicolinearity
car::vif(model_cl2_small_glm)
#no issues with correlations - all well below 0.5

## b) Decision Tree - Cluster 2 ####

#train model
set.seed(123);dec_tree2<-train(SAR ~ NUM_CASES + CUSTOMER_FOR_DAYS + anomaly_scorec2 + NEW_ACCOUNT_BALANCE + NUM_ACCOUNTS + RS_EVENT_MONTH + RS_DISTRICT_OF_BUDAPEST + RS_CUSTOMER_REGION + RS_ORIGINAL_ACCOUNT_CURRENCY + subgroup_28,data=smotedatac2,method='rpart',trControl=train_control,tuneLength = 10,metric='ROC')#97.1% AUC is best cp=0
par(mfrow=c(1,1))
dec_tree2 # best dec tree has AUC=0.8041 ####

#Conditional Inference
set.seed(123);cond_tree2<-train(SAR ~ NUM_CASES + CUSTOMER_FOR_DAYS + anomaly_scorec2 + NEW_ACCOUNT_BALANCE + NUM_ACCOUNTS + RS_EVENT_MONTH + RS_DISTRICT_OF_BUDAPEST + RS_CUSTOMER_REGION + RS_ORIGINAL_ACCOUNT_CURRENCY + subgroup_28,data=smotedatac2,method='ctree',trControl=train_control,tuneLength = 10,metric='ROC')#97.1% AUC is best cp=0
par(mfrow=c(1,1))
cond_tree2$finalModel # best dec tree has AUC=0.89 ####
cond_tree2

table(cluster1_small$RS_SC01_Amount_Exceding_250000__HUF_)
table(cluster1_small$RS_RAISED_ON, cluster1_small$SAR)
# 1st Amount > 25K , 2nd Num_cases >=0.5 , 3rd Num_cases>=1.5 , 4th Num_Accounts>=1.5  5th Customer_for_days<401.5...


# http://rstatistics.net/decision-trees-with-r/ - see this for info
dec_tree2
# best dec tree has AUC=0.9058 ####

#interpretting
varimp2<-varImp(dec_tree2);varimp2
#Amount>25k, Raised on = Client, Num cases >0.5 and then >1.5, Scenario,District of Budapest..

## c) Random Forest - Cluster 2 ####

#now to see what params to tune for random forest
paramgrid<-expand.grid(mtry=c(2:10),splitrule = "gini"
                       ,min.node.size = 1) #10 is the number of vars we are using so can't be greater than this
names(smotedatac2)
#train model
set.seed(123)#to reproduce results
ran_forestc2<-train(SAR ~ NUM_CASES + CUSTOMER_FOR_DAYS + anomaly_scorec2 + NEW_ACCOUNT_BALANCE + NUM_ACCOUNTS + RS_EVENT_MONTH + RS_DISTRICT_OF_BUDAPEST + RS_CUSTOMER_REGION + RS_ORIGINAL_ACCOUNT_CURRENCY + subgroup_28, data = smotedatac2,method='ranger',trControl=train_control,tuneGrid=paramgrid,metric = 'ROC')

ran_forestc2
# here we can select only a few predictor vars if we want, we specify we want to evaluate using 10 fold cross validation and tune the mtry param
# AUC is 0.9286% ####


## f) XGBoost - Cluster 2 ####
names(smotedatac2)
xgb_c2 = train(smotedatac2[,-70], smotedatac2$SAR, method = "xgbTree",
               trControl = train_control, tuneGrid = xgb_grid_c1)
xgb_c2

xgb_c2$finalModel  #AUC 92.66%####
#  max_depth = 8, eta = 0.01, gamma = 0.5, colsample_bytree =
# 0.75, min_child_weight = 0 and subsample = 0.5



# Conclusion cluster 2 ####
# Decision Tree (90% AUC) does better than blackbox models (XG Boost and Logit boost)
# RF Stands out again



#models for Cluster 3 ####
## a) Logistic Regression - Cluster 3####


#need to do this first to get syntactically correct level names for my factor variables (only SAR)
feature.names3=names(smotedatac3)

# for smotedatac3 with 10 variables
for (f in feature.names3) {
  if (class(smotedatac3[[f]])=="factor") {
    levels <- unique(c(smotedatac3[[f]]))
    smotedatac3[[f]] <- factor(smotedatac3[[f]],
                                      labels=make.names(levels))
  }
}





b3 = paste(c3featlist_ordered$Features[1:10], collapse = ' + '); formula_b = paste('SAR ~ ',b,sep = '');b3
# shortcut for variables to be added into model for lm statement!


# We specify the type of evaluation we want to do
train_control <- trainControl(method="cv",number=10,savePredictions=TRUE,summaryFunction=twoClassSummary,classProbs=TRUE)#10 fold cross validation,saves probs and preds predicted by 10 models
#summaryFunction argument makes it possible to choose mtry based on AUC (use metric="ROC")


## Brief Data Exploration ##
# Is scale imprtant? Not for logistic apparently - https://stats.stackexchange.com/questions/48360/is-standardization-needed-before-fitting-logistic-regression
summary(smotedatac3)
sd(smotedatac3$NUM_CASES)
sd(smotedatac3$CUSTOMER_FOR_DAYS)#SD is a lot larger, still not a concern for logistic model building!
sd(smotedatac3$NUM_ACCOUNTS)
sd(smotedatac3$RS_SCENARIO)


# i)All subsets regression ####
allsubset_df_c3_small = cbind(smotedatac3[c3featlist_ordered$Features[1:10]],smotedatac3$SAR)

subset_c3_small = bestglm(Xy = allsubset_df_c3_small,
                          family = binomial,
                          IC = "AIC",                
                          method = "exhaustive")

subset_c3_small$BestModels

#Model 1 suggests removing RS_eventmonth and rs_region  and still AIC good enough (10336.27)
#hence by simplicity, go for model1 from above

#MOdel cluster1 
model_allsubsets_cl3_small<- train(SAR ~NUM_CASES + anomaly_scorec3 + NEW_ACCOUNT_BALANCE + CUSTOMER_FOR_DAYS + RS_SCENARIO + RS_REGION + RS_TXN_TYPE + RS_BUSINESS_TYPE2 + subgroup_15 + RS_BASE_CURRENCY_AMOUNT,data= smotedatac3, method = "glm", family='binomial',
                                   trControl=train_control,metric='ROC')

model_allsubsets_cl3_small 
# 0.86 AUC #### 
model_cl3_small_glm<- glm(SAR ~ NUM_CASES + RS_CREDIT_DEBIT_CODE + NEW_ACCOUNT_BALANCE + NUM_ACCOUNTS + RS_EVENT_MONTH + RS_SCENARIO + RS_BUSINESS_TYPE2 + CUSTOMER_FOR_DAYS + RS_REGION + RS_BASE_CURRENCY_AMOUNT,data= smotedatac3,family='binomial')

model_cl3_small_glm #








#model all subsets is simpler and similar in performance (Both AIC and ROC), hence select it
# Multicolinearity
car::vif(model_cl3_small_glm)
#no issues with correlations - all well below 5

## b) Decision Tree - Cluster 3 ####

#train model
set.seed(123)
dec_tree3<-train(SAR ~ NUM_CASES + anomaly_scorec3 + NEW_ACCOUNT_BALANCE + CUSTOMER_FOR_DAYS + RS_SCENARIO + RS_REGION + RS_TXN_TYPE + RS_BUSINESS_TYPE2 + subgroup_15 + RS_BASE_CURRENCY_AMOUNT,data= smotedatac3,method='rpart',trControl=train_control,tuneLength = 10,metric='ROC')#97.1% AUC is best cp=0

dec_tree3 #98.05 AUC ####
dec_tree3$finalModel


table(cluster3_small$RS_BUSINESS_TYPE2)
table(cluster3_small$RS_RAISED_ON, cluster3_small$SAR)
# 1st Num_cases >=0.5 , 2nd B3rd Num_cases>=3.5 , 4th New_Account_Balance>= 1541429  5th Customer_for_days<793.5 and >= 443.46...


# http://rstatistics.net/decision-trees-with-r/ - see this for info
dec_tree3
# best dec tree has  AUC 0.9849729 !!! ####

#interpretting
varimp3<-varImp(dec_tree3);varimp3
#Business type2, Raised on = Client, Num cases >0.5 and then >1.5, Scenario,District of Budapest..

#Conditional Inference

set.seed(123);cond_tree3<-train(SAR ~ NUM_CASES + anomaly_scorec3 + NEW_ACCOUNT_BALANCE + CUSTOMER_FOR_DAYS + RS_SCENARIO + RS_REGION + RS_TXN_TYPE + RS_BUSINESS_TYPE2 + subgroup_15 + RS_BASE_CURRENCY_AMOUNT
                                ,data=smotedatac3,method='ctree',trControl=train_control,tuneLength = 10,metric='ROC')#97.1% AUC is best cp=0
par(mfrow=c(1,1))
cond_tree3$finalModel


# c) Random Forest - Cluster 3 ####

#now to see what params to tune for random forest
paramgrid<-expand.grid(mtry=c(2:10),splitrule = "gini"
                       ,min.node.size = 1) #10 is the number of vars we are using so can't be greater than this

#train model
set.seed(123)#to reproduce results
ran_forestc3<-train(SAR ~ NUM_CASES + anomaly_scorec3 + NEW_ACCOUNT_BALANCE + CUSTOMER_FOR_DAYS + RS_SCENARIO + RS_REGION + RS_TXN_TYPE + RS_BUSINESS_TYPE2 + subgroup_15 + RS_BASE_CURRENCY_AMOUNT, data = smotedatac3,method='ranger',trControl=train_control,tuneGrid=paramgrid,metric = 'ROC')

ran_forestc3
# here we can select only a few predictor vars if we want, we specify we want to evaluate using 10 fold cross validation and tune the mtry param
# AUC is 0.9906 ####


## f) XGBoost - Cluster 3 ####
names(smotedatac3)
xgb_c3 = train(smotedatac3[,-72], smotedatac3$SAR, method = "xgbTree",
               trControl = train_control, tuneGrid = xgb_grid_c1)
xgb_c3

xgb_c3$finalModel  #AUC 99.32%####
#  max_depth = 8, eta = 0.01, gamma = 0, colsample_bytree =
# 0.75, min_child_weight = 0 and subsample = 0.5



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




paste((sprintf("subgroup_%s",seq(1:52))),collapse = ',')#copy paste this list of subgroup features to bind with dataframe

#join these feature vectors into the dataframe!
c2_rskm_data_test<- cbind(c2_rskm_data_test,subgroup_1,subgroup_2,subgroup_3,subgroup_4,subgroup_5,subgroup_6,subgroup_7,subgroup_8,subgroup_9,subgroup_10,subgroup_11,subgroup_12,subgroup_13,subgroup_14,subgroup_15,subgroup_16,subgroup_17,subgroup_18,subgroup_19,subgroup_20,subgroup_21,subgroup_22,subgroup_23,subgroup_24,subgroup_25,subgroup_26,subgroup_27,subgroup_28,subgroup_29,subgroup_30,subgroup_31,subgroup_32,subgroup_33,subgroup_34,subgroup_35,subgroup_36,subgroup_37,subgroup_38,subgroup_39,subgroup_40,subgroup_41,subgroup_42,subgroup_43,subgroup_44,subgroup_45,subgroup_46,subgroup_47,subgroup_48,subgroup_49,subgroup_50,subgroup_51,subgroup_52)


names(c2_rskm_data_test)


#Anomaly detection using Isolation forest for each cluster to identify grouped anomalies ###
#Isolation Forest 
library(IsolationForest)
iforest = IsolationForest::IsolationTrees(c2_rskm_data_test,rFactor=1,nRowSamp =256,ntree = 100) 
# sub sampling size of 256 and ntree=10 recommended by paper from iforest
# reference: https://cs.nju.edu.cn/zhouzh/zhouzh.files/publication/icdm08b.pdf

c2_rskm_data_test$anomaly_scorec2 = AnomalyScore(c2_rskm_data_test,iforest)$outF #score !!!

names(c2_rskm_data_test)



##Cluster 3 ####
## 4b(2) Applying subgroups to test dataset


rules_c3_ridit<- paste0('c3_rskm_data_test_sd$',rules_c3) #add dataframe reference as prefix
rules_c3_ridit#rules of SD for higher SAR level defined for our dataset

#mrefined_small_riditnew<-mrefined_small_ridit
#for (i in 1:length(rules_small_ridit)){
#mrefined_small_riditnew<- mutate(mrefined_small_riditnew, eval(parse(match(rules_small[i],rules_small))) = ifelse(eval(parse(text=paste(rules_small[i]))),1,0))}

#featurize these rules as subgroup features
for (i in 1:length(rules_c3_ridit)){
  assign(paste("subgroup",i,sep="_"),ifelse(eval(parse(text=paste(rules_c3_ridit[i]))),1,0))  
} #creates 60 new vectors which can be used as features




paste((sprintf("subgroup_%s",seq(1:52))),collapse = ',')#copy paste this list of subgroup features to bind with dataframe

#join these feature vectors into the dataframe!
c3_rskm_data_test<- cbind(c3_rskm_data_test,subgroup_1,subgroup_2,subgroup_3,subgroup_4,subgroup_5,subgroup_6,subgroup_7,subgroup_8,subgroup_9,subgroup_10,subgroup_11,subgroup_12,subgroup_13,subgroup_14,subgroup_15,subgroup_16,subgroup_17,subgroup_18,subgroup_19,subgroup_20,subgroup_21,subgroup_22,subgroup_23,subgroup_24,subgroup_25,subgroup_26,subgroup_27,subgroup_28,subgroup_29,subgroup_30,subgroup_31,subgroup_32,subgroup_33,subgroup_34,subgroup_35,subgroup_36,subgroup_37,subgroup_38,subgroup_39,subgroup_40,subgroup_41,subgroup_42,subgroup_43,subgroup_44,subgroup_45,subgroup_46,subgroup_47,subgroup_48,subgroup_49,subgroup_50,subgroup_51,subgroup_52)


names(c3_rskm_data_test)

#Anomaly detection using Isolation forest for each cluster to identify grouped anomalies ###
#Isolation Forest 
library(IsolationForest)
iforest = IsolationForest::IsolationTrees(c3_rskm_data_test,rFactor=1,nRowSamp =256,ntree = 100) 
# sub sampling size of 256 and ntree=10 recommended by paper from iforest
# reference: https://cs.nju.edu.cn/zhouzh/zhouzh.files/publication/icdm08b.pdf

c3_rskm_data_test$anomaly_scorec3 = AnomalyScore(c3_rskm_data_test,iforest)$outF #score !!!

names(c3_rskm_data_test)




#### Prediction on test datasets ####


# Cluster 1 EVALUATION ####
pred_logit_c1 = predict(model_allsubsets_cl1_small, newdata=c1_rskm_data_test, type="prob")
#pred_lmt_c1 = predict(LMT_c1, newdata=c1_rskm_data_test, type="prob")
pred_dec_c1 = predict(dec_tree1, newdata=c1_rskm_data_test, type="prob")
pred_cond_c1 = predict(cond_tree1, newdata=c1_rskm_data_test, type="prob")
pred_rf_c1 = predict(ran_forestc1, newdata=c1_rskm_data_test, type="prob")
#pred_logitboost = predict(logitboost_c1,newdata=c1_rskm_data_test, type="prob")
pred_xgb_c1 = predict(xgb_c1 , newdata = c1_rskm_data_test, type = 'prob')

pred_c1 = cbind(pred_logit_c1[,2], pred_dec_c1[,2], pred_cond_c1[,2],pred_rf_c1[,2],
                 pred_xgb_c1[,2])
pred_c1 = data.frame(pred_c1)
colnames(pred_c1) = cbind('logistic','decision_tree','cond_tree',"random_forest",
                           'XGB')
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
apply(pred_c1,2,Eval_Fun_c1) ####

  #Confusion Matrix ####
pred_logit_c1$pred = factor(ifelse(pred_logit_c1$X2 >= .5, 1, 0))

confusionMatrix(pred_logit_c1[,3],c1_rskm_data_test$SAR,dnn = c("Prediction", "Reference"))

write.xlsx(apply(pred_c1,2,Eval_Fun_c1),file  = "Results_on_test_dataset_cluster1.xlsx")

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

# Skim plot the ultimate result plot for cluster 1 small data####
skimming_c1

#Confusion Matrix cluster1 ####
#confusionMatrix(unlist(pred_c1),c1_rskm_data_test$SAR)
#positive = "1",dnn = c("Prediction", "Reference")


#Conclusion - cluster 1 test ####
# Decision Tree better than RF!!

names(c2_rskm_data_test)

# Cluster 2 test ####
pred_logit_c2 = predict(model_allsubsets_cl2_small, newdata=c2_rskm_data_test, type="prob")
#pred_lmt_c2 = predict(LMT_c2, newdata=c2_rskm_data_test, type="prob")
pred_dec_c2 = predict(dec_tree2, newdata=c2_rskm_data_test, type="prob")
pred_cond_c2 = predict(cond_tree2, newdata=c2_rskm_data_test, type="prob")
pred_rf_c2 = predict(ran_forestc2, newdata=c2_rskm_data_test, type="prob")

#Breakdown plot for RF####
library(breakDown)

?broken

predict.function <- function(ran_forestc2, new_observation) predict(ran_forestc2, new_observation, type="prob")[,2]
predict.function(ran_forestc2, c2_rskm_data_test[1,])
explain_1 <- broken(ran_forestc2, c2_rskm_data_test[1,-1], data = c2_rskm_data_test[-1],
                    predict.function = predict.function, 
                    direction = "down")
explain_1

plot(explain_1) + ggtitle("breakDown plot  (direction=down) for randomForest model")


#direction of exploration= up now
explain_2 <- broken(ran_forestc2, c2_rskm_data_test[1,-1], data = c2_rskm_data_test[-1],
                    predict.function = predict.function, 
                    
                    direction = "up")
explain_2

plot(explain_2) + ggtitle("breakDown plot  (direction=up) for randomForest model")

pred_xgb_c2 = predict(xgb_c2 , newdata = c2_rskm_data_test, type = 'prob')

pred_c2 = cbind(pred_logit_c2[,2], pred_dec_c2[,2], pred_cond_c2[,2],pred_rf_c2[,2],
                pred_logitboost[,2], pred_xgb_c2[,2])
pred_c2 = data.frame(pred_c2)
colnames(pred_c2) = cbind('logistic','decision_tree','cond_tree',"random_forest",
                          "logitboost", 'XGB')
colAUC(pred_c2,c2_rskm_data_test$SAR, plotROC = T)


# That's the frame for calculating PR Sumamry
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
apply(pred_c2,2,Eval_Fun_c2) ####
write.xlsx(apply(pred_c2,2,Eval_Fun_c2),file  = "Results_on_test_dataset_cluster2.xlsx")

# Loop for creating the list of df 
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
  geom_line(data = list_lift_c2[['decision_tree']], aes(color = 'decision_tree')) +
  geom_line(data = list_lift_c2[['cond_tree']], aes(color = 'cond_tree')) +
  geom_line(data = list_lift_c2[['random_forest']], aes( color = 'RF')) +
  geom_line(data = list_lift_c2[['XGB']], aes(color = 'XGB')) +
  labs(title = "Skimming-the-cream Plot", x ="% of SARs", y="% of Alerts") +
  theme(legend.position = c(0.2,0.65),legend.direction = 'vertical') +
  scale_y_continuous(breaks=seq(0,60,20), expand = c(0,0)) +
  scale_x_continuous(breaks=seq(0,100,20)) +
  geom_vline(xintercept = 95, linetype = 'dotdash' ) +
  annotate("text", x = 98.5, y = 59, label = "95%", size = 4) +
  geom_vline(xintercept = 85, linetype = 'dotdash' ) +
  annotate("text", x = 88.5, y = 59, label = "85%", size = 4) +
  coord_cartesian(ylim = c(0, 60))

# Skim plot the ultimate result plot for cluster 2 small data####
skimming_c2

# Cluster 3 test ####


pred_logit_c3 = predict(model_allsubsets_cl3_small, newdata=c3_rskm_data_test, type="prob")
#pred_lmt_c2 = predict(LMT_c2, newdata=c2_rskm_data_test, type="prob")
pred_dec_c3 = predict(dec_tree3, newdata=c3_rskm_data_test, type="prob")
pred_cond_c3 = predict(cond_tree3, newdata=c3_rskm_data_test, type="prob")
pred_rf_c3 = predict(ran_forestc3, newdata=c3_rskm_data_test, type="prob")

pred_rf_c3 = predict(ran_forestc3, newdata=c3_rskm_data_test, type="prob")

pred_xgb_c3 = predict(xgb_c3 , newdata = c3_rskm_data_test, type = 'prob')
#Breakdown plot for RF####
library(breakDown)

?broken

predict.function <- function(ran_forestc3, new_observation) predict(ran_forestc3, new_observation, type="prob")[,2]
predict.function(ran_forestc3, c3_rskm_data_test[1,])
explain_1 <- broken(ran_forestc3, c3_rskm_data_test[1,-1], data = c3_rskm_data_test[-1],
                    predict.function = predict.function, 
                    direction = "down")
explain_1

plot(explain_1) + ggtitle("breakDown plot  (direction=down) for randomForest model")


#direction of exploration= up now
explain_2 <- broken(ran_forestc3, c3_rskm_data_test[1,-1], data = c3_rskm_data_test[-1],
                    predict.function = predict.function, 

                          direction = "up")
explain_2

plot(explain_2) + ggtitle("breakDown plot  (direction=up) for randomForest model")


#pred_lmt_c3 = predict(LMT_c3, newdata=c3_rskm_data_test, type="prob")
pred_dec_c3 = predict(dec_tree3, newdata=c3_rskm_data_test, type="prob")
pred_cond_c3 = predict(cond_tree3, newdata=c3_rskm_data_test, type="prob")
pred_logitboost = predict(logitboost_c3,newdata=c3_rskm_data_test, type="prob")
pred_xgb_c3 = predict(xgb_c3 , newdata = c3_rskm_data_test, type = 'prob')

pred_c3 = cbind(pred_logit_c3[,2], pred_dec_c3[,2], pred_cond_c3[,2],pred_rf_c3[,2],
               pred_xgb_c3[,2])
pred_c3 = data.frame(pred_c3)
colnames(pred_c3) = cbind('logistic','decision_tree','cond_tree',"random_forest",
                           'XGB')
colAUC(pred_c3,c3_rskm_data_test$SAR, plotROC = T)


# That's the frame for calculating PR Sumamry
Eval_Fun_c3 = function(pred_vector, df = c3_rskm_data_test){
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
apply(pred_c3,2,Eval_Fun_c3) ####
write.xlsx(apply(pred_c3,2,Eval_Fun_c3),file  = "Results_on_test_dataset_cluster3.xlsx")

# Loop for creating the list of df 
list_lift_c3 = list()
for (i in names(pred_c3)){
  list_lift_c3[[i]] = data.frame(Class = as.numeric(as.character(c3_rskm_data_test$SAR)), data.frame(pred_c3), cum = 1, perpop = 1)
  list_lift_c3[[i]] = list_lift_c3[[i]][order(list_lift_c3[[i]][[i]], decreasing = T),]
  list_lift_c3[[i]][['cum']] = 100*cumsum(list_lift_c3[[i]][['Class']])/sum(list_lift_c3[[i]][['Class']])
  list_lift_c3[[i]][['perpop']] = (seq(nrow(list_lift_c3[[i]]))/nrow(list_lift_c3[[i]]))*100
}
# Ploting Skimming Plot
skimming_c3 = ggplot(data = NULL, aes(cum, perpop, color = Models)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line(data = list_lift_c3[['logistic']],  aes(color = 'Logistic')) +
  geom_line(data = list_lift_c3[['decision_tree']], aes(color = 'decision_tree')) +
  geom_line(data = list_lift_c3[['cond_tree']], aes(color = 'cond_tree')) +
  geom_line(data = list_lift_c3[['random_forest']], aes( color = 'RF')) +
  geom_line(data = list_lift_c3[['XGB']], aes(color = 'XGB')) +
  labs(title = "Skimming-the-cream Plot", x ="% of SARs", y="% of Alerts") +
  theme(legend.position = c(0.2,0.65),legend.direction = 'vertical') +
  scale_y_continuous(breaks=seq(0,60,20), expand = c(0,0)) +
  scale_x_continuous(breaks=seq(0,100,20)) +
  geom_vline(xintercept = 95, linetype = 'dotdash' ) +
  annotate("text", x = 98.5, y = 59, label = "95%", size = 4) +
  geom_vline(xintercept = 85, linetype = 'dotdash' ) +
  annotate("text", x = 88.5, y = 59, label = "85%", size = 4) +
  coord_cartesian(ylim = c(0, 60))

# Skim plot the ultimate result plot for cluster 2 small data####
skimming_c3





# Conlusion - cluster 3 test ####
# Decision Tree better than RF !!!!


## Conclusion: We choose decision trees for cluster 3 : transactions
# For non-transactions, do we need model building? (discuss)####


par(mfrow= c(3,3))

skimming_c1
skimming_c2
skimming_c3



## Variable Importance - to check with existing scenarios ####
varimp1
varimp2
varimp3
