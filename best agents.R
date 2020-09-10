setwd("D:/BA classes")
getwd()
library(data.table)#used for reading and manipulation of data
library(dplyr)#used for data manipulation and joining of data
library(ggplot2)#used for plotting
library(caret)#used for modeling
library(corrplot)#used for correlation
library(xgboost)#used for xgboost model
library(cowplot)#combing mutiple plots

train = fread("Train.csv")
test = fread("Test.csv")

dim(train)
dim(test)

str(train)
str(test)
View(train)

test[,Business_Sourced := NA]  
comb = rbind(train,test)#combing the train and test to to perform bivariate analysis
dim(comb)
View(comb)


#target variable 
ggplot(train)+geom_bar(aes(train$Business_Sourced),binwidth = 100,fill="green")+xlab("Business_Sourced")


##categorial variable barplot

 ggplot(comb %>% group_by(Applicant_Occupation) %>% summarise(Count = n())) +
  geom_bar(aes(Applicant_Occupation,Count),stat = "identity",fill="red") + xlab("") +
  geom_label(aes(Applicant_Occupation,Count,label= Count),vjust=0.5) + theme(axis.text.x= element_text(angle = 45,hjust = 1))+
  ggtitle("Applicant_Occupation")

 ggplot(comb %>% group_by(Applicant_Gender_num) %>% summarise(Count = n())) +
  geom_bar(aes(Applicant_Gender_num,Count),stat = "identity",fill="red") + xlab("") +
  geom_label(aes(Applicant_Gender_num,Count,label= Count),vjust=0.5) + theme(axis.text.x= element_text(angle = 45,hjust = 1))+
  ggtitle("Applicant_Gender_num")


ggplot(comb %>% group_by(Applicant_Marital_Status_num) %>% summarise(Count = n())) +
  geom_bar(aes(Applicant_Marital_Status_num ,Count),stat = "identity",fill="red") + xlab("") +
  geom_label(aes(Applicant_Marital_Status_num,Count,label= Count),vjust=0.5) + theme(axis.text.x= element_text(angle = 45,hjust = 1))+
  ggtitle("Applicant_Marital_Status_num")

 ggplot(comb %>% group_by(Applicant_Qualification ) %>% summarise(Count = n())) +
  geom_bar(aes(Applicant_Qualification  ,Count),stat = "identity",fill="red") + xlab("") +
  geom_label(aes(Applicant_Qualification ,Count,label= Count),vjust=0.5) + theme(axis.text.x= element_text(angle = 45,hjust = 1))+
  ggtitle("Applicant_Qualification")

##MAnger plot
ggplot(comb %>% group_by(Manager_Joining_Designation) %>% summarise(Count = n())) +
  geom_bar(aes(Manager_Joining_Designation ,Count),stat = "identity",fill="red") + xlab("") +
  geom_label(aes(Manager_Joining_Designation,Count,label= Count),vjust=0.5) + theme(axis.text.x= element_text(angle = 45,hjust = 1))+
  ggtitle("Manager_Joining_Designation")

ggplot(comb %>% group_by(Manager_Current_Designation) %>% summarise(Count = n())) +
  geom_bar(aes(Manager_Current_Designation ,Count),stat = "identity",fill="red") + xlab("") +
  geom_label(aes(Manager_Current_Designation,Count,label= Count),vjust=0.5) + theme(axis.text.x= element_text(angle = 45,hjust = 1))+
  ggtitle("Manager_Current_Designation")

ggplot(comb %>% group_by(Manager_Grade) %>% summarise(Count = n())) +
  geom_bar(aes(Manager_Grade ,Count),stat = "identity",fill="red") + xlab("") +
  geom_label(aes(Manager_Grade,Count,label= Count),vjust=0.5) + theme(axis.text.x= element_text(angle = 45,hjust = 1))+
  ggtitle("Manager_Grade")

ggplot(comb %>% group_by(Manager_Status) %>% summarise(Count = n())) +
  geom_bar(aes(Manager_Status ,Count),stat = "identity",fill="red") + xlab("") +
  geom_label(aes(Manager_Status,Count,label= Count),vjust=0.5) + theme(axis.text.x= element_text(angle = 45,hjust = 1))+
  ggtitle("Manager_Status")

ggplot(comb %>% group_by(Manager_Gender) %>% summarise(Count = n())) +
  geom_bar(aes(Manager_Gender ,Count),stat = "identity",fill="red") + xlab("") +
  geom_label(aes(Manager_Gender,Count,label= Count),vjust=0.5) + theme(axis.text.x= element_text(angle = 45,hjust = 1))+
  ggtitle("Manager_Gender")

ggplot(comb %>% group_by( Manager_Num_Application ) %>% summarise(Count = n())) +
  geom_bar(aes( Manager_Num_Application ,Count),stat = "identity",fill="red") + xlab("") +
  geom_label(aes( Manager_Num_Application,Count,label= Count),vjust=0.5) + theme(axis.text.x= element_text(angle = 45,hjust = 1))+
  ggtitle(" Manager_Num_Application ")

ggplot(comb %>% group_by( Manager_Num_Coded) %>% summarise(Count = n())) +
  geom_bar(aes( Manager_Num_Coded,Count),stat = "identity",fill="red") + xlab("") +
  geom_label(aes( Manager_Num_Coded ,Count,label= Count),vjust=0.5) + theme(axis.text.x= element_text(angle = 45,hjust = 1))+
  ggtitle(" Manager_Num_Coded  ")


###numerical varial histogram
ggplot(comb)+geom_histogram(aes(Applicant_BirthDate  ),binwidth = 1,fill="blue")
ggplot(comb)+geom_histogram(aes(Application_Receipt_Date  ),binwidth = 1,fill="blue")

ggplot(comb)+geom_histogram(aes(Manager_Business),binwidth = 0.005,fill="blue")
ggplot(comb)+geom_histogram(aes(Manager_Num_Products ),binwidth = 1,fill="blue")

ggplot(comb)+geom_histogram(aes(Manager_Business2),binwidth = 0.005,fill="blue")
ggplot(comb)+geom_histogram(aes(Manager_Num_Products2),binwidth = 1,fill="blue")




#coverting date to ages (age column)
comb$Application_Receipt_Date <- as.Date(comb$Application_Receipt_Date, "%m/%d/%Y")
comb$applicant_DOB <- as.Date(comb$Applicant_BirthDate, "%m/%d/%Y")
comb$manger_DOJ2 <- as.Date(comb$Manager_DOJ, "%m/%d/%Y")
comb$manger_DOB2 <- as.Date(comb$Manager_DoB, "%m/%d/%Y")
library(lubridate)
comb$applicant_age<-as.numeric(difftime(as.Date("2009-01-01"), as.Date(comb$applicant_DOB), unit="weeks"))/52.25
comb$manger_DateofJoin<-as.numeric(difftime(as.Date("2009-01-01"), as.Date(comb$manger_DOJ2), unit="weeks"))/52.25
comb$manger_DoB<-as.numeric(difftime(as.Date("2009-01-01"), as.Date(comb$manger_DOB2), unit="weeks"))/52.25

comb$applicant_DOB<-comb$manger_DOJ2<-comb$manger_DOB2<-NULL#deleting the unwanted column
comb$Applicant_BirthDate<-comb$Manager_DOJ<-comb$Manager_DoB<-NULL


##Dummy variables
comb[,Applicant_Gender_num := ifelse(Applicant_Gender == "M",1, ifelse(Applicant_Gender == "F",0,NA))]
comb[,Applicant_Marital_Status_num := ifelse(Applicant_Marital_Status == "M",0, ifelse(Applicant_Marital_Status == "S",1,ifelse(Applicant_Marital_Status == "D",2,ifelse(Applicant_Marital_Status == "W",3,NA))))]
comb[,Applicant_Occupation_num  := ifelse(Applicant_Occupation == "Business",0, 
                                          ifelse(Applicant_Occupation  == "Others",1,
                                                 ifelse(Applicant_Occupation  == "Salaried",2,
                                                        ifelse(Applicant_Marital_Status == "SelfEmployed",3,
                                                               ifelse(Applicant_Marital_Status == "Student",4,NA)))))]




comb[,Applicant_Qualification_num := ifelse(Applicant_Qualification == "Class XII",0, 
                                            ifelse(Applicant_Qualification == "Graduate",1,
                                                   ifelse(Applicant_Qualification == "class X",2,
                                                          ifelse(Applicant_Qualification == "Others",3,4))))]



comb[,Manager_Joining_Designation_num := ifelse(Manager_Joining_Designation == "Level 1",0, 
                                                ifelse(Manager_Joining_Designation == "Level 2",1,
                                                       ifelse(Manager_Joining_Designation == "Level 3",2,
                                                              ifelse(Manager_Joining_Designation == "Level 4",3,
                                                                     ifelse(Manager_Joining_Designation == "Other",4,5)))))]


comb[,Manager_Current_Designation_num := ifelse(Manager_Current_Designation == "Level 1",0, 
                                                ifelse(Manager_Current_Designation == "Level 2",1,
                                                       ifelse(Manager_Current_Designation == "Level 3",2,
                                                              ifelse(Manager_Current_Designation == "Level 4",3,
                                                                     ifelse(Manager_Current_Designation == "Level 5",4,NA)))))]


comb[,Manager_Status_num := ifelse(Manager_Status == "Confirmation",1, ifelse(Manager_Status == "Probation",0,NA))]
comb[,Manager_Gender_num := ifelse(Manager_Gender == "M",1, ifelse(Manager_Gender == "F",0,NA))]


##delelting the unwanted variables
comb$Applicant_City_PIN<-comb$Applicant_Gender<-comb$Applicant_Marital_Status<-comb$Applicant_Occupation<-
  comb$Applicant_Qualification<-comb$Manager_Joining_Designation<-comb$Manager_Current_Designation<-comb$Manager_Status<-comb$Manager_Gender<-NULL


#######splitting of data into train and test
train = comb[1:nrow(train)] 
test = comb[(nrow(train) + 1):nrow(comb)]
test[,Business_Sourced := NULL]#removing the na values for test

str(train)
####building the model
##XGBoost model
# Transform the two data sets into xgb.Matrix
param_list = list(eta =0.01, gamma=1,max_depth=6,subsample=0.8,colsample_bytree=0.5)
dtrain = xgb.DMatrix(data = as.matrix(train[,-c("ID","Business_Sourced","Application_Receipt_Date")]),label = train$Business_Sourced) 
dtest = xgb.DMatrix(data = as.matrix(test[,-c("ID","Application_Receipt_Date")]))
#Cross Validation to found the nrounds for the model
set.seed(111)
xgbcv = xgb.cv(params = param_list , data = dtrain ,nrounds = 1000 , nfold = 5 ,print_every_n = 10,
               early_stopping_rounds = 30 , maximize = F)
#train the model
xgb_model = xgb.train(data = dtrain , params = param_list , nrounds = 529)




##predicting model
submission = fread("sample_submission.csv")

#XGBoostfile
submission$Business_Sourced<-predict(xgb_model,dtest)
write.csv(submission,"Xgboost.csv",row.names = F)



