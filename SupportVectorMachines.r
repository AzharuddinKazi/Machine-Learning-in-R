#Loading Data into R: 
bankdata=read.csv("E:\\UniversalBank.csv", header=TRUE, sep=",")  

#Data preparation
#(a) to remove the columns ID & ZIP  
bankdata_1 = subset(bankdata, select=-c(ID, ZIP.Code)) 

#(b) To create dummy variables for the categorical variable ‚??Education‚?ù and add those dummy variables to the original data.  
library("dummies") 
Educations = dummy(bankdata_1$Education) 
str(Educations) 
bankdata_2 = subset(bankdata_1,select=-c(Education)) 
bankdata_3 = cbind(bankdata_2,Educations)                    

#(c) Standardization of Data  
library(vegan) 
bankdata_4 = decostand(bankdata_3,"range") # standardize using 'Range' method

#(d) Separate dataset into train and test  
train = sample(1:5000,3000) # to take a random sample of  60% of the records for train data 
train_bankdata = bankdata_4[train,] 
nrow(train_bankdata) #no of rows 
test = (1:5000) [-train] # to take a random sample of  40% of the records for test data 
test_bankdata = bankdata_4[test,] 
nrow(test_bankdata)  #no of rows 

#Data Summary for the response variable ‚??Personal.Loan‚?ù 
table(bankdata_4$Personal.Loan) 
table(train_bankdata$Personal.Loan)
table(test_bankdata$Personal.Loan)

# Classification using SVM  
#install.packages("e1071") 
str(train_bankdata)
library(e1071)

#Building the model on train data  
x = subset(train_bankdata, select = -Personal.Loan) #remove response variable 
y  = as.factor(train_bankdata$Personal.Loan) 
model  =  svm(x,y, method = "C-classification", kernel = "linear", cost = 10, gamma = 0.1) 
summary(model)

# Applying the model on train data  
pred_train  =  predict(model, x)  
#tabulate how many have been predicted as loan takers(1) and how many  are predicted as not loan takers(0). Compare actual (i.e. "y") vs. predicted (pred_train)  
table(pred_train) 
table(y, pred_train) 
#Find accuracy & recall  
tb_train = table(y,pred_train) 
accuracy_train = sum(diag(table(y,pred_train)))/nrow(train_bankdata) 
recall_train = (tb_train[2,2]/(tb_train[2,2]+tb_train[2,1])) 
recall_train 

# Applying the model on test data
a  = subset(test_bankdata, select = -Personal.Loan) #remove response variable 
b  = as.factor(test_bankdata$Personal.Loan) 
pred_test = predict(model, a) 

#Tabulate how many have been predicted as loan takers(1) and how many are predicted as not loan #takers(0). Compare actual (i.e. "b") vs. predicted (pred_test) 
table(pred_test) 
table(b,pred_test) #actual (i.e.) is on left and predicted shown on top 
tb_test <- table(b,pred_test) 
accuracy_test = sum(diag(table(b,pred_test)))/nrow(test_bankdata) 
(tb_test[1,1]+tb_test[1,2]+tb_test[2,1]+tb_test[2,2]) 
recall_test = (tb_test[2,2]/(tb_test[2,2]+tb_test[2,1])) 
recall_test 

##Tuning the parameters
tuneResult <- tune(svm, train.x = x, train.y = y, ranges = list(gamma = 10^(-6:-1), cost = 2^(2:3)))
print(tuneResult)
tunedModel <- tuneResult$best.model 
tunedModelY <- predict(tunedModel, as.matrix(x)) 
Conf <- table(y, tunedModelY)

###############KSVM############## 
library(kernlab) 
names(train_bankdata) 
kernmodel <- ksvm(as.matrix(train_bankdata[,-7]),train_bankdata[,7],type='C-svc',kernel="rbfdot",kpar=list(sigma=1),C=10) 
kpred<- predict(kernmodel,test_bankdata[-7]) 
require(Metrics)
kRMSE<- rmse(test_bankdata[,7], kpred) 
kRMSE