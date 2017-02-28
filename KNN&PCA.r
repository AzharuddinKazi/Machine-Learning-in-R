#### problem statement: given data about different customers, we have to classify prospective loan takes, i.e, classify loan takes and non-loan takers

### reading from a dataset named Universal Bank

data<-read.csv("E:\\UniversalBank.csv",header=T)
data1=subset(data, select=-c(ID,ZIP.Code))
str(data1)

## segregating the categorical and numeric variables

name<-c("Education","Securities.Account","CD.Account",
        "Online","CreditCard")

name1<-c("Age","Experience","Income","Family",
         "CCAvg","Mortgage","Personal.Loan")

# fetching only the categorical variables

data_cat<-data1[which(colnames(data1) %in% name)]
data_cat<-data.frame(apply(data_cat,2,as.character))

# converting the categorical variables to dummy variables to apply the model

library(dummies)
data_dummy<-as.data.frame(apply(data_cat,2,FUN=dummy))

### we have to rename the colums because above function name the colunms that are difficult to understand

names(data_dummy)<-c("Edu1","Edu2","Edu3","Securities1",
                     "Securities1",
                     "CDAccount1","CDAccount2","Online1",
                     "Online2","CreditCard1","CreditCard2")

# fetching the numerical variables

data_num<-data1[(which(colnames(data1) %in% name1))]

# combining all the data to get a final processed data.
# as data_num also contains the target variable, here we are selecting only the first six colums and we will join the target variable as last column 

data_combined<-cbind(data_num[1:6],data_dummy,data_num[7])

#Dividing the data into train and test- Data Without Standardizing

rows<-seq(1,nrow(data_combined),1)
set.seed(1234)
trainrows<-sample(rows,0.7*nrow(data_combined))
train<-data_combined[trainrows,]
test<-data_combined[-trainrows,]
rm(data_cat,data_num,data_dummy,name,name1)

#KNN on un-standardized data

library(class)
pred=knn(train[1:17],test[1:17], train$Personal.Loan,k=3)
a=table(test$Personal.Loan,pred)
a

##Perfoming PCA on the data
 
 pca<-princomp(train[1:18])
 summary(pca)
 pca_test<-as.data.frame(predict(pca,test[1:18]))
 pca_train<-as.data.frame(predict(pca,train[1:18]))
 
 pred_pca=knn(pca_train[1:4],pca_test[1:4], train$Personal.Loan, k = 3)
 b=table(test$Personal.Loan,pred_pca)
b
###Standardization and its impact

library(vegan)
data2<-decostand(data_combined[1:17],method="standardize")
train1<-data2[trainrows,]
test1<-data2[-trainrows,]
pred3=knn(train1,test1, train$Personal.Loan, k = 3)
c=table(test$Personal.Loan,pred3)
c
 
 pca1<-prcomp(train1)
 summary(pca1)
 pca_test1<-as.data.frame(predict(pca1,test1))
 pca_train1<-as.data.frame(predict(pca1,train1))
 pred4=knn(pca_train1[1:10],pca_test1[1:10], train$Personal.Loan, k = 3)
 a=table(test$Personal.Loan,pred4)
 a


##We have data_combined which is non-standardised
#we split this data into train and test
pca2<-prcomp(train[1:17],scale=T)
pca_test2<-as.data.frame(predict(pca2,test[1:17]))
pca_train2<-as.data.frame(predict(pca2,train[1:17]))
summary(pca2)
pred5=knn(pca_train2[1:10],pca_test2[1:10], train$Personal.Loan, k = 3)
d=table(test$Personal.Loan,pred5)
d #print d

keep=condense(train1, train[,18])
keep

pred=knn(train1[keep, , drop=FALSE], test1, train$Personal.Loan[keep],k=10)
a <- table(pred,test$Personal.Loan)
a #print a