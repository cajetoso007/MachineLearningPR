library(caret)
library(randomForest) #Random forest for classification and regression
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
library(rpart) # Regressive Partitioning and Regression trees
library(rpart.plot) # Decision Tree plot


trainingdata<- read.csv("C:/Users/Manuel/Desktop/Coursera/MachineLearning/pml-training.csv", na.strings = c("NA",""))


testingdata<- read.csv("C:/Users/Manuel/Desktop/Coursera/MachineLearning/pml-testing.csv", na.strings= c("NA",""))



#Need to get rid of all the NA values in the datasets
trainingdata<- trainingdata[,colSums(is.na(trainingdata))==0]
testingdata<-testingdata[,colSums(is.na(testingdata))==0]

#removing the stuff we dont need:
trainingdata   <-trainingdata[,-c(1:7)]
testingdata <-testingdata[,-c(1:7)]


head(trainingdata)
head(testingdata)

summary(trainingdata)
summary(testingdata)


#Predicting the data set using "classe"

inTrain<-createDataPartition(y=trainingdata$classe, p = 0.75,list= FALSE)
training<- trainingdata[inTrain,]
testing <- trainingdata[-inTrain,]


#graphing predicted training values
plot(training$classe,col="orange", main=" Classes in the Training data",xlab="levels",ylab="Frequency")



#Using a DecisionTree:
model1<- rpart( classe ~ .,  data= training, method = "class")

pred<- predict(model1, testing, type ="class")

confusionMatrix(pred, testing$classe)

###OUTPUT:
#Confusion Matrix and Statistics

#Reference
#Prediction    A    B    C    D    E
#A            1237  229   55  105   67
#B              23  352   74   28   83
#C              32  132  601   82   84
#D              95  211   97  534   200
#E              8   25   28   55    467

#Overall Statistics

#Accuracy : 0.6507         
#95% CI : (0.6372, 0.664)
#No Information Rate : 0.2845         
#P-Value [Acc > NIR] : < 2.2e-16      

#Kappa : 0.556          
#Mcnemar's Test P-Value : < 2.2e-16      

#Statistics by Class:

#                     Class: A Class: B Class: C Class: D Class: E
#Sensitivity            0.8867  0.37092   0.7029   0.6642  0.51831
#Specificity            0.8700  0.94741   0.9185   0.8529  0.97102
#Pos Pred Value         0.7307  0.62857   0.6455   0.4697  0.80103
#Neg Pred Value         0.9508  0.86257   0.9361   0.9283  0.89956
#Prevalence             0.2845  0.19352   0.1743   0.1639  0.18373
#Detection Rate         0.2522  0.07178   0.1226   0.1089  0.09523
#Detection Prevalence   0.3452  0.11419   0.1898   0.2319  0.11888
#Balanced Accuracy      0.8784  0.65916   0.8107   0.7586  0.74467

#plotting the decision tree"
library(rpart)
library(rpart.plot)
rpart.plot(DT,main="Tree",extra=102, under=TRUE,faclen=0)



#METHOD 2: RANDOM FORESTS
library(randomForest)
rf <- randomForest(classe~., data=training,method="class")

#Predicting the dataset

pred2<- predict(rf, testing, type = "class")


confusionMatrix(pred2, testing$classe)

###OUTPUT:

##Confusion Matrix and Statistics

#Reference
#Prediction    A    B    C    D    E
#A          1394    6    0    0    0
#B            1   942   12    0    3
#C            0     1  843   13    0
#D            0     0    0  790    2
#E            0     0    0    1  896

#Overall Statistics

#Accuracy : 0.992           
##95% CI : (0.9891, 0.9943)
#No Information Rate : 0.2845          
#P-Value [Acc > NIR] : < 2.2e-16       

#Kappa : 0.9899          
#Mcnemar's Test P-Value : NA              

#Statistics by Class:

#                     Class: A Class: B Class: C
#Sensitivity            0.9993   0.9926   0.9860
#Specificity            0.9983   0.9960   0.9965
#Pos Pred Value         0.9957   0.9833   0.9837
#Neg Pred Value         0.9997   0.9982   0.9970
#Prevalence             0.2845   0.1935   0.1743
#Detection Rate         0.2843   0.1921   0.1719
#Detection Prevalence   0.2855   0.1954   0.1748
#Balanced Accuracy      0.9988   0.9943   0.9913
#                       Class: D Class: E
#Sensitivity            0.9826   0.9945
#Specificity            0.9995   0.9998
#Pos Pred Value         0.9975   0.9989
#Neg Pred Value         0.9966   0.9988
#Prevalence             0.1639   0.1837
#Detection Rate         0.1611   0.1827
#Detection Prevalence   0.1615   0.1829
#Balanced Accuracy      0.9910   0.9971


#Last Quiz: Using the testing data set

predict(rf,testingdata,type="class")
#1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
#B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
#Levels: A B C D E

#CROSS VALIDATION : ANALYSIS
#Cross Validation-Caret using BOOTSTRAP
# define training control
train_control <- trainControl(method="boot", number=100)
# train the model
model2 <- train(classe ~., data=trainingdata, trControl=train_control, method="rpart")
print(model2)

#KFOld Cross Validation
library(caret)
# define training control
train_control <- trainControl(method="cv", number=10)
# fix the parameters of the algorithm
grid <- expand.grid(.fL=c(0), .usekernel=c(FALSE), .adjust= c(FALSE))
# train the model
model5 <- train(classe~., data=trainingdata, trControl=train_control, method="rpart", tuneGrid=grid)
print(model5)