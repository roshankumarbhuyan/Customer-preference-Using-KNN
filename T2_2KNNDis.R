
#=================== SetWorking Dierctory==================================

setwd("C:\\Users\\ROS\\Documents\\R Tutorial Data\\R Tutorial Data Sets\\ROSDA\\Module2")

#=================== Loading libraries====================================

library(readxl)
library(caret)  
#install.packages("arules")
library(arules)
#====================Data import==========================================

mydata <- read_excel("Survey_Key_and_Complete_Responses_excel.xlsx",sheet = 2)
my_data_raw <- read_excel("Survey_Key_and_Complete_Responses_excel.xlsx",sheet = 2)

#===================Preprocessing==========================================
range(mydata$age)
range(mydata$elevel)
range(mydata$car)
range(mydata$zipcode)
range(mydata$credit) 
summary(mydata$credit)
#mydata$age <- discretize(mydata$age,method = "interval",categories = 3,
                       labels = NULL,ordered = FALSE,onlycuts = FALSE) 
#mydata$salary <- discretize(mydata$salary,method = "interval",categories = 3,
                         labels = NULL,ordered = FALSE,onlycuts = FALSE) 
mydata$elevel <- factor(mydata$elevel) 
mydata$brand <- factor(mydata$brand, levels = c(0,1), labels = c("Acer", "Sony"))
mydata$car <- factor(mydata$car)
mydata$zipcode <- factor(mydata$zipcode)
head(mydata,n=5)
str(mydata)

#===================== Vizualization======================================
plot(mydata$credit,mydata$salary)
plot(mydata$elevel,mydata$salary)
str(trainSet) #structure of the dataset
#levels(mydata$elevel) <- c("School","High School","College","University","Masters")
ggplot(mydata,aes(x =salary,y=age, color = brand))+ 
  geom_jitter(alpha=0.93)]
  #facet_grid(.~elevel)+
 # stat_smooth(method = "lm" ,se=F,col="blue") 

#=======================data slicing method 2============================
# slicing using Caret keeping the proportions of the class label the
indexes <- createDataPartition(y=mydata$age, times=1,p=0.7,list=FALSE)
trainSet<- mydata[indexes,]
testSet<-mydata[-indexes,]
prop.table(table(trainSet$age)) * 100
prop.table(table(testSet$age)) * 100

#===========================model======================================== 
set.seed(234)
ctrl <- trainControl(method="repeatedcv",number=10,repeats = 3) 
knnFit <- train(brand~ ., data = trainSet, method = "knn",trControl = ctrl, 
                preProcess = c("center","scale"), tuneLength = 10)

#===========================test=============================
knnpred = predict(knnFit, newdata=testSet)
head(knnpred)

#===========================Output of kNN fit=================
knnFit
plot(knnFit)
confusionMatrix(knnPredict, testing$brand )

ggplot(testSet,aes(x =salary,y=age, color = brand))+ 
  geom_jitter(alpha=0.93)
