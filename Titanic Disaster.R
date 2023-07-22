setwd("D:/Study/Semester 8/Distributed Computing/Labs/Labs/Assignment/Titanic Disaster")
rm(list = ls())
test<-read.csv("test.csv",sep = ",")
train<-read.csv("train.csv",sep = ",")
Data<- rbind(train[,0:11],test)
summary(Data)
str(Data)
sum(is.na(Data))
#################################################### Data Preprossesing And Visualization #################################################### 
# Get the names of columns that have null values
names(Data)[colSums(is.na(Data)) > 0]
med<-median(Data$Age,na.rm = TRUE)
mean<-mean(Data$Fare,na.rm = TRUE)
mean
#replace null values with med
Data$Age<-ifelse(is.na(Data$Age), med, Data$Age)
#replace null values with mean
Data$Fare<-ifelse(is.na(Data$Fare), mean, Data$Fare)
sum(is.na(Data))

# Data Visualization
library(ggplot2)
library(scales)
Data_vis<- cbind(Data[0:891,],train$Survived)
names(Data_vis)[12]<-"Survived"

# bar plot for survival rate by gender
ggplot(Data_vis, aes(x = Sex, fill = factor(Survived))) +
  geom_bar() +
  scale_fill_discrete( name = "Survived",labels = c("No", "Yes")) +
  labs(title = "Survival Rate by Gender", x = "", y = "Number of Passengers")

# histogram for passenger age
ggplot(Data_vis, aes(x = Pclass, fill = factor(Survived))) + 
  geom_bar() + 
  labs(x = "Passenger Class", y = "Count", title = "Survivors by Passenger Class") +
  scale_fill_discrete(name = "Survived", labels = c("No", "Yes"))
ggplot(Data_vis, aes(x = Age, fill = factor(Survived))) +
  geom_histogram(binwidth = 5) +
  scale_fill_discrete( name = "Survived",labels = c("No", "Yes")) +
  labs(title = "Passenger Age Distribution", x = "Age", y = "Number of Passengers") +
  scale_x_continuous(breaks = seq(0, 80, 10), limits = c(0, 80)) +
  scale_y_continuous(labels = comma)

# density plot for passenger fare
ggplot(Data_vis, aes(x = Fare, fill = factor(Survived))) +
  geom_density(alpha = 0.5) +
  scale_fill_discrete(name = "Survived",
                      labels = c("No", "Yes")) +
  labs(title = "Passenger Fare Distribution", x = "Fare", y = "Density") +
  scale_x_continuous(labels = dollar_format(prefix = "$"), limits = c(0, 600))

# Encoding
# Convert categorical columns to a factor
Data$Name <- factor(Data$Name)
Data$Sex <- factor(Data$Sex)
Data$take.off <- factor(Data$take.off)
Data$Cabin <- factor(Data$Cabin)
Data$Ticket <- factor(Data$Ticket)
#Convert the factor to a numeric representation
Data$Name <- as.numeric(Data$Name)
Data$Sex <- as.numeric(Data$Sex)
Data$take.off <- as.numeric(Data$take.off)
Data$Cabin <- as.numeric(Data$Cabin)
Data$Ticket <- as.numeric(Data$Ticket)

#Normalization
Data<-as.data.frame(scale(Data))

#Split the Data
ntrain<- Data[0:891,]
ntest<-Data[892:1309,]


#add Survived Column to train data
ntrain$Survived<- train$Survived

#Extract Features
# Compute the correlation matrix
cor_matrix <- cor(ntrain)
# Visualize the correlation matrix
library("corrplot")
corrplot(cor_matrix, method = "circle")
#Compute the correlation between Survived class and other classes
corr<-cor(ntrain$Survived,ntrain[,c("PassengerId","Pclass","Name","Sex","Age","SibSp","Parch","Ticket","Fare","Cabin","take.off")])
vec <- unlist(as.data.frame(corr))
#plot the correlation between Survived class and other classes
barplot(vec, names.arg = names(vec), xlab = "", ylab = "Survived", main = "Data Correlation",las=2)
# Drop the Features with low correlation
train_Data<-subset(ntrain,select = c(Pclass,Sex,Ticket,Fare,Cabin,take.off,Survived))
ntest<-subset(ntest,select = c(Pclass,Sex,Ticket,Fare,Cabin,take.off))
#################################################### Training Ans Testing #################################################### 
#install.packages("tidyverse")
set.seed(123)
#Split training Data to 80% training And 20% validation
train_index <- sample(nrow(train_Data), 0.8 * nrow(train_Data))
train_data <- train_Data[train_index, ]
valid_data <- train_Data[-train_index,0:6 ]

# Train Using Different Models
#Naive Bayes Model
#library(e1071)
#model <- naiveBayes(Survived ~.,data = train_data)

##############################################################
#Logistic Regression Model
#library(caret)
#model<-train(Survived ~., data = train_data,method = "glm",family = "binomial")
##############################################################
#Decision tree Model
#library(rpart)
#model <- rpart(Survived ~.,data = train_data)
##############################################################
#SVM Model
#model <- svm(Survived ~.,data = train_data)
##############################################################
#randomForest Model
library(randomForest)
model <- randomForest(Survived ~.,data = train_data, ntree = 500,mtry = 3,nodesize = 1,importance = TRUE,na.action = na.pass)
##############################################################
#compute Training Accuracy
train_pred <- predict(model, newdata = train_data[,0:6],ntree = 500)
train_pred<-ifelse(train_pred < 0.5, 0, 1)
accuracy <- mean(train_pred == train_Data[train_index,7])
cat("Training accuracy:", round(accuracy * 100, 2), "%\n")

#compute Validation Accuracy
valid_pred <- predict(model, newdata = valid_data,ntree = 500)
valid_pred<-ifelse(valid_pred < 0.5, 0, 1)
accuracy <- mean(valid_pred == train_Data[-train_index,7])
cat("Validation accuracy:", round(accuracy * 100, 2), "%\n")
dim(ntest)
dim(train_data)
# make predictions on the test set
results <- predict(model, newdata = ntest)
results <- ifelse(results < 0.5, 0, 1)
passenger_id<-test$PassengerId
nresults<-cbind(passenger_id,results)
colnames(nresults)<-c("PassengerId","Survived")
#Save Testing Results
write.csv(nresults, file = "results.csv", row.names = FALSE)
#read.csv("results.csv",sep = ",")

