##Introduction
library(dplyr)
library(ggplot2)
library(caret)
library(randomForest)
library(tidyverse)
library(gridExtra)
library(corrplot)
library(corrgram)
#Import Data

#We can download the data from this url: ("http://archive.ics.uci.edu/ml/machine-learning-databases/00356/student.zip")
student_math_performance <- read.table("student-mat.csv",sep= ";", header= T)

#Observation about data set
head(student_math_performance)
dim(student_math_performance)
sum(is.na(student_math_performance))

#Data Cleaning
any(is.na(student_math_performance))
str(student_math_performance)

##Explore and Visualize the data set. In this section we will explore and visully represent the data set. We will also learn more about the relationships between predictors and student performance. 
#The number of male vs female that passed and the exam
math_performance_gender<-student_math_performance%>%
  mutate(pass=ifelse(G3>=10,1,0), fail= ifelse(G3<10,1,0))%>%
  filter(sex=="F"|sex=="M")%>%
  group_by(sex)%>%
  summarise(Pass=sum(pass), 
            Fail=sum(fail))
#Graphical representation: 
math_performance_gender%>%
  ggplot(aes(x=sex,y=Fail))+
  geom_bar(stat="identity")
#We can see that more female students failed than male students, there was a small discrepency between the genders. 

#The graph below shows the *relationship between access to internet and the performance of the students*. It tells us that lack of internet access does impact a students average grade.
student_math_performance %>%
  group_by(internet)%>%
  ggplot(aes(x=G3, fill=internet))+
  geom_density( alpha=0.5)

# Correlation 
#We will use correlation plots to explore the data and see if there are any significant information. We will only do this for the numeric data.
num.cols <- sapply(student_math_performance, is.numeric)
cor.data <- cor(student_math_performance[,num.cols])
corrplot::corrplot(cor.data, method='color')

#There is a clear high correlation between G1,G2 and G3, which makes sense. 
#G1: first period grade, G2: second period grade, G3: final grade. This means that performing students do well each period, and poor performing students do worse for each period.
#Additionally, a high grade value has a negative correlation with past  failure. Mother and Father education levels are positively correlated, meaning the higher level of parental education level the higher grade a student will obtain. 

#We can do the same for the categorical values such as looking at the correlation between alcohol consumption and performance of the students with differences in gender
dmy <- dummyVars("~.", data=student_math_performance)
newdata <- data.frame(predict(dmy, newdata=student_math_performance))
correl1 <-cor(newdata[,c("G1", "G2", "G3","sex.F","sex.M","Walc","Dalc")])
corrplot::corrplot(correl1, method = 'color')

#  G3: Attribute variable
#Now lets look at the variable that we want to predict *G3*.
qplot(G3, data=student_math_performance, geom='histogram', bins=20, fill=..count.., xlab='G3 (final grade)')
#There is quite a number of students that obtain a *0*.In addition, there is quite a high mean occurance. 

#Modelling
#Now that we have a basic understanding of our data set. We will now build 3 different models and and compare the accuracy of respective predictions.                                                                                              number = 50))
#Create test and train sets

library(caTools)
set.seed(101)
sample<- sample.split(student_math_performance$age, SplitRatio = 0.7)
#Training data
train<- subset(student_math_performance, sample==TRUE)
#Test data
test<- subset(student_math_performance, sample==FALSE)
#Train model
model_train <- lm(G3~., train)
summary(model_train)

G3.predictions <- predict(logistic.model, test)
results <- cbind(G3.predictions, test$G3)
colnames(results) <- c('Predicted', 'Real')
results <- as.data.frame(results)

#We do notice that the model does predict some negative test scores, yet our lowest score is *0*. So to improve our model performance we will replace all negative values with *0*. 
rep_zero <- function(x){
  if(x<0){
    return(0)
  } else {
    return(x)
  }
}

# Apply the function to predicted results
results$Predicted <- sapply(results$Predicted, rep_zero)
# Check the range of predicted values
range(results$Predicted)
sse <- sum((results$Predicted - results$Real)^2)
sst <- sum((mean(student_math_performance$G3) - results$Real)^2)
R2 <- 1-sse/sst
RMSE <- sqrt(mean((results$Real - results$Predicted)^2))
R2

# Conclusion: The goal for this report was to predict G3 the final grade for students maths performance based on 33 predictors. As we explored the data set we did find some interesting things, more female students had a high failure rate for the final exam compared to the male students. Access to internet showed a detroriating effect on the performance of the students as their average grades increased compared to those without access to internet. We were able to build a linear regression model,and the model obtained a R2 of *0.7779023*, meaning that our model can explain about 78% variance in our test data. Althought it is too rigid to be useful in general, for some challenges it worked rather well.
#*Recommendations* Alernatively, we could have focused on distinguishing above average students from students in difficulty, and only used a single measure (G1) of a student's performance should be enough to classify them in one of two categories.
