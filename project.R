
mushrooms = read.csv("C:/Users/Talha Mir/Desktop/Course Slides/Semester 2/Data Mining/Project/mushrooms.csv")
summary(mushrooms)
nrow(mushrooms)
ncol(mushrooms)

str(mushrooms)

sum(is.na(mushrooms))

mushrooms$veil.type<-NULL

mushrooms <- mushrooms[-which(mushrooms$stalk.root == "?"), ]


as.numeric(mushrooms$class)



# RandomForest
library("ROCR")
library(randomForest)

sample <- sample.int(nrow(mushrooms), floor(.80*nrow(mushrooms)), replace = F)
mushroomsDatatrain <- mushrooms[sample, ]
mushroomsDatatest <- mushrooms[-sample, ]

mushrooms_randomForest <- randomForest(class~., data=mushroomsDatatrain)

predictions <- predict(mushrooms_randomForest, mushroomsDatatest)

testDataValues = as.numeric(mushroomsDatatest$class)-1
predictedValues = as.numeric(predictions)-1

rFPrecision <- sum(predictedValues & testDataValues) / sum(predictedValues)
rFRecall <- sum(predictedValues & testDataValues) / sum(testDataValues)

# NaiveBayes
library(e1071)
mushroom_naive_bayes = naiveBayes(class~., data=mushroomsDatatrain)
predictions <- predict(mushroom_naive_bayes, mushroomsDatatest)

predictions <- predict(mushroom_naive_bayes, mushroomsDatatest)

testDataValues = as.numeric(mushroomsDatatest$class)-1
predictedValues = as.numeric(predictions)-1

nBPrecision <- sum(predictedValues & testDataValues) / sum(predictedValues)
nBRecall <- sum(predictedValues & testDataValues) / sum(testDataValues)


# ID3
# https://www.kaggle.com/ikalats/smell-the-mushroom-before-tasting-it


library(RWeka)


fit <- J48(class~., data=mushroomsDatatrain)
predictions <- predict(fit, mushroomsDatatest)

testDataValues = as.numeric(mushroomsDatatest$class)-1
predictedValues = as.numeric(predictions)-1

id3Precision <- sum(predictedValues & testDataValues) / sum(predictedValues)
id3Recall <- sum(predictedValues & testDataValues) / sum(testDataValues)

#visualization of the obtained tree
library(partykit)
plot(as.party(fit))

InfoGainAttributeEval(class ~ . , data = mushroomsDatatrain)


library(ggplot2)
p <- ggplot(mushroomsDatatrain, aes(x=class,y=odor,color=class), alpha=0.3) + geom_jitter()
p





predictions <- predict(mushrooms_randomForest, mushroomsDatatest, type = 'prob')


pred <- prediction(as.numeric(predictions[,2]), as.numeric(mushroomsDatatest$class)-1)
perf <- performance(pred,"tpr","fpr")

plot(perf)
abline(a=0, b= 1)

auc <- performance(pred,"auc")
auc <- unlist(slot(auc, "y.values"))

importance(mushrooms_randomForest)
dataimp <- varImpPlot(mushrooms_randomForest, main = "Importance of each variable")

## PCA


biplot(prcomp(mushrooms), choices = c(1,2))



jobsData = read.csv("C:/Users/Talha Mir/Desktop/Course Slides/Semester 2/Data Mining/Project/filtered_data.csv", header = TRUE)
head(jobsData)
nrow(jobsData)
ncol(jobsData)
str(jobsData)


jobsData = subset(jobsData, jobsData$applied==1 | jobsData$disliked == 1 | jobsData$liked == 1)


jobsData$applied = as.factor(jobsData$applied)

jobsData = jobsData[,-1:-2]
jobsData = jobsData[,-21:-22]


# RandomForest
library("ROCR")
library(randomForest)

sample <- sample.int(nrow(jobsData), floor(.80*nrow(jobsData)), replace = F)
jobsDatatrain <- jobsData[sample, ]
jobsDatatest <- jobsData[-sample, ]

jobs_randomForest <- randomForest(applied~., data=jobsDatatrain)

predictions <- predict(jobs_randomForest, jobsDatatest)

testDataValues = as.numeric(jobsDatatest$applied)-1
predictedValues = as.numeric(predictions)-1

rFPrecision <- sum(predictedValues & testDataValues) / sum(predictedValues)
rFRecall <- sum(predictedValues & testDataValues) / sum(testDataValues)


# NaiveBayes
library(e1071)
jobs_naive_bayes = naiveBayes(applied~., data=jobsDatatrain)
predictions <- predict(jobs_naive_bayes, jobsDatatest)


testDataValues = as.numeric(jobsDatatest$applied)-1
predictedValues = as.numeric(predictions)-1

nBPrecision <- sum(predictedValues & testDataValues) / sum(predictedValues)
nBRecall <- sum(predictedValues & testDataValues) / sum(testDataValues)


pred <- prediction(as.numeric(predictions[,2]), as.numeric(jobsDatatest$y)-1)
perf <- performance(pred,"tpr","fpr")

plot(perf)
abline(a=0, b= 1)

auc <- performance(pred,"auc")
auc <- unlist(slot(auc, "y.values"))

