
jobsData = read.csv("C:/Users/Talha Mir/Desktop/Course Slides/Semester 2/Data Mining/Project/filtered_data - merged columns.csv",
                    header = TRUE)

jobsData$liked = as.factor(jobsData$liked)

sample <- sample.int(nrow(jobsData), floor(.80*nrow(jobsData)), replace = F)
jobsDatatrain <- jobsData[sample, ]
jobsDatatest <- jobsData[-sample, ]



head(jobsData)
nrow(jobsData)
ncol(jobsData)
str(jobsData)

colSums(is.na(jobsData))

#jobsData = subset(jobsData, jobsData$applied==1 | jobsData$disliked == 1 | jobsData$liked == 1)
#jobsData$disliked = as.factor(jobsData$disliked)
#jobsData$applied = as.factor(jobsData$applied)


#jobsData <- data.frame(sapply(jobsData, function(x) as.numeric(as.character(x))))
#sapply(jobsData, class)


# RandomForest
library("ROCR")
library(randomForest)

jobs_randomForest <- randomForest(liked~., data=jobsDatatrain)

predictions <- predict(jobs_randomForest, jobsDatatest)

testDataValues = as.numeric(jobsDatatest$liked)-1
predictedValues = as.numeric(predictions)-1

rFPrecision <- sum(predictedValues & testDataValues) / sum(predictedValues)
rFRecall <- sum(predictedValues & testDataValues) / sum(testDataValues)


# NaiveBayes
library(e1071)
jobs_naive_bayes = naiveBayes(liked~., data=jobsDatatrain)
predictions <- predict(jobs_naive_bayes, jobsDatatest)


testDataValues = as.numeric(jobsDatatest$applied)-1
predictedValues = as.numeric(predictions)-1

nBPrecision <- sum(predictedValues & testDataValues) / sum(predictedValues)
nBRecall <- sum(predictedValues & testDataValues) / sum(testDataValues)


# ID3

library(RWeka)

fit <- J48(liked~., data=jobsDatatrain)
predictions <- predict(fit, jobsDatatest)

testDataValues = as.numeric(jobsDatatest$liked)-1
predictedValues = as.numeric(predictions)-1

id3Precision <- sum(predictedValues & testDataValues) / sum(predictedValues)
id3Recall <- sum(predictedValues & testDataValues) / sum(testDataValues)


predictions <- predict(fit, jobsDatatest, type="prob")

pred <- prediction(as.numeric(predictions[,2]), as.numeric(jobsDatatest$liked)-1)
perf <- performance(pred,"tpr","fpr")

plot(perf)
abline(a=0, b= 1)

auc <- performance(pred,"auc")
auc <- unlist(slot(auc, "y.values"))


#visualization of the obtained tree
library(partykit)
plot(as.party(fit))

InfoGainAttributeEval(liked ~ . , data = jobsDatatrain)


library(ggplot2)
p <- ggplot(jobsDatatrain, aes(x=applied,y=expert_title,color=applied), alpha=0.3) + geom_jitter()
p


#Knn
library(class)
library(knncat)


cl <- jobsDatatrain$liked
knncat(jobsDatatrain, jobsDatatest, classcol=13, k=3)



# 5 fold cross validation with naivebayes and id3

jobsData = read.csv("C:/Users/Talha Mir/Desktop/Course Slides/Semester 2/Data Mining/Project/filtered_data - merged columns.csv",
                    header = TRUE)

jobsData$liked = as.factor(jobsData$liked)


#Randomly shuffle the data
jobsData<-jobsData[sample(nrow(jobsData)),]

#Create 5 equally size folds
folds <- cut(seq(1,nrow(jobsData)),breaks=5,labels=FALSE)

testData <- list()
trainData <- list()

id3Precision <- list()
naiveBayesPrecision <- list()

id3Recall <- list()
naiveBayesRecall <- list()

#Perform 5 fold cross validation
for(i in 1:5){
  #Segment your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData[[i]] <- jobsData[testIndexes, ]
  trainData[[i]] <- jobsData[-testIndexes, ]
  #Use the test and train data partitions however you desire...
  
  # for ID3
  model_ID3 <- J48(liked~., data=trainData[[i]])
  predictions <- predict(model_ID3, testData[[i]])
  
  testDataValues = as.numeric(testData[[i]]$liked)-1
  predictedValues = as.numeric(predictions)-1
  
  id3Precision[[i]] <- sum(predictedValues & testDataValues) / sum(predictedValues)
  id3Recall[[i]] <- sum(predictedValues & testDataValues) / sum(testDataValues)
  
  # for NaiveBayes
  
  model_naive_bayes = naiveBayes(liked~., data=trainData[[i]])
  predictions <- predict(model_naive_bayes, testData[[i]])
  
  predictedValues = as.numeric(predictions)-1
  
  naiveBayesPrecision[[i]] <- sum(predictedValues & testDataValues) / sum(predictedValues)
  naiveBayesRecall[[i]] <- sum(predictedValues & testDataValues) / sum(testDataValues)
  
}

id3AvgPrecision = mean(unlist(id3Precision))
id3AvgRecall = mean(unlist(id3Recall))
naiveBayesAvgPrecision = mean(unlist(naiveBayesPrecision))
naiveBayesavgRecall = mean(unlist(naiveBayesRecall))



# Muti-label analysis

# https://mlr-org.github.io/mlr-tutorial/devel/html/multilabel/index.html
# https://mlr-org.github.io/mlr-tutorial/release/html/learner/index.html

jobsData = read.csv("C:/Users/Talha Mir/Desktop/Course Slides/Semester 2/Data Mining/Project/filtered_data.csv", header = TRUE)

str(jobsData)


jobsData = subset(jobsData, jobsData$applied==1 | jobsData$disliked == 1 | jobsData$liked == 1)


jobsData$applied = as.logical(jobsData$applied)
jobsData$liked = as.logical(jobsData$liked)
jobsData$disliked = as.logical(jobsData$disliked)

jobsData = jobsData[,-1:-2]

sample <- sample.int(nrow(jobsData), floor(.80*nrow(jobsData)), replace = F)
jobsDatatrain <- jobsData[sample, ]
jobsDatatest <- jobsData[-sample, ]

library("mlr")
labels = colnames(jobsDatatrain)[20:22]
jobs.task = makeMultilabelTask(id = "multi", data = jobsData, target = labels)
jobs.task


lrn.br = makeMultilabelBinaryRelevanceWrapper("classif.rpart")
lrn.br

lrn.br = makeLearner("classif.rpart", predict.type = "prob")
lrn.br = makeMultilabelBinaryRelevanceWrapper(lrn.br)

mod = train(lrn.br, jobs.task)

pred = predict(mod, task = jobs.task)
pred

getPredictionProbabilities(pred)

performance(pred)

performance(pred, measures = list(multilabel.tpr, multilabel.hamloss, multilabel.acc, multilabel.f1))

getMultilabelBinaryPerformances(pred, measures = list(acc, mmce, auc))

listMeasures("multilabel")



#pred = predict(mod, newdata = jobsDatatest)



# Binomial Logistic Regression

jobsData = jobsData[,-21:-22]

sample <- sample.int(nrow(jobsData), floor(.80*nrow(jobsData)), replace = F)
jobsDatatrain <- jobsData[sample, ]
jobsDatatest <- jobsData[-sample, ]


model <- glm(liked ~.,family=binomial(link='logit'),data=jobsDatatest)

# too many fucking variables to form an equation


# FIM & Association rules

jobsData = read.csv("C:/Users/Talha Mir/Desktop/Course Slides/Semester 2/Data Mining/Project/filtered_data.csv", header = TRUE)

jobsData = subset(jobsData, jobsData$applied==1 | jobsData$disliked == 1 | jobsData$liked == 1)

sapply(jobsData, levels)


jobsData$applied = as.factor(jobsData$applied)
jobsData$liked = as.factor(jobsData$liked)
jobsData$disliked = as.factor(jobsData$disliked)


table(jobsData$applied)
table(jobsData$liked)
table(jobsData$disliked)

str(jobsData)

jobsData = jobsData[,-1:-2]

#jobsData = jobsData[,-20:-21]

jobsData = jobsData[,-20]
jobsData = jobsData[,-21]


jobsData$expert_experience = as.factor(jobsData$expert_experience)
jobsData$project_experience = as.factor(jobsData$project_experience)

library(arules)
rules <- apriori(jobsData, parameter = list(minlen=2, sup = 0.333, target="frequent itemsets"))
rules <- sort(rules, by ="sup")
inspect(rules)

rules <- apriori(jobsData, parameter = list(minlen=2, sup = 0.4, conf=0.7, target="rules"))
rules <- sort(rules, by ="lift")
inspect(rules)


rules = apriori(jobsData,
                parameter = list(minlen=2, supp=0.3, conf=0.5),
                appearance = list(rhs=c("disliked=0", "disliked=1"),default="lhs"))
rules <- sort(rules, by ="lift")
inspect(rules)





