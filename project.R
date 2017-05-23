jobsData = read.csv("C:/Users/Talha Mir/Desktop/Course Slides/Semester 2/Data Mining/Project/filtered_data - merged columns.csv", header = TRUE)
head(jobsData)
nrow(jobsData)
ncol(jobsData)
str(jobsData)


#jobsData = subset(jobsData, jobsData$applied==1 | jobsData$disliked == 1 | jobsData$liked == 1)
#jobsData$disliked = as.factor(jobsData$disliked)
#jobsData$applied = as.factor(jobsData$applied)


jobsData$liked = as.factor(jobsData$liked)


jobsData = jobsData[,-1:-2]
jobsData = jobsData[,-21:-22]


#jobsData <- data.frame(sapply(jobsData, function(x) as.numeric(as.character(x))))
#sapply(jobsData, class)


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


# ID3

library(RWeka)

fit <- J48(liked~., data=jobsDatatrain)
predictions <- predict(fit, jobsDatatest)

testDataValues = as.numeric(jobsDatatest$liked)-1
predictedValues = as.numeric(predictions)-1

id3Precision <- sum(predictedValues & testDataValues) / sum(predictedValues)
id3Recall <- sum(predictedValues & testDataValues) / sum(testDataValues)

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
knncat(jobsDatatrain, jobsDatatest, cl, k=3)


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


model <- glm(applied ~.,family=binomial(link='logit'),data=jobsDatatrain)


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





