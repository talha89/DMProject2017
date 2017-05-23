setwd('/home/gurudas/Data Mining/Project/DMProject2017')

jobsData= read.csv("filtered_data - merged columns.csv", header=TRUE)
head(jobsData)

library(e1071)

jobsData$liked = as.factor(jobsData$liked)

# sample <- sample.int(nrow(jobsData), floor(.80*nrow(jobsData)), replace = F)
# jobsDatatrain <- jobsData[sample, ]
# jobsDatatest <- jobsData[-sample, ]

#SVM
# model <- svm(liked ~ . , jobsDatatrain)
# predicted_Liked <- predict(model, jobsDatatest)
# 
# trueValues = as.numeric(jobsDatatest$liked)-1
# predicted_Liked = as.numeric(predicted_Liked)-1
# 
# svmPrecision <- sum(predicted_Liked & trueValues) / sum(predicted_Liked)
# svmRecall <- sum(predicted_Liked & trueValues) / sum(trueValues)

#SVM with 5-fold validation
goodnessTable<- (c(precision = 0,recall=0))

for(i in seq_len(5))
{
  sample <- sample.int(nrow(jobsData), floor(.80*nrow(jobsData)), replace = F)
  jobsDatatrain <- jobsData[sample, ]
  jobsDatatest <- jobsData[-sample, ]
  model <- svm(liked ~ . , jobsDatatrain)
  predicted_Liked <- predict(model, jobsDatatest)
  trueValues = as.numeric(jobsDatatest$liked)-1
  predicted_Liked = as.numeric(predicted_Liked)-1
  goodnessTable<-goodnessTable +c(precision = sum(predicted_Liked & trueValues) / sum(predicted_Liked),recall=sum(predicted_Liked & trueValues) / sum(trueValues))
}
print(goodnessTable/5)

#PCA

