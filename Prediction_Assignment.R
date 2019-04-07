library(caret)
library(randomForest) #Random forest for classification and regression
library(rpart) # Regressive Partitioning and Regression trees
library(rpart.plot) # Decision Tree plot
# setting the overall seed for reproduceability
set.seed(1000)
# We notice that both data sets contain columns with all missing values - these will be deleted.  

# Loading the training data set into my R session replacing all missing with "NA"

trainingset <- read.csv("pml-training.csv", na.strings=c("NA","#DIV/0!", ""))

# Loading the testing data set
testingset <- read.csv("pml-testing.csv", na.strings=c("NA","#DIV/0!", ""))


# Check dimensions for number of variables and number of observations
dim(trainingset)
dim(testingset)

# Delete columns with all missing values
trainingset<-trainingset[,colSums(is.na(trainingset)) == 0]
testingset <-testingset[,colSums(is.na(testingset)) == 0]

trainingset   <-trainingset[,-c(1:7)]
testingset <-testingset[,-c(1:7)]

dim(trainingset)
dim(testingset)


traintrainset <- createDataPartition(y=trainingset$classe, p=0.70, list=FALSE)
TrainTrainingSet <- trainingset[traintrainset, ] 
TestTrainingSet <- trainingset[-traintrainset, ]


plot(TrainTrainingSet$classe, col="green", main="Barchart of levels of variable classe within the TrainTrainingSet data set", xlab="classe Level", ylab="Frequency")

model1 <- rpart(classe ~ ., data=TrainTrainingSet, method="class")

prediction1 <- predict(model1, TestTrainingSet, type = "class")

# Plot the Decision Tree
rpart.plot(model1, main="Classification Tree", extra=102, under=TRUE, faclen=0)

# Test results on our TestTrainingSet data set:
confusionMatrix(prediction1, TestTrainingSet$classe)

model2 <- randomForest(classe ~. , data=TrainTrainingSet, method="class")

# Predicting:
prediction2 <- predict(model2, TestTrainingSet, type = "class")

# Test results on TestTrainingSet data set:
confusionMatrix(prediction2, TestTrainingSet$classe)

# predict outcome levels on the original Testing data set using Random Forest algorithm
predictfinal <- predict(model2, testingset, type="class")
predictfinal