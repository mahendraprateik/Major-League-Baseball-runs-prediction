#Importing data
mlbi <- read.csv("mlbinteraction.csv")
dim(mlbi)

set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the data

# calculate correlation matrix
correlationMatrix <- cor(mlbi[,-c(1:36)])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)

control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(mlbi$R~., data=mlbi, method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)