#Importing data
a <- read.csv("mlbinteraction.csv")
dim(a)

# load the library
library(mlbench)
library(caret)

#removing categorical for correlation matrix
b <- a[,-c(1:36)]

#Following code is to see importance of variables
m <- train(R~., data=b,method="lm")
# estimate variable importance
importance <- varImp(m, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

#select top 8 important variables
a <- a[,c(1:36, 41, 44, 46, 40, 51, 50, 37,48, 39)]
colnames(a)

acorr <- cor(a[,-c(1:36, 45)])
print(acorr)
write.csv(acorr, 'acorr.csv')


s <- a[,-c(1:36, 45)]

# calculate correlation matrix
correlationMatrix <- cor(s)

# summarize the correlation matrix
print(correlationMatrix)

# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.9)

# print indexes of highly correlated attributes
print(highlyCorrelated)
a <- a[,-c(40, 41,44)]
colnames(a)

set.seed(124)

#Randomly selecting 80% train and 20% test
indices <- sample(1:nrow(a), size = 0.8*nrow(a))
t3 <- a[indices,]
t4 <- a[-indices,]

#running the model
m <- step(lm(t3$R~. , data = t3), direction = "both")
summary(m)

c2 <- coefficients(m)
write.csv(t3, "t3.csv")
write.csv(t4, "t4.csv")
write.csv(c2, "c2.csv")


#Plotting the residuals
plot(resid(m))
plot(density(resid(m)))
qqnorm(resid(m))
qqline(resid(m))


