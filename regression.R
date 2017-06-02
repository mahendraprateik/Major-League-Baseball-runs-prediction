#code for Nebraska book company task - MLB data
library(dummies)    #Used for converting categorical to dummy
a <- dummy(mlbWithCategorical$TEAM, sep = ".")

#Importing the dataset without categorical variable
mlb <- read.csv("mlb.csv")
dim(mlb)

#Importing dataset that has only discrete and continuous (minus dummy) to find correlations
mlb2 <- read.csv('mlb2.csv')
dim(mlb2)

mlbCorr <- cor(mlb2)

write.csv(mlbCorr, 'corr.csv')


plot(mlb2$AVG, mlb$H)     #Batting Average and Hits correlation is 98.22%
plot(mlb2$RBI, mlb2$OBP)  #Runs Batted In and On-base percentage correlation is 83%
plot(mlb2$RBI, mlb2$SLG)  #Runs Batted In and Home Run correlation is 90.4.3%
plot(mlb2$RBI, mlb2$OPS)  #On-base Plus Slugging and Runs Batted In correlation is 94.3% 
plot(mlb2$AVG, mlb2$OBP)  #Batting average and On-base Plus Slugging correlation is 82.2% 
plot(mlb2$OBP, mlb2$OPS)  #On-base Plus Slugging and On-base percentage correlation is 85.8%
plot(mlb2$SLG, mlb2$OPS)  #Slugging percentage and On-base Plus Slugging correlation is 96.9%

#Removing the columns with high correlation
mlbWoCorr <- mlb[,-c(49, 44, 52)]
dim(mlbWoCorr)

#Creating a multiple linear regression model
model <- step(lm(R~., data = mlb, direction = "forward"))
summary(model)
b <- coefficients(model)
b
write.csv(b,"b.csv")

#Plotting the residuals
plot(resid(model))
plot(density(resid(model)))
qqnorm(resid(model))
qqline(resid(model))

