#importing train mlb3
mlb3 <- read.csv("mlb3.csv")
dim(mlb3)

mlb4 <- read.csv("mlb4.csv")
dim(mlb4)

cor(mlb3$AVG, mlb3$H)
plot(mlb3$AVG, mlb3$H)    #Batting Average and Hits correlation is 98.22%

cor(mlb3$RBI, mlb3$OBP)
plot(mlb3$RBI, mlb3$OBP)  #Runs Batted In and On-base percentage correlation is 79%

cor(mlb3$RBI, mlb3$SLG)
plot(mlb3$RBI, mlb3$SLG)  #Runs Batted In and Home Run correlation is 89%

cor(mlb3$RBI, mlb3$OPS)
plot(mlb3$RBI, mlb3$OPS)  #On-base Plus Slugging and Runs Batted In correlation is 93% 

cor(mlb3$AVG, mlb3$OBP)
plot(mlb3$AVG, mlb3$OBP)  #Batting average and On-base Plus Slugging correlation is 75.2%

cor(mlb3$OBP, mlb3$OPS)
plot(mlb3$OBP, mlb3$OPS)  #On-base Plus Slugging and On-base percentage correlation is 82%

cor(mlb3$SLG, mlb3$OPS)
plot(mlb3$SLG, mlb3$OPS)  #Slugging percentage and On-base Plus Slugging correlation is 96.9%

mlb3 <- mlb[,-c(44,49,52)]

model1 <- step(lm(R~., data = mlb3, direction = "forward"))
summary(model1)
c <- coefficients(model1)

write.csv(c,"c.csv")

#Plotting the residuals
plot(resid(model1))
plot(density(resid(model1)))
qqnorm(resid(model1))
qqline(resid(model1))

