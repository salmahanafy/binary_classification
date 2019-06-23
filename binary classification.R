rm(list=ls())

library("rpart")
library("rpart.plot")
library("randomForest")
setwd("D:/CCE/Kaggle Projects/New folder")

dataTrain <- read.csv("training.csv", header = TRUE, sep=';', dec = ",")
dataValidate <- read.csv("validation.csv", header = TRUE, sep=';', dec = ",")

#check if data is balanced
summary(dataTrain$classLabel)/length(dataTrain$classLabel)
# no.       yes. 
# 0.07459459 0.92540541 

summary(dataValidate$classLabel)/length(dataValidate$classLabel)
# no.  yes. 
# 0.535 0.465

#data is balanced in validation set but unbalanced in test set
#will balance data after cleaning as it gives better results
trainDT <-dataTrain
#variables
pairs(trainDT[,c(2,3,8,14,15,16)])

#vairable14 is highly correlated to variable17
cor.test(trainDT$variable14,trainDT$variable17)$estimate
# cor 
# 1

colMeans(is.na(trainDT[,c(14,16)]))
# variable14 variable17 
# 0.02040816 0.02040816
#remove any col from Variable14 or Variable17

colMeans(is.na(trainDT))
# remove variable18 since it has 57.972973% NA 

table(trainDT$variable19,trainDT$classLabel) 
# no. yes.
# 0 276    0
# 1   0  214
table(dataValidate$variable19,dataValidate$classLabel)
# no. yes.
# 0  53   47
# 1  54   46

#remove Variable19

#check if vaildation set contains missing predictors
sum(is.na(dataValidate)==1)
# [1] 133

#find importance of each variable using random forest 
trainDT<-dataTrain
trainDT<- trainDT[,-which(names(trainDT) %in% c("variable1","variable4","variable5",
                                                "variable7","variable10","variable12",
                                                "variable13","variable15","variable17",
                                                "variable18","variable19"))]
trainDT <- unique(trainDT)
trainRF <- na.omit(trainDT)

fitRF <- randomForest(formula = classLabel ~ .,
                      x = trainRF[,-which(names(trainRF)=="classLabel")],
                      y = trainRF[,which(names(trainRF)=="classLabel")],
                      na.action=na.omit)
round(importance(fitRF), 2)
#remove variables with low importance
#remove duplicate rows

#summary(trainDT$classLabel)/length(trainDT$classLabel)
# no.      yes. 
# 0.5632653 0.4367347

fit <- rpart(classLabel ~ .,
             method="class", 
             data=na.omit(trainDT),
             control=rpart.control(minsplit=20,maxdepth = 5, usesurrogate = 2),
             parms=list(split='information'))
rpart.plot(fit, type = 4, extra = 1)

#evaluate DT model
newdata <- dataValidate
pred <- predict(fit,newdata=newdata,type=c("class"))
acc <- table(pred,newdata$classLabel)
acc
# pred   no. yes.
# no.   94   10
# yes.  13   83

(acc[1,1]+acc[2,2])/200
#[1] 0.885

