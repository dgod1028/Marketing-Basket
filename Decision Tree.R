## install.packages(c("rpart","rpart.plot","randomForest","ada"))

## install.packages("ipred")   ## Optional


## 1 Simulate Data

set.seed(1)
Sex = as.factor(sample(c("Male","Female"),size = 500,replace = TRUE,prob=c(0.6,0.4)))
Kid = as.factor(sample(c("Yes","No"),replace = TRUE,size = 500,prob=c(0.3,0.7)))
Email = as.factor(sample(c("Yes","No"),replace = TRUE,size = 500,prob=c(0.8,0.2)))
ownHome = as.factor(sample(c("Yes","No"),replace = TRUE,size = 500,prob=c(0.4,0.6)))


Subscribe = Email
for(i in 1:500){
  if(Email[i] == "Yes"){
    temp.p = 0.7
  }
  else{
    temp.p = 0.3
  }
  Subscribe[i] = sample(c("Yes","No"),replace=TRUE,size=1,prob=c(temp.p,1-temp.p))
  if(Kid[i] == "Yes"){
    temp.p2 = 0.8 
  }
  else{
    temp.p2 = 0.2
  }
  ownHome[i] = sample(c("Yes","No"),replace=TRUE,size=1,prob=c(temp.p2,1-temp.p2))
}
Buy = as.factor(sample(c("Yes","No"),replace=TRUE,size=500,prob=c(0.6,0.4)))
for(i in 1:500){
  prob = 0.4*(as.numeric(Subscribe[i])-1) + 
    0.2*(as.numeric(Email[i]) - 1) - 
    0.08*(as.numeric(Kid[i])-1) -
    0.1*(as.numeric(Sex[i])-1) + 
    0.15*(as.numeric(ownHome[i])-1) 
  Buy[i] = sample(c("Yes","No"),replace=TRUE,size=1,prob = c(max(0.1,prob),max(1-prob,0.1)))
}

Buy.C = c()
for(i in 1:500){
  prob = 4*(as.numeric(Subscribe[i])-1) + 
    2*(as.numeric(Email[i]) - 1) - 
    1.2*(as.numeric(Kid[i])-1) -
    0.8*(as.numeric(Sex[i])-1) + 
    1.5*(as.numeric(ownHome[i])-1) 
  temp = rnorm(1,prob,1)*100
  if(temp<0){temp=0}
  Buy.C[i] = temp
}
Data = data.frame(Buy,Sex,Kid,Email,Subscribe,ownHome)
Data.C =  data.frame(Buy.C,Sex,Kid,Email,Subscribe,ownHome)

write.table(Data,"dis_data.csv",sep=",")
write.table(Data.C,"con_data.csv",sep=",")

write.table(Dec.Data,"Dec_Data.csv",sep=",")
head(Dec.Data)
for(i in 1:500){
  if(Dec.Data[i,2] == "No"){Dec.Data[i,3] = 0}
  else{Dec.Data[i,3] = floor(rnorm(1,max(0,(
    (as.numeric(Dec.Data[i,4])-1)*0.4 +
      (as.numeric(Dec.Data[i,5])-1)*0.2 +
      (as.numeric(Dec.Data[i,6])-1)*0.15 +
      (as.numeric(Dec.Data[i,7])-1)*0.6 +
      (as.numeric(Dec.Data[i,8])-1)*0.1)
    *100),5
  )
  )
  }
}


write.ta
############# 2 Classic Decison tree (discrete)


Data = read.table("dis_data.csv",header=TRUE,sep=",")
Data.C = read.table("con_data.csv",header=TRUE,sep=",")
set.seed(1)
TestD = sample(1:500,50)
Data.Tr = Data[-TestD,]
Data.T  = Data[TestD,]

head(Data.Tr)

library(rpart)      ### decision tree

result = rpart(Buy ~.,data = Data.Tr[200:300,], method="class")
result
library(rpart.plot) ### plot graph
prp(result, type=3, extra=102,nn=TRUE, fallen.leaves=TRUE,faclen=0,varlen=0,shadow.col ="grey" )

yhat.result = predict(result,Data.T,type="class")
yhat.table = table(Data.T$Buy,yhat.result)
print("==== Class")
print(yhat.table)
print(paste("estimate of misclassification error:",(yhat.table[1,2]+yhat.table[2,1])/nrow(Data.T) ))

####     Bagging (Optional)


library(ipred)
test = bagging(Buy~.,data=Data.Tr, nbagg=25,coob = TRUE)
  
print(test)

##prp(test$mtrees[[25]]$btree, type=2, extra=102,nn=TRUE, fallen.leaves=TRUE,faclen=0,varlen=0,shadow.col ="grey",)

#####################

##### 3 randomForest

library(randomForest)

set.seed(1)

bag.data = randomForest(Buy~.,data=Data.Tr,mtry=5) ## Bagging

for.data = randomForest(Buy~.,data=Data.Tr,mtry=2) ## RF

plot(bag.data$err.rate[,2],col="red",type="l",ylim=c(0.3,0.4),main="error rate",xlab="nTree",ylab="error rate")
lines(for.data$err.rate[,2],col="blue")

yhat.bag = predict(bag.data,newdata=Data.T)

yhat.for = predict(for.data,newdata=Data.T)

bag.table = table(Data.T[,1],yhat.bag)
for.table = table(Data.T[,1],yhat.for)

print("==== Bagging")
print(bag.table)
print(paste("estimate of misclassification error:",(bag.table[1,2]+bag.table[2,1])/nrow(Data.T) ))

print("==== Random Forest")
print(for.table)
print(paste("estimate of misclassification error:",(for.table[1,2]+for.table[2,1])/nrow(Data.T) ))

importance(bag.data)
importance(for.data)

varImpPlot(bag.data)
varImpPlot(for.data)

#### 4 Boosting


library(ada)

set.seed(1)
result.boost = ada(Buy~.,data=Data.Tr,iter=50,type=c("discrete"))

result.boost

yhat.boost = predict(result.boost,newdata=Data.T)

boost.table = table(yhat.boost,Data.T[,1])

print("==== Boosting")
print(boost.table)
print(paste("estimate of misclassification error:",(boost.table[1,2]+boost.table[2,1])/nrow(Data.T) ))


##### 5 Continious Data


Data.C.Tr = Data.C[-TestD,]
Data.C.T  = Data.C[TestD,]
head(Data.C)

set.seed(1)
result.C = rpart(Buy.C ~ .,data=Data.C.Tr,method="anova")
result.C
prp(result.C, type=3,nn=TRUE, fallen.leaves=TRUE,faclen=0,varlen=0,shadow.col ="grey" )

yhat.c = predict(result.C, newdata= Data.C.T)

(rmse.boost = sqrt(sum((yhat.c-Data.C.T[,1])^2)/50) )   ### RMSE

result.c2 = randomForest(Buy.C ~.,data=Data.C.Tr,mtry=5)
yhat.c2 = predict(result.c2,Data.C.T)

(rmse.aa = sqrt(sum((yhat.c2-Data.C.T[,1])^2)/50) )

result.c3 = randomForest(Buy.C ~.,data=Data.C.Tr,mtry=2)
yhat.c3 = predict(result.c3,Data.C.T)

(rmse.bb = sqrt(sum((yhat.c3-Data.C.T[,1])^2)/50) )
## optional (prune)

plotcp(result.C)

result.C1 = prune(result.C,cp = 0.015)
prp(result.C1, type=3,nn=TRUE, fallen.leaves=TRUE,faclen=0,varlen=0,shadow.col ="grey" )
