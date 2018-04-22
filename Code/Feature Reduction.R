library(glmnet)
rm(list=ls())

d = read.csv("C:/Users/Kedar/Home Depot_IndepStudy/Code/Presentation/df_train.csv", header = TRUE)
colnames(d)
View(d)
attach(d)

f1 <- as.formula("relevance ~.")
f1

m1 = lm(f1, data=d)
summary(m1)

plot(m1)
plot(m1$fitted.values,m1$residuals)

#Run Lasso
y = relevance
x = model.matrix(m1)
m2 = cv.glmnet(x,y,family="gaussian")
#summary(m2)
#m2

#m2$lambda.min #Penalising Factor Optinal Lambda
#min(m2$cvm) #Gives the min mean squared error

plot(m2)
#log(m2$lambda)

#Run Lasso with optimal penalisation factor or optimal lambda
m3 = glmnet(x,y,family = "gaussian", lambda = m2$lambda.min)
m3$beta #Shows the relevant and irrelevant variables


#K-Fold Cross Validation K = 1000
#install.packages("xgboost")
library("xgboost")

p1 = dim(dat)[1]
q1 = floor(0.2*n)
ind1 = sample.int(p1, size=q1, replace = F)
dtrain1 = dat[-ind1,]
dtest1 = dat[ind1,]

#CalculateRMSPE
RMSPE <- function(actual,predicted){
  temp1 <- mean((actual-predicted)^2)
  temp2 <- sqrt(temp1)
  return(temp2)
}

for(i in 1:10){
  p1 = dim(d)[1]
  q1 = floor(0.2*n)
  index = sample.int(p1, size=q1, replace = F)
  train = d[-index,]
  test = d[index,]

  xgb_pred = Run_GLM_GaussianDist(f1,train,test)
  xgb_pred_rmse[i] = RMSPE(test$relevance,xgb_pred)
  
  xgb_train <- svm(f1, data = train)
  xgb_pred <- predict(xgb_train, newdata = test, type = "response")
  xgb_pred_final[i] = c(xgb_pred_final,xgb_pred)


  m1.aic = step(m1)
  summary(m1.aic)
  
  
  m4 <- update(m1, .~. - cosine_s.brand - cosine_s.material)
  m4.aic = step(m4)
  summary(m4.aic)

