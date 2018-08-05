library(glmnet)
library(verification)

##Get the Data
data_original<-read.csv("C:/Academic/Capstone/capstone_caravarn_insurance/ticdata20001.csv")
test<-read.csv("C:/Academic/Capstone/capstone_caravarn_insurance/ticeval2000.csv")

#Preparing data 

ones<-data_original[which(data_original$Target==1),]
data_zero<-data_original[which(data_original$Target==0),]
data_new_zero<-data_zero[sample(nrow(data_zero), nrow(data_zero) * 0.128),]
data<- rbind(data_new_zero, ones)

#Logistic Model

ones<-data_original[which(data_original$Target==1),]
data_zero<-data_original[which(data_original$Target==0),]
data_new_zero<-data_zero[sample(nrow(data_zero), nrow(data_zero) * 0.064*5),]
data_logistic<- rbind(data_new_zero, ones)



model_log <- glm(Target ~ .- X5 -X15-X9-X12-X13-X25-X35-X36-X31, family = binomial, data_logistic)
summary(model_log)
model_Step <- step(model_log, k = log(nrow(data)))
model_Step

#Determining the cut off probability

searchgrid = seq(0.01, 0.8, 0.01)
result.gam = cbind(searchgrid, NA)
cost1 <- function(r, pi) {
  weight1 = 1
  weight0 = 1
  c1 = (r == 1) & (pi < pcut)
  c0 = (r == 0) & (pi > pcut)
  return (mean(weight1 * c1 + weight0 * c0))
}
for (i in 1:length(searchgrid)) {
  pcut <- result.gam[i,1]
  result.gam[i,2] <- cost1(data$Target,predict(model_Step,type = "response"))
}
plot(result.gam, ylab = "Cost in Training Set")

pcut<-.31

#insample_prediction
insample_predict <- predict(model_Step,data_original, type = "response")
predicted_insample <- insample_predict > 0.31
predicted_insample <- as.numeric(predicted_insample)
table(data_original$Target, predicted_insample, dnn = c("Truth", "Predicted"))

#outsample_prediction
outsample_predict <- predict(model_Step,test, type = "response")
predicted_outsample <- outsample_predict > 0.31
predicted_outsample <- as.numeric(predicted_outsample)
table(test$Target, predicted_outsample, dnn = c("Truth", "Predicted"))


# ROC Curve and AUC

roc.plot(data_original$Target == "1", insample_predict)
roc.plot(data_original$Target == "1", insample_predict)$roc.vol

roc.plot(test$Target == "1", outsample_predict)
roc.plot(test$Target == "1", outsample_predict)$roc.vol




# Logistic using lasso 

#Original 
ones<-data_original[which(data_original$Target==1),]
data_zero<-data_original[which(data_original$Target==0),]
data_new_zero<-data_zero[sample(nrow(data_zero), nrow(data_zero) * 0.064*2),]
data_logistic<- rbind(data_new_zero, ones)



glmmod<-cv.glmnet(x=as.matrix(data_logistic[,-c(86)]),y=data_logistic$Target,alpha=.012)


plot(glmmod)
cv_lasso_fit = cv.glmnet(x = as.matrix(data_logistic[, -c(which(colnames(data_logistic) == 
                                                                  "Target"))]), y = data_logistic$Target, alpha = 1, nfolds = 5)
cv_lasso_fit$lambda.min

plot(cv_lasso_fit)
coef(glmmod,.02)



#Determining the cut off probability

searchgrid = seq(0.01, 1, 0.01)
result.gam = cbind(searchgrid, NA)
cost1 <- function(r, pi) {
  weight1 = 1
  weight0 = 1
  c1 = (r == 1) & (pi < pcut)
  c0 = (r == 0) & (pi > pcut)
  return (mean((weight1 * c1) + (weight0 * c0)))
}
for (i in 1:length(searchgrid)) {
  pcut <- result.gam[i,1]
  result.gam[i,2] <- cost1(data_original$Target,predict(glmmod, as.matrix(data_original[, -c(which(colnames(data_original) ==  "Target"))]), s = cv_lasso_fit$lambda.min)
  )
}
plot(result.gam, ylab = "Cost in Training Set")

pcut<-.65

insample_predict = predict(glmmod, as.matrix(data_original[, -c(which(colnames(data_original) == 
                                                                        
                                                                        "Target"))]), s = cv_lasso_fit$lambda.min)
insample_predict<-as.numeric(insample_predict>=pcut)
table(data_original$Target,insample_predict)


outsample_predict = predict(glmmod, as.matrix(test[, -c(which(colnames(test) == 
                                                                
                                                                "Target"))]), s = cv_lasso_fit$lambda.min)
outsample_predict<-as.numeric(outsample_predict>=pcut)
table(test$Target,outsample_predict)
