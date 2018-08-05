#Support Vector Machine
library(e1071)
library(verification)

##Get the Data
data_original<-read.csv("C:/Academic/Capstone/capstone_caravarn_insurance/ticdata20001.csv")
test<-read.csv("C:/Academic/Capstone/capstone_caravarn_insurance/ticeval2000.csv")

#Preparing data 

ones<-data_original[which(data_original$Target==1),]
data_zero<-data_original[which(data_original$Target==0),]
data_new_zero<-data_zero[sample(nrow(data_zero), nrow(data_zero) * 0.128),]
data<- rbind(data_new_zero, ones)


searchgrid = seq(1, 15, 1)
result.cost.insample = cbind(searchgrid, NA)
result.cost.outsample = cbind(searchgrid, NA)

cost <- function(r, pi) {
  weight1 = 3
  weight0 = 1
  c1 = (r == 1) & (pi == 0)  #logical vector - true if actual 1 but predict 0
  c0 = (r == 0) & (pi == 1)  #logical vecotr - true if actual 0 but predict 1
  return(mean(weight1 * c1 + weight0 * c0))
}


for (i in 1:15) {
  p<-i
  data_check<-data_zero[sample(nrow(data_zero), nrow(data_zero) * 0.064*p),]
  data_svm<-rbind(ones,data_check)
  data_svm<-data_svm[,-c(50,71)]
  
  
  model_svm = svm(Target ~ ., data = data_svm, cost = 1, gamma = 1/length(data_svm), 
                  probability = TRUE)
  
  prob.svm.insample = predict(model_svm, data_original, probability = TRUE)
  pred.svm.insample = as.numeric((prob.svm.insample >= 0.55))
  
  prob.svm.outsample = predict(model_svm, test, probability = TRUE)  
  pred.svm.outsample = as.numeric((prob.svm.outsample >= 0.55))
  
  
  
  cost1.insample<-cost(data_original$Target,pred.svm.insample )
  result.cost.insample[i,2]<-cost1.insample
  
  cost1.outsample<-cost(test$Target, pred.svm.outsample)
  result.cost.outsample[i,2]<-cost1.outsample
  
}
plot(result.cost.insample)
plot(result.cost.outsample)


data_svm_final<-data_zero[sample(nrow(data_zero), nrow(data_zero) * 0.064*4),]
data_svm_final<-rbind(ones,data_svm_final)
data_svm_final<-data_svm_final[,-c(50,71)]


model_svm_final = svm(Target ~ ., data = data_svm_final, cost = 1, gamma = 1/length(data_svm), 
                      probability = TRUE)

model_svm_final
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
  result.gam[i,2] <- cost1(data_original$Target,predict(model_svm_final,data_original,type = "prob"))
}
plot(result.gam, ylab = "Cost in Training Set")



prob.svm.insample = predict(model_svm_final, data_original, probability = TRUE)
pred.svm.insample = as.numeric((prob.svm.insample >= 0.5))
table(data_original$Target, pred.svm.insample, dnn = c("Obs", "Pred"))

prob.svm.outsample = predict(model_svm_final, test, probability = TRUE)
pred.svm.outsample = as.numeric((prob.svm.outsample >= 0.5))
table(test$Target, pred.svm.outsample, dnn = c("Obs", "Pred"))


roc.plot(data_original$Target == "1", prob.svm.insample)
roc.plot(data_original$Target == "1", prob.svm.insample)$roc.vol

roc.plot(test$Target == "1", prob.svm.outsample)
roc.plot(test$Target == "1", prob.svm.outsample)$roc.vol


