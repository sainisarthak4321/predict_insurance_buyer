library(rpart)
library(rpart)
library(ROCR)

##Get the Data
data_original<-read.csv("C:/Academic/Capstone/capstone_caravarn_insurance/ticdata20001.csv")
test<-read.csv("C:/Academic/Capstone/capstone_caravarn_insurance/ticeval2000.csv")

#Preparing data 

ones<-data_original[which(data_original$Target==1),]
data_zero<-data_original[which(data_original$Target==0),]
data_new_zero<-data_zero[sample(nrow(data_zero), nrow(data_zero) * 0.128),]
data<- rbind(data_new_zero, ones)


# Choice Based sampling: Deciding the best sample size 
searchgrid = seq(1, 15, 1)
result.cost.insample = cbind(searchgrid, NA)
result.cost.outsample = cbind(searchgrid, NA)

library(rpart)

for (i in 1:15) {
  p<-i 
  data_check<-data_zero[sample(nrow(data_zero), nrow(data_zero) * 0.064*p),]
  data_loop<-rbind(ones,data_check)
  
  
  # Cart Model
  cart <- rpart(formula = Target ~. , data = data_loop, method="class")
  insample_cart = predict(cart, data_original,type="class")
  outsample_cart = predict(cart, test,type="class")
  cost <- function(r, pi) {
    weight1 = 3
    weight0 = 1
    c1 = (r == 1) & (pi == 0)  #logical vector - true if actual 1 but predict 0
    c0 = (r == 0) & (pi == 1)  #logical vecotr - true if actual 0 but predict 1
    return(mean(weight1 * c1 + weight0 * c0))
  }
  
  cost1.insample<-cost(data_original$Target, insample_cart)
  result.cost.insample[i,2]<-cost1.insample
  
  cost1.outsample<-cost(test$Target, outsample_cart)
  result.cost.outsample[i,2]<-cost1.outsample
  
}
plot(result.cost.insample)
plot(result.cost.outsample)

# Cart Model
final_cart<-data_zero[sample(nrow(data_zero), nrow(data_zero) * 0.064*2),]
data_cart<-rbind(ones,final_cart)
cart <- rpart(formula = Target ~. , data = data_cart, method="class")

cart
plot(cart)
text(cart)

#predict insample
total_predict <- predict(cart, data_original,type="class")
table(data_original$Target, total_predict, dnn = c("Truth", "Predicted"))

#predict outsample
predict_outsample = predict(cart, test,type="class")
table(test$Target, predict_outsample, dnn = c("Truth", "Predicted"))

#ROC Curve and AUC
#Insample
# Probability of getting 1
rpart2 = predict(cart, data_original, type = "prob")
pred = prediction(rpart2[, 2], data_original$Target)
perf = performance(pred, "tpr", "fpr")
plot(perf, colorize = TRUE)
slot(performance(pred, "auc"), "y.values")[[1]]

#Outsample
rpart3 = predict(cart, test, type = "prob")
pred = prediction(rpart3[, 2], test$Target)
perf = performance(pred, "tpr", "fpr")
plot(perf, colorize = TRUE)
slot(performance(pred, "auc"), "y.values")[[1]]



# Pruning 
plotcp(cart)

new_cart<-prune(cart,cp=.019)
plot(new_cart)
text(new_cart)
