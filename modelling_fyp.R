install.packages('DEoptimR')

library(randomForest)
library(caret)

install.packages("caret", dependencies = TRUE)

#read in data
data<-read.csv("dataset100000_2.csv",header=TRUE)

plot(data$demand,cex=0.2)

head(data)
data['X']=NULL
data$clust_no=NULL

weekend<-data[(!data$day=="Midweek"),]
week<-data[(data$day=="Midweek"),]
week$day=NULL
weekend$day=NULL

#Split into training/test data
set.seed(15)
samp_week = sample(c(0,1), nrow(week), replace=TRUE, prob=c(0.3,0.7))
train_week = week[samp_week == 1,]
test_week = week[samp_week == 0,]

samp_weekend = sample(c(0,1), nrow(weekend), replace=TRUE, prob=c(0.3,0.7))
train_weekend = weekend[samp_weekend == 1,]
test_weekend = weekend[samp_weekend == 0,]

########single tree##########
#fit classification tree
#week
library(rpart)
library(rpart.plot)
fit_wk <- rpart(train_week$demand ~ ., data=train_week,cp=0.0001,method="anova")
summary(fit_wk)

fit_wk$cptable
plotcp(fit_wk)

#get best cp
i.min_wk<-which.min(fit_wk$cptable[,"xerror"])
i.se_wk<-which.min(abs(fit_wk$cptable[,"xerror"]-(fit_wk$cptable[i.min_wk,"xerror"]+fit_wk$cptable[i.min_wk,"xstd"])))
cp.best_wk<-fit_wk$cptable[i.se_wk,"CP"]
cp.best_wk

#tree with best cp
fitnew_wk <- rpart(train_week$demand ~ ., data=train_week,cp=cp.best_wk,method="anova")
summary(fitnew_wk)

#model evaluation for trees
#evaluate tree
pred1_wk <-predict(fit_wk,test_week)
pred2_wk <-predict(fitnew_wk,test_week)

plot(pred1_wk,test_week$demand,xlab="Predicted Values",ylab="Actual Values",main="Actual vs Predicted values")
cor(pred1_wk,test_week$demand)
plot(pred2_wk,test_week$demand,xlab="Predicted Values",ylab="Actual Values",main="Actual vs Predicted values")
cor(pred2_wk,test_week$demand)

#error
error.wk_st<-test_week$demand-pred1_wk

# Root Mean Squared Error
rmse <-sqrt(mean(error.wk_st^2))

# Mean Absolute Error
mae <-mean(abs(error.wk_st))

#r squared
R2 <- 1 - (sum((error.wk_st )^2)/sum((test_week$demand-mean(test_week$demand))^2))


error2_wk<-(pred2_wk-test_week$demand)
sse2_wk<-sum((error2_wk^2))
mse2_wk<-sse2_wk/(nrow(test_week))


#weekend
fit_wkend <- rpart(train_weekend$demand ~ ., data=train_weekend,cp=0.0001,method="anova")
summary(fit_wkend)

fit_wkend$cptable
plotcp(fit_wkend)

#get best cp
i.min_wkend<-which.min(fit_wkend$cptable[,"xerror"])
i.se_wkend<-which.min(abs(fit_wkend$cptable[,"xerror"]-(fit_wkend$cptable[i.min_wkend,"xerror"]+fit_wkend$cptable[i.min_wkend,"xstd"])))
cp.best_wkend<-fit_wk$cptable[i.se_wkend,"CP"]
cp.best_wkend

#tree with best cp
fitnew_wkend <- rpart(train_weekend$demand ~ ., data=train_weekend,cp=cp.best_wkend,method="anova")
summary(fitnew_wkend)

#model evaluation for trees
#evaluate tree
pred1_wkend <-predict(fit_wkend,test_weekend)
pred2_wkend <-predict(fitnew_wkend,test_weekend)

plot(pred1_wkend,test_weekend$demand,xlab="Predicted Values",ylab="Actual Values",main="Actual vs Predicted values")
cor(pred1_wkend,test_weekend$demand)
plot(pred2_wkend,test_weekend$demand,xlab="Predicted Values",ylab="Actual Values",main="Actual vs Predicted values")
cor(pred2_wkend,test_weekend$demand)

#error
error.wkend_st<-test_weekend$demand-pred1_wkend

# Root Mean Squared Error
rmse <-sqrt(mean(error.wkend_st^2))

# Mean Absolute Error
mae <-mean(abs(error.wkend_st))

#r squared
R2 <- 1 - (sum((error.wk_st )^2)/sum((test_week$demand-mean(test_week$demand))^2))




###############random forest##############
#grid serach week
control <- trainControl(method="cv", number=3, search="grid")
tunegrid <- expand.grid(.mtry = c(2,4,6,8,10) )
rf_gridsearch <- train(demand~., data=train_week,ntree=200, method="rf", metric="RMSE", tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
write.csv(rf_gridsearch$results,"rf_grid_wk.csv")

plot(rf_gridsearch,xlab="Mtry",main="Random Forest Week Gridsearch")

rf_gridsearch_wk <- train(demand~., data=train_week,ntree=200, method="rf", metric="RMSE", tuneGrid=tunegrid1, trControl=control)
write.csv(xgb_gridsearch_wkend$results,"xgboost_grid.csv")

#best model week
randf_week=randomForest(demand~.,data=train_week,ntree=200,mtry=10, importance=TRUE)
randf_week$importance
varImpPlot(randf_week,main="Variable Importance Weekday Model")

plot(randf_week,lwd=2,main="Error for Number of Tress")

rand.prob<-predict(randf_week,test_week)
plot(rand.prob,test_week$demand,xlab="Predcited Values",ylab="Actual Values",main="Plot of Actual vs Predicted Values",cex=0.2)
cor(rand.prob,test_week$demand)

error.wk<-test_week$demand-rand.prob

# Root Mean Squared Error
rmse <-sqrt(mean(error.wk^2))

# Mean Absolute Error
mae <-mean(abs(error.wk))

#r squared
R2 <- 1 - (sum((error.wk )^2)/sum((test_week$demand-mean(test_week$demand))^2))

#grid serach weekend
control <- trainControl(method="cv", number=3, search="grid")
tunegrid <- expand.grid(.mtry = c(2,4,6,8,10) )
rf_gridsearch_wkend <- train(demand~., data=train_weekend,ntree=200,method="rf", metric="RMSE", tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch_wkend)
plot(rf_gridsearch_wkend,xlab="Mtry",main="Random Forest Weekend Gridsearch")
rf_gridsearch_wkend$finalModel
write.csv(rf_gridsearch_wkend$results,"rf_grid_wkend.csv")

#best model weekend 
randf_weekend=randomForest(demand~.,data=train_weekend,ntree=200,mtry=10, importance=TRUE)
plot(randf_weekend)
rand.prob.wkend<-predict(randf_weekend,test_weekend)
plot(rand.prob.wkend,test_weekend$demand,xlab="Predcited Values",ylab="Actual Values",main="Plot of Actual vs Predicted Values",cex=0.2)
abline(lm(rand.prob ~ test_week$demand))
cor(rand.prob.wkend,test_weekend$demand)

plot(randf_weekend,lwd=2,main="Error for Number of Tress")

error.wkend<-test_weekend$demand-rand.prob.wkend

# Root Mean Squared Error
rmse <-sqrt(mean(error.wkend^2))

# Mean Absolute Error
mae <-mean(abs(error.wkend))

#r squared
R2 <- 1 - (sum((error.wkend )^2)/sum((test_weekend$demand-mean(test_weekend$demand))^2))

randf_weekend$importance
varImpPlot(randf_weekend,main="Variable Importance Weekend Model")

############xg boosting##################
citation("caret")
data$time_cat2[data$time_cat=="before 6am"] <- 5
data$time_cat2[data$time_cat=="6am - 7am"] <- 6
data$time_cat2[data$time_cat=="7am - 8am"] <- 7
data$time_cat2[data$time_cat=="8am - 9am"] <- 8
data$time_cat2[data$time_cat=="9am - 10am"] <- 9
data$time_cat2[data$time_cat=="10am - 11am"] <- 10
data$time_cat2[data$time_cat=="11am - 12pm"] <- 11
data$time_cat2[data$time_cat=="12pm - 1pm"] <- 12
data$time_cat2[data$time_cat=="1pm - 2pm"] <- 13
data$time_cat2[data$time_cat=="2pm - 3pm"] <- 14
data$time_cat2[data$time_cat=="3pm - 4pm"] <- 15
data$time_cat2[data$time_cat=="4pm - 5pm"] <- 16
data$time_cat2[data$time_cat=="5pm - 6pm"] <- 17
data$time_cat2[data$time_cat=="6pm - 7pm"] <- 18
data$time_cat2[data$time_cat=="7pm - 8pm"] <- 19
data$time_cat2[data$time_cat=="8pm - 9pm"] <- 20
data$time_cat2[data$time_cat=="after 9pm"] <- 21
library(xgboost)

#week#
#grid search
control <- trainControl(method="cv", number=6, search="grid")
tunegrid = expand.grid(
  nrounds = 1000,
  eta = c(0.5,0.25,0.1,0.01, 0.001, 0.0001),
  max_depth = c(2, 4, 6, 8, 10,12),
  gamma = 1,
  colsample_bytree=1,
  min_child_weight=1,
  subsample=1)

xgb_gridsearch_wk <- train(demand~., data=train_week, method="xgbTree", metric="RMSE", tuneGrid=tunegrid, trControl=control)
write.csv(xgb_gridsearch_wk$results,"xgboost_grid_wk.csv")
plot(xgb_gridsearch_wk)
xgb_gridsearch_wk$finalModel

head(test_week)
#final model
dtrain_wk = xgb.DMatrix(data =  as.matrix(train_week[-c(1,10)]), label = train_week$demand)
dtest_wk = xgb.DMatrix(data =  as.matrix(test_week[-c(1,10)]), label = test_week$demand)

xgb <- xgboost(data = dtrain_wk,
               max.depth=10,
               booster = "gbtree", 
               objective = "reg:linear",
               nrounds=1000,
               eta=0.1,
               print_every_n=100)
plot(xgb$evaluation_log,cex=0.2,main="RMSE for number of iterations",xlab="Number of Iterations",ylab="RMSE")

xgb.pred <- predict(xgb, dtest_wk)
plot(xgb.pred,test_week$demand,xlab="Predcited Values",ylab="Actual Values",main="Plot of Actual vs Predicted Values Boosting Week",cex=0.2)
cor(xgb.pred,test_week$demand)
sse<-sum((test_week$demand-xgb.pred)^2)
mse<-sse/nrow(test_week)
rmse<-sqrt(mse)
mae<-sum(abs(test_week$demand-xgb.pred))/nrow(test_week)
R2 <- 1 - (sum((test_week$demand-xgb.pred )^2)/sum((test_week$demand-mean(xgb.pred ))^2))


#weekend#
#grid search
control <- trainControl(method="cv", number=7, search="grid")
control <- trainControl(method="cv", number=6, search="grid")
tunegrid = expand.grid(
  nrounds = 1000,
  eta = c(0.5,0.25,0.1,0.01, 0.001, 0.0001),
  max_depth = c(2, 4, 6, 8, 10,12),
  gamma = 1,
  colsample_bytree=1,
  min_child_weight=1,
  subsample=1)
xgb_gridsearch_wkend <- train(demand~., data=train_weekend, method="xgbTree", metric="RMSE", tuneGrid=tunegrid, trControl=control)
write.csv(xgb_gridsearch_wkend$results,"xgboost_grid.csv")
plot(xgb_gridsearch_wkend)
xgb_gridsearch_wkend$finalModel

#final model
dtrain_wkend = xgb.DMatrix(data =  as.matrix(train_weekend[-c(1,10)]), label = train_weekend$demand)
dtest_wkend = xgb.DMatrix(data =  as.matrix(test_weekend[-c(1,10)]), label = test_weekend$demand)

xgb_wkend <- xgboost(data = dtrain_wkend,
               max.depth=10,
               booster = "gbtree", 
               objective = "reg:linear",
               nrounds=1000,
               print_every_n=100,
               eta=0.25)

plot(xgb_wkend$evaluation_log,cex=0.2,main="RMSE for number of iterations",xlab="Number of Iterations",ylab="RMSE")

xgb_wkend.pred <- predict (xgb_wkend, dtest_wkend)
plot(xgb_wkend.pred,test_weekend$demand,xlab="Predcited Values",ylab="Actual Values",main="Plot of Actual vs Predicted Values",cex=0.2)
cor(xgb_wkend.pred,test_weekend$demand)

sse<-sum((test_weekend$demand-xgb_wkend.pred)^2)
mse<-sse/nrow(test_weekend)
rmse<-sqrt(mse)
mae<-sum(abs(test_weekend$demand-xgb_wkend.pred))/nrow(test_weekend)
R2 <- 1 - (sum((test_weekend$demand-xgb_wkend.pred )^2)/sum((test_weekend$demand-mean(xgb_wkend.pred ))^2))



