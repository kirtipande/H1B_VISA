##### Load the cleaned data
cleaned_data <- read.csv("h1b_finaldataset.csv")

##### Exploratory Data Analysis
library(ggplot2)
library(dplyr)
library(tidyr)

# status vs waiting times
ggplot(cleaned_data, aes(x = status, y = Sub_to_dec)) +
  geom_boxplot() +
  theme_bw() +
  scale_y_continuous("Scaled Waiting Times") +
  scale_x_discrete("Application Status") +
  ggtitle("Waiting Times vs Application Status") +
  theme(plot.title = element_text(hjust = 0.5))

# status vs wages
ggplot(cleaned_data, aes(x = status, y = wage)) +
  geom_boxplot() +
  theme_bw() +
  scale_y_continuous("Scaled Wages") +
  scale_x_discrete("Application Status") +
  ggtitle("Wages vs Application Status") +
  theme(plot.title = element_text(hjust = 0.5))

# status vs application submission month
ggplot(cleaned_data, aes(x = status, y = sub_mon)) +
  geom_boxplot() +
  theme_bw() +
  scale_y_continuous("Application Submission Month") +
  scale_x_discrete("Application Status") +
  ggtitle("Wages vs Application Status") +
  theme(plot.title = element_text(hjust = 0.5))

# status vs submission month
d_month_status <- as.data.frame(cleaned_data %>%
                                  group_by(sub_mon, status) %>%
                                  summarise(Count = n()))

d_month_status <- d_month_status %>% spread(key = status, value = Count)

d_month_status$DENIED[is.na(d_month_status$DENIED)] <- 0

d_month_status$prop_denied <- d_month_status$DENIED*100/(d_month_status$CERTIFIED + d_month_status$DENIED)

ggplot(d_month_status, aes(x = sub_mon, y = prop_denied)) +
  geom_bar(stat = "identity") +
  ggtitle("Submission Month vs Denial Rate") +
  scale_x_discrete("Application Submission Month") +
  scale_y_continuous("Percentage applications denied")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# decision month vs denial rate
d_month_status <- as.data.frame(cleaned_data %>%
                                  group_by(de_mon, status) %>%
                                  summarise(Count = n()))

d_month_status <- d_month_status %>% spread(key = status, value = Count)

d_month_status$DENIED[is.na(d_month_status$DENIED)] <- 0

d_month_status$prop_denied <- d_month_status$DENIED*100/(d_month_status$CERTIFIED + d_month_status$DENIED)

ggplot(d_month_status, aes(x = de_mon, y = prop_denied)) +
  geom_bar(stat = "identity") +
  ggtitle("Decision Month vs Denial Rate") +
  scale_x_discrete("Application Decision Month") +
  scale_y_continuous("Percentage applications denied")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


##### Training and Testing Datasets
ds <- cleaned_data # Copy the data to ds variable

ds$sub_mon <- as.factor(ds$sub_mon)
ds$de_mon <- as.factor(ds$de_mon)

train <- sample(dim(ds)[1],dim(ds)[1]*.9) # 90/10 training testing split



##### Logistic Regression
# Define 'Denied' as positive
### Full Model
ds.train <- ds[train,] # Set up training set
ds.test <- ds[-train,] # Set up testing set
logit <- glm(status~.,data=ds.train,family = "binomial") # Fit the model
summary(logit)

# Full Model False Negative Rate
preds <- predict(logit,ds.test[,-8],type = 'response') # Generate predictions between 0 and 1
preds.log <- rep("CERTIFIED",length(preds))
preds.log[preds >= .5] <- "DENIED" # Assign classifications to preds.log based on the predictions
t.full <- table(ds.test$status,preds.log)
t.full[2,1]/sum(t.full[2,]) # 65.169% False Negatives/Classified Positives
t.full # Confusion matrix


### Stepwise Selection - Backwards
logit.step <- step(logit,direction = "backward")
# formula(logit.step) gives back the full model
# Model from backwards selection below
# logit.step <- glm(status ~ sub_mon + de_mon + workers + fulltime + pay_unit + h1bdepen + willful_violator +
#                    STEM + employer_region + worksite_region + Sub_to_dec + wage + 
#                    soc_new,data=ds.train,family = 'binomial')
summary(logit.step)
# Prediction error
preds.step <- predict(logit.step,ds.test[,-8],type = 'response') # Generate predictions between 0 and 1
preds.log.step <- rep("CERTIFIED",length(preds.step))
preds.log.step[preds.step >= .5] <- "DENIED" 
t.step <- table(ds.test$status,preds.log.step)
t.step[2,1]/sum(t.step[2,]) # 65.169% FN error
t.step # Confusion matrix


### Threshold selection[0.01, 0.9, step=0,01]
ds.test$pre <- predict(logit.step,ds.test[,-8], type='response')
thre=seq(0.01,0.9,0.01)
thre_FNR=NULL
thre_error=NULL
thre_FPR=NULL
# Denied=1 i.e. Positive
for (i in thre){
  ds.test$pred='CERTIFIED'
  ds.test$pred[which(ds.test$pre>=i)]='DENIED'
  ds.test$pred=as.factor(ds.test$pred)
  table(ds.test$status, ds.test$pred)
  result=data.frame(table(ds.test$status, ds.test$pred))
  #result: [1]TN [2]FN [3]FP [4]TP
  TN=result[1,3]
  FN=result[2,3]
  FP=result[3,3]
  TP=result[4,3]  
  #TPR=TP/(TP+FN) #sensitivity/power
  FNR=FN/(FN+TP) #false negative rate -- goal is to min FNR
  FPR=FP/(TN+FP) #specitificity
  error=(FP+FN)/dim(ds.test)[1]
  #thre_TPR=c(thre_TPR,TPR)
  thre_FNR=c(thre_FNR,FNR)
  thre_error=c(thre_error,error)
  thre_FPR=c(thre_FPR,FPR)
}
thre_set=data.frame(thre,thre_error,thre_FNR,thre_FPR)
ds.test <- ds.test[,-c(15,16)]
# Plot of total_error_rate, false_negative_rate, false_positve_rate
# Goal: minimize all these three rate, escapecially FNR
plot(thre_error~thre,pch=15,col="DarkTurquoise",cex=0.75,ylim=c(0,1),type='b',xlab='threshold',ylab='')
points(thre,thre_FNR,pch=16,col="DeepPink",cex=0.75)
points(thre,thre_FPR,pch=17,col="RosyBrown",cex=0.75)
lines(thre,thre_FNR,col="DeepPink")
lines(thre,thre_FPR,col="RosyBrown")
legend(0.6,0.56,c("Error_rate","FNR","FPR"),col=c("DarkTurquoise","DeepPink","RosyBrown"),text.col=c("DarkTurquoise","DeepPink","RosyBrown"),pch=c(15,16,17))
abline(v=0.08,lty=2,col='blue')
# From Rates Plot, we can choose threshold=0.08

# Fit predictions based on new threshold
preds.log.thre <- rep("CERTIFIED",length(preds.step))
preds.log.thre[preds.step >= .08] <- "DENIED" 
t.step <- table(ds.test$status,preds.log.thre)
t.step[2,1]/sum(t.step[2,]) # 42.055% FN error
t.step # Confusion matrix



##### LDA
library (MASS) 
# Fit LDA model
m_lda=lda(status~.,data=ds.train) 
lda.pred=predict(m_lda,ds.test[,-8])
lda_pred=lda.pred$class
t.lda <- table(ds.test$status, lda_pred) 
t.lda[2,1]/sum(t.lda[2,]) # FN_rate: 44.944%

# Set equal prior probabilities
m_lda2=lda(status~sub_mon+de_mon+fulltime+pay_unit+h1bdepen
          +willful_violator+STEM+employer_region+worksite_region
          +Sub_to_dec+wage,data=ds.train,prior=c(0.5,0.5))
lda.pred2=predict(m_lda2,ds.test[,-8])
lda_pred2=lda.pred2$class
table(ds.test$status, lda_pred2) # FN_rate: 43.018%



##### Random Forest
library(randomForest)

# Iterate through different numbers of trees to find the best one
for (i in c(50,100,250)){
  status.rf.trees = randomForest(formula = status ~ ., data = ds.train, ntree = i, 
                                 proximity = F, mtry = 4)
  pred.bag.trees = predict(status.rf.trees, newdata = ds.test, type = "response" )
  print(table(pred.bag.trees,ds.test$status)[2,2]/(table(pred.bag.trees,ds.test$status)[1,2]+table(pred.bag.trees,ds.test$status)[2,2]))
  # This prints accuracy, so higher is better
  # Output: 55.9%, 55.9%, 56.1%
}
# 50 is the best number of trees. It costs too much time to run 250 trees for only 0.2% improvement

# Iterate through different sets of thresholds to find the best set
for (i in seq(0.05, 0.5, 0.05)){
  status.rf.c = randomForest(formula = status ~ ., data = ds.train, ntree = 50,
                           proximity = F, mtry = 4, cutoff=c(i,1-i))
  pred.bag.c = predict(status.rf.c, newdata = ds.test, type = "response" )
  print(table(pred.bag.c,ds.test$status)[2,2]/(table(pred.bag.c,ds.test$status)[1,2]+ table(pred.bag.c,ds.test$status)[2,2]))
  # This also prints accuracy, not error
}
# The best threshold was the default threshold.

# Random forest with tuned ntree and prediction threshold.
status.rf = randomForest(formula = status ~ ., data=ds.train, ntree=50, proximity=F, mtry = 4)
varImpPlot(status.rf, main="Importance for Random Forest")

# Bagging model with the 3 best variables as identified by Importance plot
status.bag = randomForest(formula = status ~ wage + Sub_to_dec + soc_new, data=ds.train, ntree=50, proximity=F, mtry = 3)
varImpPlot(status.bag)
pred.bag = predict(status.bag,newdata = ds.test, type = "response")
table(ds.test$status,pred.bag) # 46.102% Error



##### XGBoost
install.packages("xgboost")
require(xgboost)
require(Matrix)
require(data.table)

previous_na_action <- options('na.action')
h1b <- cleaned_data
train_data <- ds.train
test_data <- ds.test

##Data preperation : Reading & Cleaning dataset :Replacing status with status_final as the last column for training and testing data
train_data$status_final <-train_data$status
names(train_data)
train_data=train_data[-c(8)]
test_data$status_final<-test_data$status
test_data_n <-test_data
test_data_n <- test_data_n[-c(8)]
names(test_data_n)

test_data_n <- test_data_n[-c(14)]
train_data_n <-train_data
train_data_n <-train_data_n[-c(14)]
names(train_data_n)
#--------------------------------------------------------------------------------------------------------------------
##Data Preperation : XGBoost works with only matrix input


#Converting train data to matrix format
options(na.action='na.pass')
sparse_matrix <- model.matrix(~., data = train_data_n)
#converting test data to matrix format
sparse_matrix_test <- model.matrix(~ ., data = test_data_n)


#Creating a numeric vector with the expected output : CERTIFIED ->1, DENIED->0

names(train_data)
output_vector <- train_data[c(14)]
names(output_vector)
output_vector$status_final=gsub("CERTIFIED", 0, output_vector$status_final) 
output_vector$status_final=gsub("DENIED", 1, output_vector$status_final)
dim(output_vector)


#XGBoost function requires the output in matrix format
output_vector <-as.matrix(output_vector) 
table(output_vector)
nrow(output_vector)
dim(output_vector)
#--------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------
##MODEL BUILDING
#
#XGBoost algorithm for classification
bst <- xgboost(data = sparse_matrix, label = output_vector, max_depth = 10,
               eta = 0.3, nthread = 2, nrounds = 200,objective = "binary:logistic")

#Identifying important features
importance <- xgb.importance(feature_names = colnames(sparse_matrix), model = bst)
importanceRaw <- xgb.importance(feature_names = colnames(sparse_matrix), model = bst, data = sparse_matrix, label = output_vector)
importanceClean <- importanceRaw[,`:=`(Cover=NULL, Frequency=NULL)]

#Plotting important variables
xgb.plot.importance(importance_matrix = importance)
y_pred <- predict(bst, data.matrix(sparse_matrix_test))

#Attaching predicted column to the test dataset
test_data$pred <- y_pred
names(test_data)
table(test_data$pred)

#Assigning threshold as 0.5 to convert numeric predictions to categorical
test_data$predv[which(test_data$pred >=0.05)]= 'DENIED'
test_data$predv[which(test_data$pred <0.05)]= 'CERTIFIED'


#--------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------
#TESTING
#Generating confusion matrix
confusion_matrix <- table(test_data$status_final,test_data$predv)
confusion_matrix # 30.182% error *Best model*
FP_error <- confusion_matrix[2,1]/(confusion_matrix[2,2]+confusion_matrix[2,1])
TN_error <- confusion_matrix[1,2]/(confusion_matrix[1,1]+confusion_matrix[1,2])

FP_error
TN_error
FP_error_rate <-FP_error *100
TN_error_rate <-TN_error *100
FP_error_rate
TN_error_rate

options(na.action=previous_na_action$na.action)
