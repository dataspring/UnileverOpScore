#Multi-Class Summary Function
#Based on caret:::twoClassSummary
require(compiler)
multiClassSummary <- cmpfun(function (data, lev = NULL, model = NULL){
  #Load Libraries
  require(Metrics)
  require(caret)
  #Check data
  if (!all(levels(data[, "pred"]) == levels(data[, "obs"])))
    stop("levels of observed and predicted data do not match")
  #Calculate custom one-vs-all stats for each class
  prob_stats <- lapply(levels(data[, "pred"]), function(class){
    #Grab one-vs-all data for the class
    pred <- ifelse(data[, "pred"] == class, 1, 0)
    obs <- ifelse(data[, "obs"] == class, 1, 0)
    prob <- data[,class]
    
    #Calculate one-vs-all AUC and logLoss and return
    cap_prob <- pmin(pmax(prob, .000001), .999999)
    prob_stats <- c(auc(obs, prob), logLoss(obs, cap_prob))
    names(prob_stats) <- c('ROC', 'logLoss')
    return(prob_stats)
  })
  prob_stats <- do.call(rbind, prob_stats)
  rownames(prob_stats) <- paste('Class:', levels(data[, "pred"]))
  #Calculate confusion matrix-based statistics
  CM <- confusionMatrix(data[, "pred"], data[, "obs"])
  #Aggregate and average class-wise stats
  #Todo: add weights
  class_stats <- cbind(CM$byClass, prob_stats)
  class_stats <- colMeans(class_stats)
  
  #Aggregate overall stats
  overall_stats <- c(CM$overall)
  #Combine overall with class-wise stats and remove some stats we don't want
  stats <- c(overall_stats, class_stats)
  stats <- stats[! names(stats) %in% c('AccuracyNull',
                                       'Prevalence', 'Detection Prevalence')]
  #Clean names and return
  names(stats) <- gsub('[[:blank:]]+', '_', names(stats))
  return(stats)
}) 

CLEAR WORKSPACE
rm(list = ls(all = TRUE))
gc(reset=TRUE)




  # https://archive.ics.uci.edu/ml/machine-learning-databases/iris/
  readData <- function(path.name, file.name, column.types, missing.types, columnnames) {
    read.csv( url(paste(path.name, file.name, sep="")),
              colClasses=column.types, 
              na.strings = missing.types, col.names = columnnames 
    )
  }

Titanic.path <- "https://archive.ics.uci.edu/ml/machine-learning-databases/iris/"
filename <- "iris.data"
missing.types <- c("NA", "")
train.column.types <- c('numeric',   # Age
                        'numeric',   # SibSp
                        'numeric',   # Parch
                        'numeric',   # Ticke
                        'character') # Cabint
columnnames <- c('Sepallength','Sepalwidth','Petallength','Petalwidth','SpeciesType')

train.raw <- readData(Titanic.path, filename, 
                      train.column.types, missing.types, columnnames)
train.raw$Species <- as.factor(train.raw$SpeciesType)
train.raw$SpeciesType <- NULL

iris <- train.raw[1:110, ]
iris.test <- train.raw[111:149,]

#Setup parallel cluster
#If running on the command line of linux, use method='fork'
library(doParallel)
cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)

#Fit model
library(caret)
set.seed(19556)
model <- train(
  Species~.,
  data=iris,
  method='knn',
  tuneGrid=expand.grid(.k=1:30),
  metric='ROC',
  trControl=trainControl(
    method='repeatedcv',
    number=10,
    repeats=15,
    classProbs=TRUE,
    summaryFunction=multiClassSummary))


#Fit model
library(caret)
set.seed(19556)
modelnorm <- train(
  Species~.,
  data=iris,
  method='knn',
  tuneGrid=expand.grid(.k=1:30),
  metric='Accuracy',
  trControl=trainControl(
    method='repeatedcv',
    number=10,
    repeats=15,
    classProbs=TRUE))

cv.ctrl <- trainControl(method = "repeatedcv", repeats = 3,
                        summaryFunction = multiClassSummary,
                        classProbs = TRUE)

glm.tune.1 <- train(
  Species~.,
  data=iris,
  method='glm')




#Fit model
library(caret)
library(randomForest)
set.seed(19556)
modelnorm <- train(
  Species~.,
  data=iris,
  method='rf',
  tuneGrid=data.frame(.mtry = c(2, 3)),
  metric='ROC',
  trControl=trainControl(
    method='repeatedcv',
    number=10,
    repeats=15,
    classProbs=TRUE), ntree=1000)



#Stop parallel cluster
stopCluster(cl)

iris.pred <- predict(model, iris.test)
confusionMatrix(iris.pred, iris.test$Species)

iris.pred.norm <- predict(modelnorm, iris.test)
confusionMatrix(iris.pred.norm, iris.test$Species)


for(stat in c('Accuracy', 'Kappa', 'AccuracyLower', 'AccuracyUpper', 'AccuracyPValue',
              'Sensitivity', 'Specificity', 'Pos_Pred_Value',
              'Neg_Pred_Value', 'Detection_Rate', 'ROC', 'logLoss')) {
  plot(model, metric=stat)
}

plot(model, metric='ROC')

str(model)


##############################################################

sum(is.na(df.train))
save(df.train, file="D:\\Data Mining\\02-Dextra-Unilever\\01-InputsReadOnly\\unilever-train1.RData" )

load("D:\\Data Mining\\02-Dextra-Unilever\\01-InputsReadOnly\\unilever-train1.RData")

### ------------------------ Prediction part starts -------------------------------------------

require(caret)  # for modelling

## split training data into train batch and test batch
set.seed(7861)
training1.rows <- createDataPartition(df.train$Overall.Opinion, 
                                     p = 0.8, list = FALSE)
train1.batch.orig <- df.train[training1.rows, ]
test1.batch.orig <- df.train[-training1.rows, ]


train1.batch <- train1.batch.orig[,-c(1,2)]
test1.batch <- test1.batch.orig[,-c(1,2)]
train1.bat <- train1.batch.orig[,c(128:170)]

count


##---------------------------------------------------------------------------------------------------
#Multi-Class Summary Function
#Based on caret:::twoClassSummary
require(compiler)
multiClassSummary <- cmpfun(function (data, lev = NULL, model = NULL){
  #Load Libraries
  require(Metrics)
  require(caret)
  #Check data
  if (!all(levels(data[, "pred"]) == levels(data[, "obs"])))
    stop("levels of observed and predicted data do not match")
  #Calculate custom one-vs-all stats for each class
  prob_stats <- lapply(levels(data[, "pred"]), function(class){
    #Grab one-vs-all data for the class
    pred <- ifelse(data[, "pred"] == class, 1, 0)
    obs <- ifelse(data[, "obs"] == class, 1, 0)
    prob <- data[,class]
    
    #Calculate one-vs-all AUC and logLoss and return
    cap_prob <- pmin(pmax(prob, .000001), .999999)
    prob_stats <- c(auc(obs, prob), logLoss(obs, cap_prob))
    names(prob_stats) <- c('ROC', 'logLoss')
    return(prob_stats)
  })
  prob_stats <- do.call(rbind, prob_stats)
  rownames(prob_stats) <- paste('Class:', levels(data[, "pred"]))
  #Calculate confusion matrix-based statistics
  CM <- confusionMatrix(data[, "pred"], data[, "obs"])
  #Aggregate and average class-wise stats
  #Todo: add weights
  class_stats <- cbind(CM$byClass, prob_stats)
  class_stats <- colMeans(class_stats)
  
  #Aggregate overall stats
  overall_stats <- c(CM$overall)
  #Combine overall with class-wise stats and remove some stats we don't want
  stats <- c(overall_stats, class_stats)
  stats <- stats[! names(stats) %in% c('AccuracyNull',
                                       'Prevalence', 'Detection Prevalence')]
  #Clean names and return
  names(stats) <- gsub('[[:blank:]]+', '_', names(stats))
  return(stats)
}) 
#-----------------------------------------------------------------------------------------------------------------





# Using classification --------------------------------------




####-------------------trainig propoer models begin-------------------


## Define control function to handle optional arguments for train function
## Models to be assessed based on largest absolute area under ROC curve
cv.ctrl <- trainControl(method = "repeatedcv", repeats = 2, number = 5,
                        summaryFunction = multiClassSummary,
                        classProbs = TRUE)

#####------------------------------KNN-----------------------------------------------

require('caret')
library("doParallel")
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl, cores = detectCores() - 1)

start.time <- Sys.time()
###...Relevent codes..........
set.seed(35)
knn.tune <- train(
  Overall.Opinion ~ .,
  data=train1.batch,
  method='knn',
  tuneGrid=expand.grid(.k=1:9),
  metric='ROC',
  trControl=cv.ctrl)
#-------- time taken code 
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken




#####------------------------------randomForest-----------------------------------------------

require('randomForest')
require('caret')
library("doParallel")
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl, cores = detectCores() - 1)

start.time <- Sys.time()
###...Relevent codes..........
rf.grid  <- expand.grid(mtry = 150) # you can put different values for mtry
rf.tune  <- train(x = train1.batch[,-c(170)],
                        y = train1.batch$Overall.Opinion,
                        method = "rf",
                        metric = "ROC",
                        ntree = 1100,
                        tuneGrid = rf.grid,
                        trControl = cv.ctrl)
#-------- time taken code 
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken




#####------------------------------gbm-----------------------------------------------

require('gbm')
require('caret')
library("doParallel")
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl, cores = detectCores() - 1)
# stopcluster(cl)
start.time <- Sys.time()
###...Relevent codes..........

## Define control function to handle optional arguments for train function
## Models to be assessed based on largest absolute area under ROC curve
cv.ctrl <- trainControl(method = "repeatedcv", repeats = 1, number = 2,
                        summaryFunction = multiClassSummary,
                        classProbs = TRUE)

## note the dot preceding each variable
gbm.grid <-  expand.grid(n.trees = 500,interaction.depth = 11,shrinkage = 0.1)

gbm.tune <- train(x = train1.batch[,-c(170)],
                  y = train1.batch$Overall.Opinion,
                  method = "gbm",
                  metric = 'ROC',
                  trControl = cv.ctrl,
                  tuneGrid = gbm.grid)
#-------- time taken code 
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


#####------------------------------lrpart regression trees-----------------------------------------------

require('MASS')
require('caret')
library("doParallel")
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl, cores = detectCores() - 1)
# stopcluster(cl)
start.time <- Sys.time()
###...Relevent codes..........

## Define control function to handle optional arguments for train function
## Models to be assessed based on largest absolute area under ROC curve
cv.ctrl <- trainControl(method = "repeatedcv", repeats = 1, number = 2,
                        summaryFunction = multiClassSummary,
                        classProbs = TRUE)

## note the dot preceding each variable
rpart.grid <-  expand.grid(cp = 0.005)

rpart.tune <- train(x = train1.batch[,-c(170)],
                  y = train1.batch$Overall.Opinion,
                  method = "rpart",
                  metric = 'ROC',
                  trControl = cv.ctrl,
                  tuneGrid = rpart.grid)

#-------- time taken code 
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


# rpart.raw <- rpart(formula = Overall.Opinion ~ .,data=train1.batch)
# library(e1071)
# fm <- formula(Overall.Opinion ~ .,data=train1.batch)
# # Explore minsplit
# audit.rpart <- tune.rpart(fm, data=test1.batch, minsplit=seq(10,100,10))
# plot(audit.rpart, main="Tune rpart on minsplit")
# # cp
# audit.rpart <- tune.rpart(fm, data = test1.batch, cp = c(0.002,0.005,0.01,0.015,0.02,0.03))
# plot(audit.rpart,main="Performance of rpart vs. cp")
# readline()
# # maxdepth
# audit.rpart <- tune.rpart(fm, data = test1.batch, maxdepth = 1:5)
# plot(audit.rpart,main="Performance of rpart vs. cp")
# readline()



library(e1071)
fm <- formula(Overall.Opinion ~ .,data=train1.batch)
# Explore minsplit
audit.rpart <- tune.randomForest(fm, data=test1.batch, mtry=seq(2,7,1))
plot(audit.rpart, main="Tune rpart on mtry")
# cp
audit.rpart <- tune.rpart(fm, data = test1.batch, ntree = c(100,500,1000,1500))
plot(audit.rpart,main="Performance of rpart vs. cp")
readline()
# maxdepth
audit.rpart <- tune.rpart(fm, data = test1.batch, nodesize = 1:5)
plot(audit.rpart,main="Performance of rpart vs. cp")
readline()


#####------------------------------ada-----------------------------------------------

require('ada')
require('caret')
library("doParallel")
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl, cores = detectCores() - 1)
# stopcluster(cl)
start.time <- Sys.time()
###...Relevent codes..........

## Define control function to handle optional arguments for train function
## Models to be assessed based on largest absolute area under ROC curve
cv.ctrl <- trainControl(method = "repeatedcv", repeats = 1, number = 2,
                        summaryFunction = multiClassSummary,
                        classProbs = TRUE)

## note the dot preceding each variable
ada.grid <- expand.grid(.iter = c(50, 100),
                        .maxdepth = c(4, 8),
                        .nu = c(0.1, 1))

ada.tune <- train(x = test1.batch[,-c(170)],
                  y = test1.batch$Overall.Opinion,
                  method = "ada",
                  metric = 'ROC',
                  trControl = cv.ctrl,
                  tuneGrid = ada.grid)
#-------- time taken code 
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken



## drop the base level (R default)
M8 <- bayesglm (Overall.Opinion ~., data = test1.batch, 
                family=binomial(link="logit"), prior.scale=2.5, prior.df=Inf)
display (M8)


#####------------------------------svm-----------------------------------------------
require('kernlab')
require('caret')
library("doParallel")
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl, cores = detectCores() - 1)

start.time <- Sys.time()
###...Relevent codes..........

set.seed(35)
svm.tune <- train(x = test1.batch[,-c(170)],
                  y = test1.batch$Overall.Opinion,
                  method = "svmRadial",
                  tuneLength = 9,
                  metric = "ROC",
                  trControl = cv.ctrl)
#-------- time taken code 
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken



## Logistic regression model
glm.pred <- predict(glm.tune.5, test.batch)
confusionMatrix(glm.pred, test.batch$Fate)
## Boosted model
ada.pred <- predict(ada.tune, test.batch)
confusionMatrix(ada.pred, test.batch$Fate)
## Random Forest model
rf.pred <- predict(rf.tune, test.batch)
confusionMatrix(rf.pred, test.batch$Fate)
## SVM model 
svm.pred <- predict(svm.tune, test.batch)
confusionMatrix(svm.pred, test.batch$Fate)

## Logistic regression model (BLACK curve)
glm.probs <- predict(glm.tune.5, test.batch, type = "prob")
glm.ROC <- roc(response = test.batch$Fate,
               predictor = glm.probs$Survived,
               levels = levels(test.batch$Fate))
plot(glm.ROC, type="S")   
## Area under the curve: 0.8609 

## Boosted model (GREEN curve)
ada.probs <- predict(ada.tune, test.batch, type = "prob")
ada.ROC <- roc(response = test.batch$Fate,
               predictor = ada.probs$Survived,
               levels = levels(test.batch$Fate))
plot(ada.ROC, add=TRUE, col="green")    
## Area under the curve: 0.8759

## Random Forest model (RED curve)
rf.probs <- predict(rf.tune, test.batch, type = "prob")
rf.ROC <- roc(response = test.batch$Fate,
              predictor = rf.probs$Survived,
              levels = levels(test.batch$Fate))
plot(rf.ROC, add=TRUE, col="red") 
## Area under the curve: 0.8713

## SVM model (BLUE curve)
svm.probs <- predict(svm.tune, test.batch, type = "prob")
svm.ROC <- roc(response = test.batch$Fate,
               predictor = svm.probs$Survived,
               levels = levels(test.batch$Fate))
plot(svm.ROC, add=TRUE, col="blue")


cv.values <- resamples(list(Logit = glm.tune.5, Ada = ada.tune, 
                            RF = rf.tune, SVM = svm.tune))
dotplot(cv.values, metric = "ROC")



uni.rf.pr.no <- predict(rf.tune.nocomp, test.batch[,-c(1:127)])
Sys.time()
confusionMatrix(uni.rf.pr.no, test.batch$Overall.Opinion)







