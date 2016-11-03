#required Libraries
library(h2o)
library(kernlab)
library(gbm)


# Start h2o ,decide min and max memory sizes,number of cpu cores to be used (-1 means all cores)
localH2O <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE,
                     max_mem_size = '12g', min_mem_size = '4g', nthreads = -1)

#####################################################################################################
#ONLY FOG NET
load('RDFiles/FogNetTrainingDataSet_OnlyFNData.Rd')


training.data$X <- as.Date(training.data$X)
testing.data$X <- as.Date(testing.data$X)

#Training data
training.data.hex<-as.h2o(training.data[1:15])
#predict values for the Testing dataset 
testing.data.hex<-as.h2o(testing.data[1:14])


#crete H2O model
H2ORandomForest.OnlyFogNet <- h2o.randomForest(x = c(1,3:15), y = 2 ,  training_frame = training.data.hex,
                                    model_id='H2ORandomForest.OnlyFogNet',ntrees = 1000)

#H2ORandomForest
H2ORandomForest.OnlyFogNet.prediction.hex <- h2o.predict(H2ORandomForest.OnlyFogNet, testing.data.hex)
H2ORandomForest.OnlyFogNet.prediction <- as.data.frame(H2ORandomForest.OnlyFogNet.prediction.hex)



#-------------------------------------------------------------------------------------------------------#

predict_vars <- colnames(training.data)
predict_vars <- predict_vars[-2] 

#Creating Formula
f <- as.formula(paste(paste('yield', collapse = " + "),'~', paste(predict_vars, collapse = " + ")))


SVMModel <- ksvm(f, data = training.data)
SVMModel.prediction <- predict(SVMModel, testing.data[,predict_vars], type = "response")
SVMModel.prediction <- as.data.frame(SVMModel.prediction)

for(i in seq(1:dim(SVMModel.prediction)[1])){
  if(SVMModel.prediction$V1[i] < 0){
    SVMModel.prediction$V1[i] <- 0.0
  }
}

SVM.OnlyFogNet.prediction <- SVMModel.prediction






#-------------------------------------------------------------------------------------------------------#

predict_vars <- colnames(training.data)
predict_vars <- predict_vars[-2:-1] 

training.data$Year <- as.character(training.data$Year)
training.data$Year <- as.factor(training.data$Year)

#Creating Formula
f <- as.formula(paste(paste('yield', collapse = " + "),'~', paste(predict_vars, collapse = " + ")))

GBM.Model <- gbm(f,data = training.data, n.trees = 1000)
GBM.prediction <- predict(GBM.Model, testing.data[,predict_vars],n.trees = 500, type = "link")
GBM.prediction <- as.data.frame(GBM.prediction)

GBM.OnlyFogNet.prediction <- GBM.prediction

#####################################################################################################
# FOG NET PLUS SIDI 
load('RDFiles/FogNetTrainingDataSet_FNPlusSidi.Rd')

training.data.PlusSidi$X <- as.Date(training.data.PlusSidi$X)
testing.data.PlusSidi$X <- as.Date(testing.data.PlusSidi$X)


#Training data
training.data.hex<-as.h2o(training.data.PlusSidi[1:39])
#predict values for the Testing dataset 
testing.data.hex<-as.h2o(testing.data.PlusSidi[1:38])

#crete H2O model
H2ORandomForest.PlusSidi <- h2o.randomForest(x = c(1,3:39), y = 2 ,  training_frame = training.data.hex,
                                               model_id='H2ORandomForest.PlusSidi',ntrees = 1000)

#H2ORandomForest
H2ORandomForest.PlusSidi.prediction.hex <- h2o.predict(H2ORandomForest.PlusSidi, testing.data.hex)
H2ORandomForest.PlusSidi.prediction <- as.data.frame(H2ORandomForest.PlusSidi.prediction.hex)


#-------------------------------------------------------------------------------------------------------#

predict_vars <- colnames(training.data.PlusSidi)
predict_vars <- predict_vars[-2] 

#Creating Formula
f <- as.formula(paste(paste('yield', collapse = " + "),'~', paste(predict_vars, collapse = " + ")))


SVMModel <- ksvm(f, data = training.data.PlusSidi)
SVMModel.prediction <- predict(SVMModel, testing.data.PlusSidi[,predict_vars], type = "response")
SVMModel.prediction <- as.data.frame(SVMModel.prediction)

for(i in seq(1:dim(SVMModel.prediction)[1])){
  if(SVMModel.prediction$V1[i] < 0){
    SVMModel.prediction$V1[i] <- 0.0
  }
}

SVM.PlusSidi.prediction <- SVMModel.prediction

#-------------------------------------------------------------------------------------------------------#

predict_vars <- colnames(training.data.PlusSidi)
predict_vars <- predict_vars[-2:-1] 

training.data$Year <- as.character(training.data.PlusSidi$Year)
training.data$Year <- as.factor(training.data.PlusSidi$Year)

#Creating Formula
f <- as.formula(paste(paste('yield', collapse = " + "),'~', paste(predict_vars, collapse = " + ")))

GBM.Model <- gbm(f,data = training.data.PlusSidi, n.trees = 1000)
GBM.prediction <- predict(GBM.Model, testing.data.PlusSidi[,predict_vars],n.trees = 500, type = "link")
GBM.prediction <- as.data.frame(GBM.prediction)

GBM.PlusSidi.prediction  <- GBM.prediction


#####################################################################################################
# FOG NET PLUS GUELMIM 
load('RDFiles/FogNetTrainingDataSet_FNPlusGuelmim.Rd')

training.data.PlusGuelmim$X <- as.Date(training.data.PlusGuelmim$X)
testing.data.PlusGuelmim$X <- as.Date(testing.data.PlusGuelmim$X)



#Training data
training.data.hex<-as.h2o(training.data.PlusGuelmim[1:27])
#predict values for the Testing dataset 
testing.data.hex<-as.h2o(testing.data.PlusGuelmim[1:26])

#crete H2O model
H2ORandomForest.PlusGuelmim <- h2o.randomForest(x = c(1,3:27), y = 2 ,  training_frame = training.data.hex,
                                             model_id='H2ORandomForest.PlusGuelmim',ntrees = 1000)

#H2ORandomForest
H2ORandomForest.PlusGuelmim.prediction.hex <- h2o.predict(H2ORandomForest.PlusGuelmim, testing.data.hex)
H2ORandomForest.PlusGuelmim.prediction <- as.data.frame(H2ORandomForest.PlusGuelmim.prediction.hex)

#-------------------------------------------------------------------------------------------------------#

predict_vars <- colnames(training.data.PlusGuelmim)
predict_vars <- predict_vars[-2] 

#Creating Formula
f <- as.formula(paste(paste('yield', collapse = " + "),'~', paste(predict_vars, collapse = " + ")))


SVMModel <- ksvm(f, data = training.data.PlusGuelmim)
SVMModel.prediction <- predict(SVMModel, testing.data.PlusGuelmim[,predict_vars], type = "response")
SVMModel.prediction <- as.data.frame(SVMModel.prediction)

for(i in seq(1:dim(SVMModel.prediction)[1])){
  if(SVMModel.prediction$V1[i] < 0){
    SVMModel.prediction$V1[i] <- 0.0
  }
}

SVM.PlusGuelmim.prediction <- SVMModel.prediction

#-------------------------------------------------------------------------------------------------------#

predict_vars <- colnames(training.data.PlusGuelmim)
predict_vars <- predict_vars[-2:-1] 

training.data$Year <- as.character(training.data.PlusGuelmim$Year)
training.data$Year <- as.factor(training.data.PlusGuelmim$Year)

#Creating Formula
f <- as.formula(paste(paste('yield', collapse = " + "),'~', paste(predict_vars, collapse = " + ")))

GBM.Model <- gbm(f,data = training.data.PlusGuelmim, n.trees = 1000)
GBM.prediction <- predict(GBM.Model, testing.data.PlusGuelmim[,predict_vars],n.trees = 500, type = "link")
GBM.prediction <- as.data.frame(GBM.prediction)

GBM.PlusGuelmim.prediction  <- GBM.prediction




#####################################################################################################
# FOG NET PLUS AGADIR 
load('RDFiles/FogNetTrainingDataSet_FNPlusAgadir.Rd')


training.data.PlusAgadir$X <- as.Date(training.data.PlusAgadir$X)
testing.data.PlusAgadir$X <- as.Date(testing.data.PlusAgadir$X)


#Training data
training.data.hex<-as.h2o(training.data.PlusAgadir[1:27])
#predict values for the Testing dataset 
testing.data.hex<-as.h2o(testing.data.PlusAgadir[1:26])

#crete H2O model
H2ORandomForest.PlusAgadir <- h2o.randomForest(x = c(1,3:27), y = 2 ,  training_frame = training.data.hex,
                                                model_id='H2ORandomForest.PlusAgadir',ntrees = 1000)

#H2ORandomForest
H2ORandomForest.PlusAgadir.prediction.hex <- h2o.predict(H2ORandomForest.PlusAgadir, testing.data.hex)
H2ORandomForest.PlusAgadir.prediction <- as.data.frame(H2ORandomForest.PlusAgadir.prediction.hex)



#-------------------------------------------------------------------------------------------------------#

predict_vars <- colnames(training.data.PlusAgadir)
predict_vars <- predict_vars[-2] 

#Creating Formula
f <- as.formula(paste(paste('yield', collapse = " + "),'~', paste(predict_vars, collapse = " + ")))


SVMModel <- ksvm(f, data = training.data.PlusAgadir)
SVMModel.prediction <- predict(SVMModel, testing.data.PlusAgadir[,predict_vars], type = "response")
SVMModel.prediction <- as.data.frame(SVMModel.prediction)

for(i in seq(1:dim(SVMModel.prediction)[1])){
  if(SVMModel.prediction$V1[i] < 0){
    SVMModel.prediction$V1[i] <- 0.0
  }
}

SVM.PlusAgadir.prediction <- SVMModel.prediction


#-------------------------------------------------------------------------------------------------------#

predict_vars <- colnames(training.data.PlusAgadir)
predict_vars <- predict_vars[-2:-1] 

training.data$Year <- as.character(training.data.PlusAgadir$Year)
training.data$Year <- as.factor(training.data.PlusAgadir$Year)

#Creating Formula
f <- as.formula(paste(paste('yield', collapse = " + "),'~', paste(predict_vars, collapse = " + ")))

GBM.Model <- gbm(f,data = training.data.PlusAgadir, n.trees = 1000)
GBM.prediction <- predict(GBM.Model, testing.data.PlusAgadir[,predict_vars],n.trees = 500, type = "link")
GBM.prediction <- as.data.frame(GBM.prediction)

GBM.PlusAgadir.prediction  <- GBM.prediction



#####################################################################################################
# FOG NET PLUS Nearby Airports (SIDI AND GUELMIM) 
load('RDFiles/FogNetTrainingDataSet_FNPlusNearby.Rd')

training.data.PlusNearby$X <- as.Date(training.data.PlusNearby$X)
testing.data.PlusNearby$X <- as.Date(testing.data.PlusNearby$X)


#Training data
training.data.hex<-as.h2o(training.data.PlusNearby[1:51])
#predict values for the Testing dataset 
testing.data.hex<-as.h2o(testing.data.PlusNearby[1:50])

#crete H2O model
H2ORandomForest.PlusNearby <- h2o.randomForest(x = c(1,3:51), y = 2 ,  training_frame = training.data.hex,
                                               model_id='H2ORandomForest.PlusNearby',ntrees = 1000)

#H2ORandomForest
H2ORandomForest.PlusNearby.prediction.hex <- h2o.predict(H2ORandomForest.PlusNearby, testing.data.hex)
H2ORandomForest.PlusNearby.prediction <- as.data.frame(H2ORandomForest.PlusNearby.prediction.hex)


#-------------------------------------------------------------------------------------------------------#

predict_vars <- colnames(training.data.PlusNearby)
predict_vars <- predict_vars[-2] 

#Creating Formula
f <- as.formula(paste(paste('yield', collapse = " + "),'~', paste(predict_vars, collapse = " + ")))


SVMModel <- ksvm(f, data = training.data.PlusNearby)
SVMModel.prediction <- predict(SVMModel, testing.data.PlusNearby[,predict_vars], type = "response")
SVMModel.prediction <- as.data.frame(SVMModel.prediction)

for(i in seq(1:dim(SVMModel.prediction)[1])){
  if(SVMModel.prediction$V1[i] < 0){
    SVMModel.prediction$V1[i] <- 0.0
  }
}

SVM.PlusNearby.prediction <- SVMModel.prediction

#-------------------------------------------------------------------------------------------------------#

predict_vars <- colnames(training.data.PlusNearby)
predict_vars <- predict_vars[-2:-1] 

training.data$Year <- as.character(training.data.PlusNearby$Year)
training.data$Year <- as.factor(training.data.PlusNearby$Year)

#Creating Formula
f <- as.formula(paste(paste('yield', collapse = " + "),'~', paste(predict_vars, collapse = " + ")))

GBM.Model <- gbm(f,data = training.data.PlusNearby, n.trees = 1000)
GBM.prediction <- predict(GBM.Model, testing.data.PlusNearby[,predict_vars],n.trees = 500, type = "link")
GBM.prediction <- as.data.frame(GBM.prediction)

GBM.PlusNearby.prediction  <- GBM.prediction


#####################################################################################################
# FOG NET PLUS ALL Airports
load('RDFiles/FogNetTrainingDataSet_FNPlusAll.Rd')

training.data.PlusAll$X <- as.Date(training.data.PlusAll$X)
testing.data.PlusAll$X <- as.Date(testing.data.PlusAll$X)


#Training data
training.data.hex<-as.h2o(training.data.PlusAll[1:63])
#predict values for the Testing dataset 
testing.data.hex<-as.h2o(testing.data.PlusAll[1:62])

#crete H2O model
H2ORandomForest.PlusAll <- h2o.randomForest(x = c(1,3:63), y = 2 ,  training_frame = training.data.hex,
                                               model_id='H2ORandomForest.PlusAll',ntrees = 1000)

#H2ORandomForest
H2ORandomForest.PlusAll.prediction.hex <- h2o.predict(H2ORandomForest.PlusAll, testing.data.hex)
H2ORandomForest.PlusAll.prediction <- as.data.frame(H2ORandomForest.PlusAll.prediction.hex)

#-------------------------------------------------------------------------------------------------------#

predict_vars <- colnames(training.data.PlusAll)
predict_vars <- predict_vars[-2] 

#Creating Formula
f <- as.formula(paste(paste('yield', collapse = " + "),'~', paste(predict_vars, collapse = " + ")))


SVMModel <- ksvm(f, data = training.data.PlusAll)
SVMModel.prediction <- predict(SVMModel, testing.data.PlusAll[,predict_vars], type = "response")
SVMModel.prediction <- as.data.frame(SVMModel.prediction)

for(i in seq(1:dim(SVMModel.prediction)[1])){
  if(SVMModel.prediction$V1[i] < 0){
    SVMModel.prediction$V1[i] <- 0.0
  }
}

SVM.PlusAll.prediction <- SVMModel.prediction


#-------------------------------------------------------------------------------------------------------#

predict_vars <- colnames(training.data.PlusAll)
predict_vars <- predict_vars[-2:-1] 

training.data$Year <- as.character(training.data.PlusAll$Year)
training.data$Year <- as.factor(training.data.PlusAll$Year)

#Creating Formula
f <- as.formula(paste(paste('yield', collapse = " + "),'~', paste(predict_vars, collapse = " + ")))

GBM.Model <- gbm(f,data = training.data.PlusAll, n.trees = 1000)
GBM.prediction <- predict(GBM.Model, testing.data.PlusAll[,predict_vars],n.trees = 500, type = "link")
GBM.prediction <- as.data.frame(GBM.prediction)

GBM.PlusAll.prediction   <- GBM.prediction


############################################################################################################
#Model To Identify the Yield Level

#Load the data
load('RDFiles/FogNetTrainingDataSet_FNPlusNearby.Rd')

training.data <- training.data.PlusNearby
testing.data <- testing.data.PlusNearby

#Calculate Yield level 
training.data$yieldlevel <- floor(training.data$yield )
training.data$yieldlevel <- as.factor(training.data$yieldlevel )

#Convert to Date
training.data$X <- as.Date(training.data$X)
testing.data$X <- as.Date(testing.data$X)


#Training data
training.data.hex<-as.h2o(training.data)
#predict values for the Testing dataset 
testing.data.hex<-as.h2o(testing.data)

#crete H2O model
#H2ODeepLearning <- h2o.deeplearning(x = predlist, y = pred ,  training_frame = training.data.hex,
#                                    model_id='H2ODeepLearning', epochs = 100,
#                                    hidden = c(200,200,200,200,200))


H2ODeepLearning.level.prediction.hex <- h2o.predict(H2ODeepLearning, testing.data.hex)
H2ODeepLearning.level.prediction <- as.data.frame(H2ODeepLearning.level.prediction.hex)
H2ODeepLearning.level.prediction <- as.data.frame(H2ODeepLearning.level.prediction$predict)

#xxx<- summary(H2ORandomForest.OnlyFogNet.level.prediction)
#xxx<- as.data.frame(xxx)
#xxx <- cbind(xxx,as.integer(rownames(xxx)))

#####################################################################################################

BestScorePrediction_3.8 <-  read.csv('Submissions/H2ORandomForest_InitialData.csv', stringsAsFactors = F,na.strings = c("", " "))
BestScorePrediction_3.7 <-  read.csv('Submissions/H2ORandomForest_Ensemble_TSImpute2.csv', stringsAsFactors = F,na.strings = c("", " "))


load('RDFiles/FogNetTrainingDataSet_OnlyFNData.Rd')
#Create ID 
testing.data$id<-1:dim(testing.data)[1]
H2ORandomForest.OnlyFogNet.prediction$id<-1:dim(H2ORandomForest.OnlyFogNet.prediction)[1]
H2ORandomForest.PlusSidi.prediction$id<-1:dim(H2ORandomForest.PlusSidi.prediction)[1]
H2ORandomForest.PlusGuelmim.prediction$id<-1:dim(H2ORandomForest.PlusGuelmim.prediction)[1]
H2ORandomForest.PlusAgadir.prediction$id<-1:dim(H2ORandomForest.PlusAgadir.prediction)[1]
H2ORandomForest.PlusNearby.prediction$id<-1:dim(H2ORandomForest.PlusNearby.prediction)[1]
H2ORandomForest.PlusAll.prediction$id<-1:dim(H2ORandomForest.PlusAll.prediction)[1]

SVM.OnlyFogNet.prediction$id<-1:dim(SVM.OnlyFogNet.prediction)[1]
SVM.PlusSidi.prediction$id<-1:dim(SVM.PlusSidi.prediction)[1]
SVM.PlusGuelmim.prediction$id<-1:dim(SVM.PlusGuelmim.prediction)[1]
SVM.PlusAgadir.prediction$id<-1:dim(SVM.PlusAgadir.prediction)[1]
SVM.PlusNearby.prediction$id<-1:dim(SVM.PlusNearby.prediction)[1]
SVM.PlusAll.prediction$id<-1:dim(SVM.PlusAll.prediction)[1]


GBM.OnlyFogNet.prediction$id<-1:dim(GBM.OnlyFogNet.prediction)[1]
GBM.PlusSidi.prediction$id<-1:dim(GBM.PlusSidi.prediction)[1]
GBM.PlusGuelmim.prediction$id<-1:dim(GBM.PlusGuelmim.prediction)[1]
GBM.PlusAgadir.prediction$id<-1:dim(GBM.PlusAgadir.prediction)[1]
GBM.PlusNearby.prediction$id<-1:dim(GBM.PlusNearby.prediction)[1]
GBM.PlusAll.prediction$id<-1:dim(GBM.PlusAll.prediction)[1]

H2ODeepLearning.level.prediction$id<-1:dim(H2ODeepLearning.level.prediction)[1]


BestScorePrediction_3.8$id<-1:dim(BestScorePrediction_3.8)[1]
BestScorePrediction_3.7$id<-1:dim(BestScorePrediction_3.7)[1]


#Group all Predictions
PREDICTIONS <- testing.data[,c('id','X')]
#H2ORandomForest.OnlyFogNet.prediction
PREDICTIONS <- data.frame(merge(PREDICTIONS,H2ORandomForest.OnlyFogNet.prediction,by="id"))
names(PREDICTIONS)[names(PREDICTIONS)=="predict"] <- "H2ORandomForest.OnlyFogNet.prediction"

#H2ORandomForest.PlusSidi.prediction
PREDICTIONS <- data.frame(merge(PREDICTIONS,H2ORandomForest.PlusSidi.prediction,by="id"))
names(PREDICTIONS)[names(PREDICTIONS)=="predict"] <- "H2ORandomForest.PlusSidi.prediction"


#H2ORandomForest.PlusGuelmim.prediction
PREDICTIONS <- data.frame(merge(PREDICTIONS,H2ORandomForest.PlusGuelmim.prediction,by="id"))
names(PREDICTIONS)[names(PREDICTIONS)=="predict"] <- "H2ORandomForest.PlusGuelmim.prediction"


#H2ORandomForest.PlusAgadir.prediction
PREDICTIONS <- data.frame(merge(PREDICTIONS,H2ORandomForest.PlusAgadir.prediction,by="id"))
names(PREDICTIONS)[names(PREDICTIONS)=="predict"] <- "H2ORandomForest.PlusAgadir.prediction"


#H2ORandomForest.PlusNearby.prediction
PREDICTIONS <- data.frame(merge(PREDICTIONS,H2ORandomForest.PlusNearby.prediction,by="id"))
names(PREDICTIONS)[names(PREDICTIONS)=="predict"] <- "H2ORandomForest.PlusNearby.prediction"


#H2ORandomForest.PlusAll.prediction
PREDICTIONS <- data.frame(merge(PREDICTIONS,H2ORandomForest.PlusAll.prediction,by="id"))
names(PREDICTIONS)[names(PREDICTIONS)=="predict"] <- "H2ORandomForest.PlusAll.prediction"


#SVM.OnlyFogNet.prediction
PREDICTIONS <- data.frame(merge(PREDICTIONS,SVM.OnlyFogNet.prediction,by="id"))
names(PREDICTIONS)[names(PREDICTIONS)=="V1"] <- "SVM.OnlyFogNet.prediction"


#SVM.PlusSidi.prediction
PREDICTIONS <- data.frame(merge(PREDICTIONS,SVM.PlusSidi.prediction,by="id"))
names(PREDICTIONS)[names(PREDICTIONS)=="V1"] <- "SVM.PlusSidi.prediction"
  

#SVM.PlusGuelmim.prediction
PREDICTIONS <- data.frame(merge(PREDICTIONS,SVM.PlusGuelmim.prediction,by="id"))
names(PREDICTIONS)[names(PREDICTIONS)=="V1"] <- "SVM.PlusGuelmim.prediction"


#SVM.PlusAgadir.prediction
PREDICTIONS <- data.frame(merge(PREDICTIONS,SVM.PlusAgadir.prediction,by="id"))
names(PREDICTIONS)[names(PREDICTIONS)=="V1"] <- "SVM.PlusAgadir.prediction"

#SVM.PlusNearby.prediction
PREDICTIONS <- data.frame(merge(PREDICTIONS,SVM.PlusNearby.prediction,by="id"))
names(PREDICTIONS)[names(PREDICTIONS)=="V1"] <- "SVM.PlusNearby.prediction"

#SVM.PlusAll.prediction
PREDICTIONS <- data.frame(merge(PREDICTIONS,SVM.PlusAll.prediction,by="id"))
names(PREDICTIONS)[names(PREDICTIONS)=="V1"] <- "SVM.PlusAll.prediction"


#GBM.OnlyFogNet.prediction
PREDICTIONS <- data.frame(merge(PREDICTIONS,GBM.OnlyFogNet.prediction,by="id"))
names(PREDICTIONS)[names(PREDICTIONS)=="GBM.prediction"] <- "GBM.OnlyFogNet.prediction"


#GBM.PlusSidi.prediction
PREDICTIONS <- data.frame(merge(PREDICTIONS,GBM.PlusSidi.prediction,by="id"))
names(PREDICTIONS)[names(PREDICTIONS)=="GBM.prediction"] <- "GBM.PlusSidi.prediction"

#GBM.PlusGuelmim.prediction
PREDICTIONS <- data.frame(merge(PREDICTIONS,GBM.PlusGuelmim.prediction,by="id"))
names(PREDICTIONS)[names(PREDICTIONS)=="GBM.prediction"] <- "GBM.PlusGuelmim.prediction"

#GBM.PlusAgadir.prediction
PREDICTIONS <- data.frame(merge(PREDICTIONS,GBM.PlusAgadir.prediction,by="id"))
names(PREDICTIONS)[names(PREDICTIONS)=="GBM.prediction"] <- "GBM.PlusAgadir.prediction"


#GBM.PlusNearby.prediction
PREDICTIONS <- data.frame(merge(PREDICTIONS,GBM.PlusNearby.prediction,by="id"))
names(PREDICTIONS)[names(PREDICTIONS)=="GBM.prediction"] <- "GBM.PlusNearby.prediction"

#GBM.PlusAll.prediction
PREDICTIONS <- data.frame(merge(PREDICTIONS,GBM.PlusAll.prediction,by="id"))
names(PREDICTIONS)[names(PREDICTIONS)=="GBM.prediction"] <- "GBM.PlusAll.prediction"

#H2ORandomForest.OnlyFogNet.level.prediction
PREDICTIONS <- data.frame(merge(PREDICTIONS,H2ODeepLearning.level.prediction,by="id"))
names(PREDICTIONS)[names(PREDICTIONS)=="H2ODeepLearning.level.prediction.predict"] <- "H2ODeepLearning.level.prediction"


#BestScorePrediction_3.8
PREDICTIONS <- data.frame(merge(PREDICTIONS,BestScorePrediction_3.8[,c('id','yield')],by="id"))
names(PREDICTIONS)[names(PREDICTIONS)=="yield"] <- "BestScorePrediction_3.8"

#BestScorePrediction_3.7
PREDICTIONS <- data.frame(merge(PREDICTIONS,BestScorePrediction_3.7[,c('id','yield')],by="id"))
names(PREDICTIONS)[names(PREDICTIONS)=="yield"] <- "BestScorePrediction_3.7"


#Create different Data sets
save(PREDICTIONS,file='RDFiles/FogNetPredictions.Rd')

load('RDFiles/FogNetPredictions.Rd')

##################################################################################################
#Code to get the min value of each prediction

PREDICTIONS$FinalPrediction <- NULL 
PREDICTIONS$FinalPrediction <- 0
for (i in 1:dim(PREDICTIONS)[1]){
#for (i in 1:5){
  #predict.value <- 41.339
  predict.level <- as.character(PREDICTIONS$H2ODeepLearning.level.prediction[i])
  predict.level <- as.integer(predict.level)
  predict.level <- 0.0
  
  list <- PREDICTIONS[i,c('H2ORandomForest.OnlyFogNet.prediction','H2ORandomForest.PlusSidi.prediction',
                         'H2ORandomForest.PlusGuelmim.prediction','H2ORandomForest.PlusAgadir.prediction',
                         'H2ORandomForest.PlusNearby.prediction','H2ORandomForest.PlusAll.prediction',
                         'SVM.OnlyFogNet.prediction','SVM.PlusSidi.prediction','SVM.PlusGuelmim.prediction',
                         'SVM.PlusAgadir.prediction','SVM.PlusNearby.prediction','SVM.PlusAll.prediction')]
                         #'GBM.OnlyFogNet.prediction','GBM.PlusSidi.prediction','GBM.PlusGuelmim.prediction',
                         #'GBM.PlusAgadir.prediction','GBM.PlusNearby.prediction','GBM.PlusAll.prediction',
                         #'BestScorePrediction_3.8','BestScorePrediction_3.7')]
  list <- sort(list)
  predict.value <- ifelse(findInterval(predict.level, list)==0,list[1],list[findInterval(predict.level, list)])
  
  PREDICTIONS$FinalPrediction[i]<- predict.value[[1]][1]
  
  if((predict.value[[1]][1]) <  0.00696 ){
    #print('missmatch')
    PREDICTIONS$FinalPrediction[i] <- 0
    #PREDICTIONS$FinalPrediction[i] <- PREDICTIONS$H2ORandomForest.OnlyFogNet.level.prediction[i]
  }
  
  #if((predict.value[[1]][1] > (predict.level + 2)) || (predict.value[[1]][1] < (predict.level - 2)) ){
  #  #print('missmatch')
  #  PREDICTIONS$FinalPrediction[i] <- PREDICTIONS$H2ODeepLearning.level.prediction[i]
  #}
  

}


colnames(PREDICTIONS)

#Write to Submission

submission.data <- data.frame('Date' = PREDICTIONS$X, yield = PREDICTIONS$FinalPrediction)

write.csv(submission.data, file = 'Submissions/RandomForest Model 3.csv', row.names = F)

###################################################################################################
#Compare mechanism
#Factor Variables
factor_vars <- c('H2ORandomForest.OnlyFogNet.prediction','H2ORandomForest.PlusSidi.prediction',
                 'H2ORandomForest.PlusGuelmim.prediction','H2ORandomForest.PlusAgadir.prediction',
                 'H2ORandomForest.PlusNearby.prediction','H2ORandomForest.PlusAll.prediction',
                 'SVM.OnlyFogNet.prediction','SVM.PlusSidi.prediction','SVM.PlusGuelmim.prediction',
                 'SVM.PlusAgadir.prediction','SVM.PlusNearby.prediction','SVM.PlusAll.prediction',
                 'GBM.OnlyFogNet.prediction','GBM.PlusSidi.prediction','GBM.PlusGuelmim.prediction',
                 'GBM.PlusAgadir.prediction','GBM.PlusNearby.prediction','GBM.PlusAll.prediction',
                 #'H2ODeepLearning.level.prediction',
                 'BestScorePrediction_3.8','BestScorePrediction_3.7','FinalPrediction')
#Round Values
PREDICTIONS[factor_vars] <- lapply(PREDICTIONS[factor_vars], function(x) floor(x))
#Factor Values
PREDICTIONS[factor_vars] <- lapply(PREDICTIONS[factor_vars], function(x) as.factor(x))

#PREDICTIONS$BestScorePrediction2 <- floor(PREDICTIONS$BestScorePrediction2)
#PREDICTIONS$BestScorePrediction2 <- as.factor(PREDICTIONS$BestScorePrediction2)
summary(PREDICTIONS)


xxx<- summary(PREDICTIONS$FinalPrediction)
yyy<- summary(PREDICTIONS$H2ODeepLearning.level.prediction)

xxx<- as.data.frame(xxx)
yyy<- as.data.frame(yyy)

xxx <- cbind(xxx,as.integer(rownames(xxx)))
yyy <- cbind(yyy,as.integer(rownames(yyy)))

names(xxx)[names(xxx)=="as.integer(rownames(xxx))"] <- "id"
names(yyy)[names(yyy)=="as.integer(rownames(yyy))"] <- "id"

names(xxx)[names(xxx)=="xxx"] <- "FinalPrediction"
names(yyy)[names(yyy)=="yyy"] <- "Level"

zzz <- merge(xxx,yyy,by='id',all=T)

###################################################################################################

