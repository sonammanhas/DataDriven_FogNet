library(mice)
library(imputeTS)


training.yeild <- read.csv('Target Variable Water Yield.csv', stringsAsFactors = F,na.strings = c("", " "))
testing.yeild <- read.csv('submission_format.csv', stringsAsFactors = F,na.strings = c("", " "))
testing.yeild$yield <- -1
full.data.yeild <- rbind(training.yeild,testing.yeild)
full.data.yeild.order <- full.data.yeild[with(full.data.yeild, order(X)), ]
full.data.yeild.order[full.data.yeild.order==-1] <- NA




training.data <- read.csv('Training set Microclimate - 2 hour intervals.csv', stringsAsFactors = F,na.strings = c("", " "))
testing.data <- read.csv('Test set Microclimate - 2 hour intervals.csv', stringsAsFactors = F,na.strings = c("", " "))


training.data5 <- read.csv('Training set Microclimate - 5 minute intervals.csv', stringsAsFactors = F,na.strings = c("", " "))
testing.data5 <- read.csv('Test set Microclimate - 5 min intervals.csv', stringsAsFactors = F,na.strings = c("", " "))

#colnames(training.data)
#colnames(training.data5)

full.data.other <- rbind(training.data,testing.data)
full.data <- rbind(training.data5,testing.data5)

full.data <- data.frame(merge(full.data,full.data.other,by="X",all = T))

full.data <- full.data[with(full.data, order(X)), ]

# Missing Values
for(i in seq(from=1,to=dim(full.data)[1])){
  
  #Humidity
  if(full.data$humidity.x[i]==0 || is.na(full.data$humidity.x[i]) == TRUE){full.data$humidity.x[i] <- full.data$humidity.y[i]}
  if(full.data$humidity.x[i]==0 & is.na(full.data$humidity.x[i]) == FALSE){full.data$humidity.x[i] <- -1}
  
  #temp
  if(full.data$temp.x[i]==0 || is.na(full.data$temp.x[i]) == TRUE){full.data$temp.x[i] <- full.data$temp.y[i]}
  if(full.data$temp.x[i]==0 & is.na(full.data$temp.x[i]) == FALSE){full.data$temp.x[i] <- -1}
  
  #leafwet_lwscnt
  if(full.data$leafwet_lwscnt.x[i]==0 || is.na(full.data$leafwet_lwscnt.x[i]) == TRUE){full.data$leafwet_lwscnt.x[i] <- full.data$leafwet_lwscnt.y[i]}
  if(full.data$leafwet_lwscnt.x[i]==0 & is.na(full.data$leafwet_lwscnt.x[i]) == FALSE){full.data$leafwet_lwscnt.x[i] <- -1}
  
  #gusts_ms
  if(full.data$gusts_ms.x[i]==0 || is.na(full.data$gusts_ms.x[i]) == TRUE){full.data$gusts_ms.x[i] <- full.data$gusts_ms.y[i]}
  if(full.data$gusts_ms.x[i]==0 & is.na(full.data$gusts_ms.x[i]) == FALSE){full.data$gusts_ms.x[i] <- -1}
  
  #wind_dir
  if(full.data$wind_dir.x[i]==0 || is.na(full.data$wind_dir.x[i]) == TRUE){full.data$wind_dir.x[i] <- full.data$wind_dir.y[i]}
  if(full.data$wind_dir.x[i]==0 & is.na(full.data$wind_dir.x[i]) == FALSE){full.data$wind_dir.x[i] <- -1}
  
  #wind_ms
  if(full.data$wind_ms.x[i]==0 || is.na(full.data$wind_ms.x[i]) == TRUE){full.data$wind_ms.x[i] <- full.data$wind_ms.y[i]}
  if(full.data$wind_ms.x[i]==0 & is.na(full.data$wind_ms.x[i]) == FALSE){full.data$wind_ms.x[i] <- -1}
  
  
  #percip_mm
  if(full.data$percip_mm.x[i]==0 || is.na(full.data$percip_mm.x[i]) == TRUE){full.data$percip_mm.x[i] <- full.data$percip_mm.y[i]}
  #leafwet450_min
  if(full.data$leafwet450_min.x[i]==0 || is.na(full.data$leafwet450_min.x[i]) == TRUE){full.data$leafwet450_min.x[i] <- full.data$leafwet450_min.y[i]}
  #leafwet460_min
  if(full.data$leafwet460_min.x[i]==0 || is.na(full.data$leafwet460_min.x[i]) == TRUE){full.data$leafwet460_min.x[i] <- full.data$leafwet460_min.y[i]}
  
  
  
  
  
}

full.data <- full.data[,c('X','percip_mm.x','humidity.x','temp.x','leafwet450_min.x','leafwet460_min.x',
                          'leafwet_lwscnt.x','gusts_ms.x','wind_dir.x','wind_ms.x')]

names(full.data)[names(full.data)=="percip_mm.x"] <- "percip_mm"
names(full.data)[names(full.data)=="humidity.x"] <- "humidity"
names(full.data)[names(full.data)=="temp.x"] <- "temp"
names(full.data)[names(full.data)=="leafwet450_min.x"] <- "leafwet450_min"
names(full.data)[names(full.data)=="leafwet460_min.x"] <- "leafwet460_min"
names(full.data)[names(full.data)=="leafwet_lwscnt.x"] <- "leafwet_lwscnt"
names(full.data)[names(full.data)=="gusts_ms.x"] <- "gusts_ms"
names(full.data)[names(full.data)=="wind_dir.x"] <- "wind_dir"
names(full.data)[names(full.data)=="wind_ms.x"] <- "wind_ms"

full.data[full.data==-1] <- NA

full.data <- data.frame(merge(full.data.yeild.order,full.data,by="X",all = T))
full.data <- full.data[with(full.data, order(X)), ]


#full.data <- data.frame(merge(full.data.yeild.order,full.data,by="X",all.x = T))
#full.data$yield <- NULL


#Extract Date Time as Factors

full.data$Year <- substr(full.data$X,1,4)
full.data$Month <- substr(full.data$X,6,7)
full.data$Day <- substr(full.data$X,9,10)
full.data$Hour <- substr(full.data$X,12,13)
full.data$Min <- substr(full.data$X,15,16)


full.data$Year <- as.factor(full.data$Year)
full.data$Month <- as.factor(full.data$Month)
full.data$Day <- as.factor(full.data$Day)
full.data$Hour <- as.factor(full.data$Hour)
full.data$Min <- as.factor(full.data$Min)

summary(full.data)

#TS Impute
#for(i in seq(from=2,to=dim(full.data)[2])){
#  x <- ts(full.data[,i])
#  full.data[,i] <- na.interpolation(x)
#}

full.data.nafill <- mice(full.data)
full.data <- complete(full.data.nafill,1)


full.data$Min <- NULL

summary(full.data)

#save
#save(full.data,file='RDFiles/FogNetTrainingDataSet.Rd')
#save(full.data,file='RDFiles/FogNetTrainingDataSet2.Rd')



full.data$yield <- NULL

######################################################################################################
#ONLY FogNet
#Merge
testing.data <- data.frame(merge(testing.yeild,full.data,by="X",all.x=TRUE))
testing.data$yield <- NULL
#summary(testing.data)
#?merge

#Merge
training.data <- data.frame(merge(training.yeild,full.data,by="X",all.x=TRUE))

#Create different Data sets
save(training.data,testing.data,file='RDFiles/FogNetTrainingDataSet_OnlyFNData.Rd')


######################################################################################################
#Load imputed airport TS data
load('RDFiles/FogNetAirportTSImpute.Rd')
######################################################################################################

#Fog NEt Plus Sidi
full.data.PlusSidi <- data.frame(merge(full.data,Sidi.data.full,by="X",all.x = T))

#Merge
testing.data.PlusSidi <- data.frame(merge(testing.yeild,full.data.PlusSidi,by="X",all.x=TRUE))
testing.data.PlusSidi$yield <- NULL


#Merge
training.data.PlusSidi <- data.frame(merge(training.yeild,full.data.PlusSidi,by="X",all.x=TRUE))

#Create different Data sets
save(testing.data.PlusSidi,training.data.PlusSidi,file='RDFiles/FogNetTrainingDataSet_FNPlusSidi.Rd')

######################################################################################################

#FogNet Plus Guelmim
full.data.PlusGuelmim <- data.frame(merge(full.data,Guelmim.data.full,by="X",all.x = T))

#Merge
testing.data.PlusGuelmim <- data.frame(merge(testing.yeild,full.data.PlusGuelmim,by="X",all.x=TRUE))
testing.data.PlusGuelmim$yield <- NULL


#Merge
training.data.PlusGuelmim <- data.frame(merge(training.yeild,full.data.PlusGuelmim,by="X",all.x=TRUE))

#Create different Data sets
save(testing.data.PlusGuelmim,training.data.PlusGuelmim,file='RDFiles/FogNetTrainingDataSet_FNPlusGuelmim.Rd')


######################################################################################################

#FogNet Plus Agadir
full.data.PlusAgadir <- data.frame(merge(full.data,Agadir.data.full,by="X",all.x = T))

#Merge
testing.data.PlusAgadir <- data.frame(merge(testing.yeild,full.data.PlusAgadir,by="X",all.x=TRUE))
testing.data.PlusAgadir$yield <- NULL


#Merge
training.data.PlusAgadir <- data.frame(merge(training.yeild,full.data.PlusAgadir,by="X",all.x=TRUE))

#Create different Data sets
save(testing.data.PlusAgadir,training.data.PlusAgadir,file='RDFiles/FogNetTrainingDataSet_FNPlusAgadir.Rd')

######################################################################################################

#FogNet Plus Nearby
full.data.PlusNearby <- data.frame(merge(full.data,Sidi.data.full,by="X",all.x = T))
full.data.PlusNearby <- data.frame(merge(full.data.PlusNearby,Guelmim.data.full,by="X",all.x = T))


#Merge
testing.data.PlusNearby <- data.frame(merge(testing.yeild,full.data.PlusNearby,by="X",all.x=TRUE))
testing.data.PlusNearby$yield <- NULL


#Merge
training.data.PlusNearby <- data.frame(merge(training.yeild,full.data.PlusNearby,by="X",all.x=TRUE))

#Create different Data sets
save(testing.data.PlusNearby,training.data.PlusNearby,file='RDFiles/FogNetTrainingDataSet_FNPlusNearby.Rd')


######################################################################################################

#FogNet Plus All
full.data.PlusAll <- data.frame(merge(full.data,Sidi.data.full,by="X",all.x = T))
full.data.PlusAll <- data.frame(merge(full.data.PlusAll,Guelmim.data.full,by="X",all.x = T))
full.data.PlusAll <- data.frame(merge(full.data.PlusAll,Agadir.data.full,by="X",all.x = T))

#Merge
testing.data.PlusAll <- data.frame(merge(testing.yeild,full.data.PlusAll,by="X",all.x=TRUE))
testing.data.PlusAll$yield <- NULL


#Merge
training.data.PlusAll <- data.frame(merge(training.yeild,full.data.PlusAll,by="X",all.x=TRUE))

#Create different Data sets
save(testing.data.PlusAll,training.data.PlusAll,file='RDFiles/FogNetTrainingDataSet_FNPlusAll.Rd')



######################################################################################################

