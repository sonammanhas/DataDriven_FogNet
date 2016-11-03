#Libraries
#TS Impute
#install.packages("imputeTS")
#library(imputeTS)
library(mice)




training.yeild <- read.csv('Target Variable Water Yield.csv', stringsAsFactors = F,na.strings = c("", " "))
testing.yeild <- read.csv('submission_format.csv', stringsAsFactors = F,na.strings = c("", " "))
testing.yeild$yield <- -1

full.data.yeild <- rbind(training.yeild,testing.yeild)
full.data.yeild.order <- full.data.yeild[with(full.data.yeild, order(X)), ]
#full.data.yeild.order$X <- as.POSIXlt(full.data.yeild.order$X)


#########################################################################################
#AGADIR DATA
Agadir.data <-  read.csv('Macroclimate Agadir Airport.csv', stringsAsFactors = F,na.strings = c("", " "))
colnames(Agadir.data)
names(Agadir.data)[1]<-"X"
names(Agadir.data)[2]<-"Agadir.T"
names(Agadir.data)[3]<-"Agadir.P0"
names(Agadir.data)[4]<-"Agadir.P"
names(Agadir.data)[5]<-"Agadir.U"
names(Agadir.data)[6]<-"Agadir.DD"
names(Agadir.data)[7]<-"Agadir.Ff"
names(Agadir.data)[8]<-"Agadir.ff10"
names(Agadir.data)[9]<-"Agadir.WW"
names(Agadir.data)[10]<-"Agadir.W.W."
names(Agadir.data)[11]<-"Agadir.C"
names(Agadir.data)[12]<-"Agadir.VV"
names(Agadir.data)[13]<-"Agadir.Td"
#Agadir.data$X <- as.POSIXlt(Agadir.data$X)

summary(Agadir.data)

#Fill NA Values
Agadir.data$Agadir.ff10[is.na(Agadir.data$Agadir.ff10)] <- 0
Agadir.data$Agadir.WW[is.na(Agadir.data$Agadir.WW)] <- 'Unknown'
Agadir.data$Agadir.W.W.[is.na(Agadir.data$Agadir.W.W.)] <- 'Unknown'


#Factor Variables
factor_vars <- c('Agadir.DD','Agadir.C','Agadir.WW','Agadir.W.W.','Agadir.VV','Agadir.Ff','Agadir.ff10')
Agadir.data[factor_vars] <- lapply(Agadir.data[factor_vars], function(x) as.factor(x))


summary(Agadir.data)

#Complete the Agadir Data for required datetime
Agadir.data.full <- data.frame(merge(full.data.yeild.order,Agadir.data,by="X",all.x = T))
Agadir.data.full$yield <- NULL

#Extract Date Time as Factors

Agadir.data.full$Year <- substr(Agadir.data.full$X,1,4)
Agadir.data.full$Month <- substr(Agadir.data.full$X,6,7)
Agadir.data.full$Day <- substr(Agadir.data.full$X,9,10)
Agadir.data.full$Hour <- substr(Agadir.data.full$X,12,13)


factor_vars <-c('Year','Month','Day','Hour')
Agadir.data.full[factor_vars] <- lapply(Agadir.data.full[factor_vars], function(x) as.factor(x))


summary(Agadir.data.full)

#TS Impute
#for(i in seq(from=2,to=dim(Agadir.data.full)[2])){
#  x <- ts(Agadir.data.full[,i])
#  Agadir.data.full[,i] <- na.interpolation(x)
#}
Agadir.data.full.nafill <- mice(Agadir.data.full,method = 'pmm')
Agadir.data.full <- complete(Agadir.data.full.nafill,1)

#TS Impute
Agadir.data.full$Agadir.P0 <- na.interpolation(ts(Agadir.data.full$Agadir.P0))




###########################################################################################################
#GUELMIM DATA
Guelmim.data <-  read.csv('Macroclimate Guelmim Airport.csv', stringsAsFactors = F,na.strings = c("", " "))
colnames(Guelmim.data)
names(Guelmim.data)[1]<-"X"
names(Guelmim.data)[2]<-"Guelmim.T"
names(Guelmim.data)[3]<-"Guelmim.P0"
names(Guelmim.data)[4]<-"Guelmim.P"
names(Guelmim.data)[5]<-"Guelmim.U"
names(Guelmim.data)[6]<-"Guelmim.DD"
names(Guelmim.data)[7]<-"Guelmim.Ff"
names(Guelmim.data)[8]<-"Guelmim.ff10"
names(Guelmim.data)[9]<-"Guelmim.WW"
names(Guelmim.data)[10]<-"Guelmim.W.W."
names(Guelmim.data)[11]<-"Guelmim.C"
names(Guelmim.data)[12]<-"Guelmim.VV"
names(Guelmim.data)[13]<-"Guelmim.Td"

#Fill NA Values
Guelmim.data$Guelmim.ff10[is.na(Guelmim.data$Guelmim.ff10)] <- 0
Guelmim.data$Guelmim.WW[is.na(Guelmim.data$Guelmim.WW)] <- 'Unknown'
Guelmim.data$Guelmim.W.W.[is.na(Guelmim.data$Guelmim.W.W.)] <- 'Unknown'

#Factor Variables
factor_vars <- c('Guelmim.DD','Guelmim.C','Guelmim.WW','Guelmim.W.W.','Guelmim.VV','Guelmim.Ff','Guelmim.ff10')
Guelmim.data[factor_vars] <- lapply(Guelmim.data[factor_vars], function(x) as.factor(x))


summary(Guelmim.data)

#Complete the Agadir Data for required datetime
Guelmim.data.full <- data.frame(merge(full.data.yeild.order,Guelmim.data,by="X",all.x = T))
Guelmim.data.full$yield <- NULL

#Extract Date Time as Factors

Guelmim.data.full$Year <- substr(Guelmim.data.full$X,1,4)
Guelmim.data.full$Month <- substr(Guelmim.data.full$X,6,7)
Guelmim.data.full$Day <- substr(Guelmim.data.full$X,9,10)
Guelmim.data.full$Hour <- substr(Guelmim.data.full$X,12,13)


factor_vars <-c('Year','Month','Day','Hour')
Guelmim.data.full[factor_vars] <- lapply(Guelmim.data.full[factor_vars], function(x) as.factor(x))


summary(Guelmim.data.full)



#TS Impute
#for(i in seq(from=2,to=dim(Guelmim.data.full)[2])){
#  x <- ts(Guelmim.data.full[,i])
#  Guelmim.data.full[,i] <- na.interpolation(x)
#}

Guelmim.data.full.nafill <- mice(Guelmim.data.full,method = 'pmm')
Guelmim.data.full <- complete(Guelmim.data.full.nafill,1)

#TS Impute
#Guelmim.data.full$Guelmim.P0 <- na.interpolation(ts(Guelmim.data.full$Guelmim.P0))


###########################################################################################################
#SIDI DAtA
Sidi.data <-  read.csv('Macroclimate Sidi Ifni Weather Station.csv', stringsAsFactors = F,na.strings = c("", " "))
colnames(Sidi.data)
names(Sidi.data)[1]<-"X"
names(Sidi.data)[2]<-"Sidi.T"
names(Sidi.data)[3]<-"Sidi.P0"
names(Sidi.data)[4]<-"Sidi.P"
names(Sidi.data)[5]<-"Sidi.Pa"
names(Sidi.data)[6]<-"Sidi.U"
names(Sidi.data)[7]<-"Sidi.DD"
names(Sidi.data)[8]<-"Sidi.Ff"
names(Sidi.data)[9]<-"Sidi.N"
names(Sidi.data)[10]<-"Sidi.WW"
names(Sidi.data)[11]<-"Sidi.W1"
names(Sidi.data)[12]<-"Sidi.W2"
names(Sidi.data)[13]<-"Sidi.Tn"
names(Sidi.data)[14]<-"Sidi.Tx"
names(Sidi.data)[15]<-"Sidi.Cl"
names(Sidi.data)[16]<-"Sidi.Nh"
names(Sidi.data)[17]<-"Sidi.H"
names(Sidi.data)[18]<-"Sidi.Cm"
names(Sidi.data)[19]<-"Sidi.Ch"
names(Sidi.data)[20]<-"Sidi.VV"
names(Sidi.data)[21]<-"Sidi.Td"
names(Sidi.data)[22]<-"Sidi.RRR"
names(Sidi.data)[23]<-"Sidi.tR"
names(Sidi.data)[24]<-"Sidi.E"
names(Sidi.data)[25]<-"Sidi.Tg"
names(Sidi.data)[26]<-"Sidi.E."
names(Sidi.data)[27]<-"Sidi.sss"
colnames(Sidi.data)

summary(Sidi.data)

#Fill NA Values
Sidi.data$Sidi.WW[is.na(Sidi.data$Sidi.WW)] <- 'Unknown'
Sidi.data$Sidi.W1[is.na(Sidi.data$Sidi.W1)] <- 'Unknown'
Sidi.data$Sidi.W2[is.na(Sidi.data$Sidi.W2)] <- 'Unknown'

Sidi.data$Sidi.E. <- NULL
Sidi.data$Sidi.sss <- NULL 
#Sidi.data$Siri.RRR <- NULL 

factor_vars <- c('Sidi.DD','Sidi.Ff','Sidi.N','Sidi.WW','Sidi.W1','Sidi.W2','Sidi.Cl','Sidi.Nh',
                 'Sidi.H','Sidi.Cm','Sidi.Ch','Sidi.tR','Sidi.E','Sidi.Tg','Sidi.RRR')
Sidi.data[factor_vars] <- lapply(Sidi.data[factor_vars], function(x) as.factor(x))


summary(Sidi.data)

#Complete the Agadir Data for required datetime
Sidi.data.full <- data.frame(merge(full.data.yeild.order,Sidi.data,by="X",all.x = T))
Sidi.data.full$yield <- NULL

#Extract Date Time as Factors

Sidi.data.full$Year <- substr(Sidi.data.full$X,1,4)
Sidi.data.full$Month <- substr(Sidi.data.full$X,6,7)
Sidi.data.full$Day <- substr(Sidi.data.full$X,9,10)
Sidi.data.full$Hour <- substr(Sidi.data.full$X,12,13)


factor_vars <-c('Year','Month','Day','Hour')
Sidi.data.full[factor_vars] <- lapply(Sidi.data.full[factor_vars], function(x) as.factor(x))



summary(Sidi.data.full)

#TS Impute
#for(i in seq(from=2,to=dim(Sidi.data.full)[2])){
#  x <- ts(Sidi.data.full[,2])
#  Sidi.data.full[,i] <- na.interpolation(x)
#}

Sidi.data.full.nafill <- mice(Sidi.data.full,method = 'pmm')
Sidi.data.full <- complete(Sidi.data.full.nafill,1)

#TS Impute
Sidi.data.full$Sidi.P <- na.interpolation(ts(Sidi.data.full$Sidi.P))



##REMOVE THE DATE TIME EXTRA COLUMNS
Agadir.data.full$Year <- Guelmim.data.full$Year <- Sidi.data.full$Year <- NULL
Agadir.data.full$Month <- Guelmim.data.full$Month <- Sidi.data.full$Month <- NULL
Agadir.data.full$Day <- Guelmim.data.full$Day <- Sidi.data.full$Day <- NULL
Agadir.data.full$Hour <- Guelmim.data.full$Hour <- Sidi.data.full$Hour <- NULL



save(Sidi.data.full,Guelmim.data.full,Agadir.data.full,file='RDFiles/FogNetAirportTSImpute.Rd')

load('RDFiles/FogNetAirportTSImpute.Rd')