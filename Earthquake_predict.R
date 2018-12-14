

data=read.csv("https://raw.githubusercontent.com/Trion129/EarthquakePrediction/master/database.csv")
View(data)
data<-data[c('Date','Time','Latitude','Longitude','Depth','Magnitude')]
set.seed(1408)
splitIndex <- createDataPartition(fraud$Manipulator, p = .7,list = FALSE, times = 1)
trainData <- data[splitIndex,]
testData <- data[-splitIndex,]
View(trainData)
View(test)
str(data)
#knn model
train_control<- trainControl(method="cv", number=5, savePredictions = TRUE)
model<- train(Magnitude~Latitude+Longitude+Depth,data=trainData, trControl=train_control, method="knn",kmax=5)
print(model)
model
rPred1<- predict(model,testData)

both<-cbind(testData$Magnitude,rPred1)
View(both)


diff<-as.data.frame(abs(testData$Magnitude[1:23257]-rPred1[1:23257]))


accuracy<-(diff/testData$Magnitude[1:23257])*100

error_rate<-sum(accuracy)/23257

accuracy_final<-100-error_rate

accuracy_final


