#memuat dan membagi data
str(iris)
set.seed(1234)
ind <- sample(2,nrow(iris),replace=TRUE,prob=c(0.7,0.3))
trainData <- iris[ind==1,]
testingData <- iris[ind==2,]
View(trainData)
View(testingData)

#membangun decision tree
library(party)
myFormula <- Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width
iris_ctree <- ctree(myFormula,data=trainData)

#mengecek hasil prediksi
table(predict(iris_ctree),trainData$Species)

#plot
print(iris_ctree)
plot(iris_ctree)
plot(iris_ctree,type='simple')

#next data
#memuat data
data('bodyfat', package='TH.data')
dim(bodyfat)
attributes(bodyfat)
bodyfat[1:5,]

#memuat kelompok sampel trainData dan testingData
set.seed(1234)
ind<-sample(2,nrow(bodyfat),replace=TRUE,prob=c(0.7,0.3))
bodyfatTrain <- bodyfat[ind==1,]
bodyfatTesting <- bodyfat[ind==2,]

#melatih pohon keputusan
library(rpart)
formulaBF <- DEXfat~age+waistcirc+hipcirc+elbowbreadth+kneebreadth
bodyfat_rpart <- rpart(formulaBF,data=bodyfatTrain,control=rpart.control(minsplit=10))
attributes(bodyfat_rpart)

plot(bodyfat_rpart)
text(bodyfat_rpart,use.n=T)

opt <- which.min(bodyfat_rpart$cptable[,'xerror'])
cp <- bodyfat_rpart$cptable[opt,'CP']
bodyfat_prune <- prune(bodyfat_rpart,cp=cp)
print(bodyfat_prune)
plot(bodyfat_prune)
text(bodyfat_prune,use.n=T)

DEXfat_pred <- predict(bodyfat_prune,newdata=bodyfatTesting)
xlim <- range(bodyfat$DEXfat)
plot(DEXfat_pred~DEXfat,data=bodyfatTesting,xlab='Observed',ylab='Predicted',ylim=xlim,xlim=xlim)
abline(a=0,b=1)