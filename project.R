setwd("/R/practicalmachinelearning")

pmldata<-read.csv("pml-training.csv")
pmltest<-read.csv("pml-testing.csv")

head(pmldata,10)
dim(pmldata)
summary(pmldata)
pmldata[,160]

head(pmltest,10)
dim(pmltest)
pmltest[,160]

g1<- ggplot(pmldata,aes(x=roll_belt)) + geom_histogram()
g1

plot(pmldata$roll_belt,pmldata$classe)

g1 <- ggplot(pmldata, aes(x = magnet_dumbbell_z, y = magnet_dumbbell_y)) + geom_point(aes(color=classe)) + facet_wrap(~classe) + scale_y_continuous(limits=c(-1000,1000))
g1 <- g1 + labs(title="magnet_dumbbell_z vs. magnet_dumbbell_y")
g1

g2 <- ggplot(pmldata, aes(x = roll_belt, y = yaw_belt)) + geom_point(aes(color=classe)) + facet_wrap(~classe) 
g2 <- g2 + labs(title="roll_belt vs. yaw_belt")
g2

library(caret)
set.seed(2015)
inTraining<-createDataPartition(pmldata$classe,p=.6,list=FALSE)
training<-pmldata[inTraining,]
testing<-pmldata[-inTraining,]

head(training,20)
summary(training)

library(ElemStatLearn)
data(vowel.train)
vowel.train$y<-factor(vowel.train$y)
set.seed(33833)
head(vowel.train)
elemMod<-train(y~.,data=vowel.train,method="rf",prox=TRUE)
dim(vowel.train)
elemMod
elemMod$finalModel
elemimp<-varImp(elemMod,scale=FALSE)
elemimp

pmlMod<-train(classe~.,data=pmldata,method="rf",prox=TRUE)
pmlMod
print(pmlMod$finalModel)
pmlimp<-varImp(pmlMod,scale=FALSE)
pmlimp
plot(pmlimp,top=30)

plot(pmlMod$finalModel)
text(pmlMod$finalModel,use.n=TRUE,all=TRUE,cex=.8)

library(rattle)
fancyRpartPlot(pmlMod$finalModel)

predict(pmlMod,newdata=pmltest)
warnings()

head(pmltest,10)
names(pmltest)
names(pmldata)

sum(is.na(pmldata))
na_count<-sapply(pmldata,function(y) sum(length(which(is.na(y)))))
na_count<-data.frame(na_count)
na_count

dim(pmltest)
na_count_test<-sapply(pmltest,function(y) sum(length(which(is.na(y)))))
na_count_test<-data.frame(na_count_test)
na_count_test

testset<-pmltest[,grepl("^roll|^pitch|^yaw|^total_accel|_x$|_y$|_z$|^problem_id$",names(pmltest))]
testset
dim(testset)
sum(is.na(testset))

pmldataset<-pmldata[,grepl("^roll|^pitch|^yaw|^total_accel|_x$|_y$|_z$|^classe$",names(pmldata))]
dim(pmldataset)
sum(is.na(pmldataset))

modFit<-train(classe~.,data=pmldataset,method="rf",prox=TRUE)
modFit
print(modFit$finalModel)
modimp<-varImp(modFit,scale=FALSE)
modimp
plot(modimp,top=30)

predict(modFit,newdata=pmltest)
answers=predict(modFit,newdata=pmltest)
answers<-lapply(answers,as.character)
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(answers)

plot(modFit, metric="Kappa")
ggplot(modFit)

varImp(modFit,scale=FALSE)
