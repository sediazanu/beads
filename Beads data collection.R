require(randomForest)
library(randomForest)
library(tree)
library(caTools)
library(rpart)

beads1 = read.csv("bead_data_1.csv",header=TRUE)

beads = beads1[,-1]


class = beads[,-(2:5)]
order = beads[,-c(1,3:5)]
genus = beads[,-c(1:2,4:5)]
species = beads[,-c(1:3,5)]
location = beads[,-c(1:4)]


##CLASS
dimbeads=dim(beads)
#N = dim[1]
#q = dim[2]
n = nrow(class)

set.seed(101)
sample = sample(1:n, size = round(0.7*n))
trainclass = class[sample,] 
testclass = class[-sample,]
fitclasstrain <- rpart(class ~ asx+glx+ser+gly+ala+val, method="class", data=trainclass, control =rpart.control(minsplit=10, minbucket=5, cp=0.0001) )
fitclasstest <- rpart(class ~ asx+glx+ser+gly+ala+val, method="class", data=testclass, control =rpart.control(minsplit=10, minbucket=5, cp=0.0001) )
#Grow decision tree
plot(fitclasstrain, uniform=TRUE, main="Decision Tree")
text(fitclasstrain, use.n=TRUE, all=TRUE, cex=0.5)

tree.pred=predict(fitclasstrain,trainclass[,-1],type="class")
table(predicted=tree.pred, true=trainclass$class)

plot(fitclasstest, uniform=TRUE, main="Decision Tree")
text(fitclasstest, use.n=TRUE, all=TRUE, cex=.5)

printcp(fitclasstrain) # display the results
summary(fitclasstrain) # detailed summary of splits

#RANDOMFOREST
trainclass$class = factor(trainclass$class)
rf.beadtrainclass = randomForest(class ~ asx+glx+ser+gly+ala+val, data = trainclass)
rf.beadtrainclass

testclass$class = factor(testclass$class)
rf.beadtestclass = randomForest(class ~ asx+glx+ser+gly+ala+val, data = testclass)
rf.beadtestclass

##ORDER
n1 = nrow(order)

sample1 = sample(1:n1, size = round(0.7*n1))
trainorder = order[sample1,] 
testorder = order[-sample1,]
fitordertrain <- rpart(order ~ asx+glx+ser+gly+ala+val, method="class", data=trainorder )
fitordertest <- rpart(order ~ asx+glx+ser+gly+ala+val, method="class", data=testorder)
#Grow decision tree
plot(fitordertrain, uniform=TRUE, main="Decision Tree")
text(fitordertrain, use.n=TRUE, all=TRUE, cex=.8)

plot(fitordertest, uniform=TRUE, main="Decision Tree")
text(fitordertest, use.n=TRUE, all=TRUE, cex=.8)

#RANDOMFOREST
trainorder$order = factor(trainorder$order)
rf.beadtrainorder = randomForest(order ~ asx+glx+ser+gly+ala+val, data = trainorder)
rf.beadtrainorder

testorder$order = factor(testorder$order)
rf.beadtestorder = randomForest(order ~ asx+glx+ser+gly+ala+val, data = testorder)
rf.beadtestorder


##GENUS
n2 = nrow(genus)

sample2 = sample(1:n2, size = round(0.7*n2))
traingenus = genus[sample2,] 
testgenus = genus[-sample2,]
fitgenustrain <- rpart(Genus ~ asx+glx+ser+gly+ala+val, method="class", data=traingenus )
fitgenustest <- rpart(Genus ~ asx+glx+ser+gly+ala+val, method="class", data=testgenus)
#Grow decision tree
plot(fitgenustrain, uniform=TRUE, main="Decision Tree")
text(fitgenustrain, use.n=TRUE, all=TRUE, cex=.8)

plot(fitgenustest, uniform=TRUE, main="Decision Tree")
text(fitgenustest, use.n=TRUE, all=TRUE, cex=.8)

#RANDOMFOREST
traingenus$Genus = factor(traingenus$Genus)
rf.beadtraingenus = randomForest(Genus~ asx+glx+ser+gly+ala+val, data = traingenus)
rf.beadtraingenus

testgenus$Genus = factor(testgenus$Genus)
rf.beadtestgenus= randomForest(Genus ~ asx+glx+ser+gly+ala+val, data = testgenus)
rf.beadtestgenus


##SPECIES
n3 = nrow(species)

sample3 = sample(1:n3, size = round(0.7*n3))
trainspecies = species[sample3,] 
testspecies = species[-sample3,]
fitspeciestrain <- rpart(Species ~ asx+glx+ser+gly+ala+val, method="class", data=trainspecies )
fitspeciestest <- rpart(Species ~ asx+glx+ser+gly+ala+val, method="class", data=testspecies)
#Grow decision tree
plot(fitspeciestrain, uniform=TRUE, main="Decision Tree")
text(fitspeciestrain, use.n=TRUE, all=TRUE, cex=.8)

plot(fitspeciestest, uniform=TRUE, main="Decision Tree")
text(fitspeciestest, use.n=TRUE, all=TRUE, cex=.8)

#RANDOMFOREST
trainspecies$Species = factor(trainspecies$Species)
rf.beadtrainspecies = randomForest(Species~ asx+glx+ser+gly+ala+val, data = trainspecies)
rf.beadtrainspecies

testspecies$Species = factor(testspecies$Species)
rf.beadtestspecies= randomForest(Species ~ asx+glx+ser+gly+ala+val, data = testspecies)
rf.beadtestspecies

##LOCATION
n4 = nrow(location)

sample4 = sample(1:n4, size = round(0.7*n4))
trainlocation = location[sample4,] 
testlocation = location[-sample4,]
fitlocationtrain <- rpart(Location ~ asx+glx+ser+gly+ala+val, method="class", data=trainlocation )
fitlocationtest <- rpart(Location ~ asx+glx+ser+gly+ala+val, method="class", data=testlocation)
#Grow decision tree
plot(fitlocationtrain, uniform=TRUE, main="Decision Tree")
text(fitlocationtrain, use.n=TRUE, all=TRUE, cex=0.5)



plot(fitlocationtest, uniform=TRUE, main="Decision Tree")
text(fitlocationtest, use.n=TRUE, all=TRUE, cex=.5)
printcp(fitlocationtrain) # display the results
summary(fitlocationtrain) # detailed summary of splits

#RANDOMFOREST
trainlocation$Location = factor(trainlocation$Location)
rf.beadtrainlocation = randomForest(Location ~ asx+glx+ser+gly+ala+val, data = trainlocation)
rf.beadtrainlocation

testlocation$Location = factor(testlocation$Location)
rf.beadtestlocation = randomForest(Location ~ asx+glx+ser+gly+ala+val, data = testlocation)
rf.beadtestlocation
plot(rf.beadtestlocation)


#Plot tree

library(rpart.plot)
rpart.plot(fitlocationtrain, uniform=TRUE, main="Decision Tree", box.palette = list("Reds"))
rpart.plot(fitlocation, uniform=TRUE, main="Decision Tree", box.palette = list("Blues"))
#text(fit, use.n=TRUE, all=TRUE, cex=.8)
install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(fit)

install.packages("maptree")
library(maptree)
draw.tree(fitclass1,cex=0.80)
draw.tree(fitorder1,cex=0.80)
draw.tree(fitlocation1,cex=0.80)
draw.tree(fitlocationtrain, cex=0.8)
draw.tree(fitspecies1, cex=0.8)



order$order = factor(order$order)
rf.bead2 = randomForest(order ~ asx+glx+ser+gly+ala+val, data = order, subset = train)
rf.bead2

location$Location = factor(location$Location)
rf.bead3 = randomForest(Location ~ asx+glx+ser+gly+ala+val, data = location, subset = train)
rf.bead3

genus$Genus = factor(genus$Genus)
rf.bead4 = randomForest(Genus ~ asx+glx+ser+gly+ala+val, data = genus, subset = train)
rf.bead4

species$Species = factor(species$Species)
rf.bead5 = randomForest(Species ~ asx+glx+ser+gly+ala+val, data = species, subset = train)
rf.bead5



pred = predict(rf.bead3, order[-train,])
table(pred,order[-train,q])

sum( (as.numeric(pred)-as.numeric(location$Location[-train]))^2 )

rf.bead = randomForest(class ~ asx+glx+ser+gly+ala+val, data = beads, subset = train, mtry = 5)
rf.bead
