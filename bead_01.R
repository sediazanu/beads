require(randomForest)
setwd("D:/Users/newto/Documents/York teachings/R 2021-22/Group projects/Group project dataset/Beads")

bead_data <- read.csv(file = "bead_data_1.csv", header = TRUE)

set.seed(123)
dim_bc = dim(bead_data)

N = dim_bc[1]
q = dim_bc[2]


train = sample(1:N, round(N/2))


library(rpart)
library(rpart.plot)
#Grow decision tree
fit <- rpart(class ~ asx+glx+ser+gly+ala+val, method="class", data=bead_data[train,], control =rpart.control(minsplit=10, minbucket=5, cp=0.0001) )

printcp(fit) # display the results
summary(fit) # detailed summary of splits

#Plot tree
plot(fit, uniform=TRUE, main="Decision Tree")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

#For fancier plot you can use the rpart.plot library
#library(rpart.plot)
#rpart.plot(fit)


#Grow random forest
bead_data$class = factor(bead_data$class)
library(randomForest)
rf.bead = randomForest(class ~ asx+glx+ser+gly+ala+val, data = bead_data, subset = train)
rf.bead

pred = predict(rf.bead, bead_data[-train,])
table(pred,bead_data[-train,q])

sum( (as.numeric(pred)-as.numeric(bead_data$class[-train]))^2 )

rf.bead = randomForest(class ~ asx+glx+ser+gly+ala+val, data = bead_data, subset = train, mtry = 5)
rf.bead
  