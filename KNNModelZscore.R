library(lattice)
library(caret)
library(graphics)
library(stats)
library(philentropy)
library(class)
library(gmodels)
methodKNN <-function(norm,Salary,n,tune){
  #used all 6:31 features
  trains = norm[1:225,]
  tests = (norm[226:273,])
  valid = norm[274:322,]
   trains.label <- Salary[1:225]
  plot(x = 1:length(trains.label),y=trains.label,xlab = "Player",ylab = "Salary")
 tests.label <- as.factor(Salary[226:273])
  plot(x = 1:length(tests.label),y=tests.label,xlab = "Player",ylab = "Salary",col="green")
  valid.label <- as.factor(Salary[274:322])
  if(tune){
  prc_test_pred <- knn(train = trains, test = valid, cl = trains.label, k = n, use.all = FALSE)
  }
  else{prc_test_pred <- knn(train = trains, test = tests, cl = trains.label, k = n, use.all = FALSE)
  }
   result <- as.numeric(as.character(prc_test_pred))
  #plot(x = 1:length(result),y=result,xlab = "Player",ylab = "Salary",col="red")
  
  
  #combine the label
  if(tune){
  xtab <- table(prc_test_pred,valid.label)
  results <- confusionMatrix(xtab)
  m <- cbind(result,valid.label)
  }
  else{
    xtab <- table(prc_test_pred,tests.label)
    results <- confusionMatrix(xtab)
    m <- cbind(result,tests.label)
  }
  #plot 
  m.sort <- m[order(m[,2],decreasing=TRUE),]# if is cosine, rank from high to low according to distance
  plot(x = 1: nrow(m),y=m.sort[,2],col="red", xlab = "Salary in Milions", ylab = "Players")
  points(x = 1:nrow(m), y = m.sort[,1],col="green",pch = 4)
  legend(1, 95, legend=c("Line 1", "Line 2"),
         col=c("red", "blue"), lty=1:2, cex=0.8)
  return(results)
 
}
normalize<- function(x){
  x.min = min(x)
  x.max = max(x)
  return((x-x.min) / (x.max-x.min))
}
#player.data = read.csv('/home/michael/git/PredictSalary/player_stats.csv')
#player_data <- player.data[sample(nrow(player.data)),]

#try to use all features
#Salary = (player_data$Salary)
Salary = player_data$SalaryClass
playerFeatures <- player_data[,6:30]
playerFeatures[is.na(playerFeatures)] <- 0

norm <- matrix(,ncol=0,nrow=nrow(playerFeatures))
#norm 
for(i in 1:length(playerFeatures)){
  #z-score
  norm<-cbind(norm,scale(playerFeatures[,i],center=TRUE,scale=TRUE))
  #minimax
  #norm<-cbind(norm,normalize(playerFeatures[,i]))
}


comp<-matrix(nrow=1,ncol=0)
for(i in 1:80){
    result <- methodKNN(norm,Salary,i,TRUE)
    comp<-cbind(comp,result$overall['Accuracy'])
}
plot(x=1:ncol(comp),y=comp,type="b")
max(comp)
n <- which.max(comp)
n 
m <- methodKNN(norm,Salary,10,FALSE)
m$overall['Accuracy']

