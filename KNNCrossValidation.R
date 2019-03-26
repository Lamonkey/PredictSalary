library(lattice)
library(caret)
library(graphics)
library(stats)
library(philentropy)
library(class)
library(gmodels)
library(caret)
methodKNN <-function(norm,Salary,n,f){
  
  #used all 6:31 features
  trains = norm[1:322,]
 # tests = (norm[276:322,])
  trains.label <- Salary[1:322]
  plot(x = 1:length(trains.label),y=trains.label,xlab = "Player",ylab = "Salary")
  tests.label <- Salary[276:322]
  plot(x = 1:length(tests.label),y=tests.label,xlab = "Player",ylab = "Salary",col="green")
  
  cl = Salary[1:322]
  prc_test_pred <- knn.cv(train = trains, cl = trains.label,  k = n, p = f )
  result <- as.numeric(as.character(prc_test_pred))
  plot(x = 1:length(result),y=result,xlab = "Player",ylab = "Salary",col="red")
  
  
  #combine the label
  m <- cbind(result,trains.label)[276:322,]
  #for comparision took only 276:322
  #plot 
  m.sort <- m[order(m[,2],decreasing=TRUE),]# if is cosine, rank from high to low according to distance
  plot(x = 1: nrow(m),y=m.sort[,2],col="red")
  points(x = 1:nrow(m), y = m.sort[,1],col="green")
  legend(1, 95, legend=c("Line 1", "Line 2"),
         col=c("red", "blue"), lty=1:2, cex=0.8)
 # plot(x=1:length(Salary),y=Salary)
  
  # error function to evaluate the performance
  error = sum((m[,1]-m[,2])^2)
  return(error)
}

player_data = read.csv('/home/michael/git/PredictSalary/player_stats.csv')
#try to use all features
Salary = (player_data$Salary)
playerFeatures <- player_data[,6:30]
playerFeatures[is.na(playerFeatures)] <- 0

norm <- matrix(,ncol=0,nrow=nrow(playerFeatures))
#norm using z-score
for(i in 1:length(playerFeatures)){
  norm<-cbind(norm,scale(playerFeatures[,i],center=TRUE,scale=TRUE))
}


comps<-matrix(nrow=50,ncol=10)
for(i in 1:50){
  for(k in 1:10){
  error <- methodKNN(norm,Salary,i,k)
  comps[i,k] <- error
  }
  
  
  
}
plot(x=1:ncol(comps),y=comps,type="b")
min(comps)
which(comps == min(comps), arr.ind = TRUE)
methodKNN(norm,Salary,1,10)

