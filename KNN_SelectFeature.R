#This one is to try selected features
library(lattice)
library(caret)
library(graphics)
library(stats)
library(philentropy)
library(class)
library(gmodels)

#shuffle 
player.data.random = player.data[sample(nrow(player.data)),]


#choose the dependent
dependendVariable <- player.data.random$SalaryClassExpanded



#split into train, development, test by 70%, 15%,15%
#leave out position
#TODO: convert Position to integer data
player.data.random$Pos <- unclass(player.data.random$Pos)
train_x <-player.data.random[1:255,selectedFeatures];
development_x <-player.data.random[256:304,selectedFeatures];
test_x <- player.data.random[305:322,selectedFeatures]
train_y <-dependendVariable[1:255];
development_y <- dependendVariable[256:304];
test_y <- dependendVariable[305:322];

#hyper tuning k
results<- matrix(ncol=2)
for(k in 1:40){
  predict <- knn(train = train_x, test = development_x, cl = train_y, k = k, use.all = FALSE)
  xtab <- table(predict,development_y)
  sum = 0
  for (i in 1:ncol(xtab)){
    tmp = 0
    for (m in 1: nrow(xtab)){
      tmp =  tmp + xtab[m,i]*(i-m)^2
    }
    sum = sum + tmp
  }
  mse <- sum / length(development_y)
  
  results <- rbind(results,c(k,mse))
  
}
plot(results,main = "MSE of K neighbor",xlab = "number of neighbor", ylab="MSE")
n <- which.min(results[,2]) -1 


#use knn on test set
predict <- knn(train = train_x, test = test_x, cl = train_y, k = n, use.all = FALSE)
xtab <- table(predict,test_y)
result <- confusionMatrix(xtab)
for (i in 1:ncol(xtab)){
  tmp = 0
  for (m in 1: nrow(xtab)){
    tmp =  tmp + xtab[m,i]*(i-m)^2
  }
  sum = sum + tmp
}
mse <- sum / length(development_y)
mse
  

