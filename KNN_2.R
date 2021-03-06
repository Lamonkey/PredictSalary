library(lattice)
library(caret)
library(graphics)
library(stats)
library(philentropy)
library(class)
library(gmodels)
# import data
player_data = read.csv(choose.files())
#shuffle 
player_data.random = player_data[sample(nrow(player_data)),]
#choose features
features <- player_data.random[,6:30]
features[is.na(features)] <- 0
#choose class
class_ <- player_data.random[,33]
norm_ <- matrix(,ncol=0,nrow=nrow(features))
#normalize, minimax 
normalize<- function(x){
  x.min = min(x)
  x.max = max(x)
  return((x-x.min) / (x.max-x.min))
}

for(i in 1:length(features)){
  norm_<-cbind(norm_,normalize(features[,i]))
}

#split into train, development, test by 70%, 15%,15%
train_x <-norm_[1:255,];
development_x <-norm_[256:304,];
test_x <- norm_[305:322,]
train_y <-class_[1:255];
development_y <- class_[256:304];
test_y <- class_[305:322];

#hyper tuning k
results <- matrix(ncol = 2)
for(k in 1:40){
  predict <- knn(train = train_x, test = development_x, cl = train_y, k = k, use.all = FALSE)
  xtab <- table(predict,development_y)
  result <- confusionMatrix(xtab)$overall["Accuracy"]
  results <- rbind(results,c(k,result))
}
results[1,] <- c(0,0)
n <- which.max(results[,2])-1

#use knn on test set
predict <- knn(train = train_x, test = test_x, cl = train_y, k = k, use.all = FALSE)
xtab <- table(predict,test_y)
result <- confusionMatrix(xtab)

