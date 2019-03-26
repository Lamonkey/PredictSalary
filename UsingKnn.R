library(lattice)
library(caret)
library(graphics)
library(stats)
library(philentropy)
library(class)
library(gmodels)
#read date
#player_data = read.csv(file.choose(new = FALSE))
player_data = read.csv('/home/michael/git/PredictSalary/player_stats.csv')

#normalize using minimax
#PTS
PTS = normalize(player_data$PTS)
#AST
AST = normalize(player_data$AST)
#TRB
TRB = normalize(player_data$TRB)
#Salary
Salary = (player_data$Salary)

num.test = 65
num.dev = 257
#test
devs = cbind(PTS[1:275],AST[1:275],TRB[1:275])

tests = (cbind(PTS[276:322],AST[276:322],TRB[276:322]))

#test = (cbind(PTS[277],AST[277],REB[277]))
#test.Salary = Salary[277]

normalize<- function(x){
  x.min = min(x)
  x.max = max(x)
  return((x-x.min) / (x.max-x.min))
}

cosine<-function(x){
  return (distance(x, method = "cosine"))
}

knn <- function(dev,test,salary = Salary){
dev.Salary = salary[1:275]
test.Salary = salary[276:322]
m <- rbind(dev,test)
#mini max 

#norm using z-score

# method to calculate the distance

# get  neighbours 
dis.cosine <- cosine(m)[,276][1:275]

# append salary to neibor
neighbour <- t(rbind(dis.cosine,dev.Salary))
length(dis.cosine)
length(dev.Salary)
#sort accroding to distance 
m.sort <- neighbour[order(neighbour[,1],decreasing=TRUE),] # if is cosine, rank from high to low according to distance
return (pred.salary = m.sort[1,2])
}

#sample
result <- matrix(data = NA, nrow = 0,ncol = 2)
for(i in 1:nrow(tests)){
  pred.salary <- knn(devs,tests[i,])
  result <- rbind(result,c(pred.salary,tests.Salary[i]))
}
#try to use all features
playerFeatures <- player_data[,6:30]
playerFeatures[is.na(playerFeatures)] <- 0


#norm using min max
norm <- matrix(,ncol=0,nrow=nrow(playerFeatures))

for(i in 1:length(playerFeatures)){
  norm<-cbind(norm,normalize(playerFeatures[,i]))
}

# try to use the knn class library

#used only three
#trains = cbind(PTS[1:275],AST[1:275],TRB[1:275])
#tests = (cbind(PTS[276:322],AST[276:322],TRB[276:322]))

methodKNN <-function(norm,Salary){
#used all 6:31 features
trains = norm[1:275,]
tests = (norm[276:322,])

plot(x = 1:length(trains.label),y=trains.label,xlab = "Player",ylab = "Salary")
trains.label <- Salary[1:275]

plot(x = 1:length(tests.label),y=tests.label,xlab = "Player",ylab = "Salary",col="green")
tests.label <- Salary[276:322]
cl = Salary[276:322]
prc_test_pred <- knn(train = trains, test = tests, cl = trains.label, k = 1)
result <- as.numeric(as.character(prc_test_pred))
plot(x = 1:length(result),y=result,xlab = "Player",ylab = "Salary",col="red")


#combine the label
m <- cbind(result,tests.label)
#plot 
m.sort <- m[order(m[,2],decreasing=TRUE),]# if is cosine, rank from high to low according to distance
plot(x = 1: nrow(m),y=m.sort[,2],col="red")
points(x = 1:nrow(m), y = m.sort[,1],col="green")
legend(1, 95, legend=c("Line 1", "Line 2"),
       col=c("red", "blue"), lty=1:2, cex=0.8)
plot(x=1:length(Salary),y=Salary)

# error function to evaluate the performance
error = sum((m[,1]-m[,2])^2)
return(error)
}
plot(x=1:ncol(comp),y=comp,type="b")
