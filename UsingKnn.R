library(lattice)
library(caret)
library(graphics)
library(stats)
library(philentropy)
library(class)
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

knn(dev,test[3,])


