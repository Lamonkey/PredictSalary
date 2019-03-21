library(ggplot2)

# import the data
player_data.raw = read.csv(file.choose(new = FALSE))

#using PCA to reduce feature
autoplot(prcomp(player_data.raw))
#with age
mtplayer.pca <- prcomp(player_data.raw[,c(4,8:10,22:31)],center = TRUE,scale. = TRUE)
#without age
mtplayer.pca <- prcomp(player_data.raw[,c(8:10,22:31)],center = TRUE,scale. = TRUE)
summary(mtplayer.pca)
#get ride of salary
mtplayer.pca <- prcomp(player_data.raw[,c(8:10,22:30)],center = TRUE,scale. = TRUE)

#salary accroding to age
plot(player_data.raw$Age,player_data.raw$Salary)
#scatter to min of play
plot(player_data.raw$MP,player_data.raw$Salary)
