#set working directory to correct folder
setwd("/users/keefeseanc/desktop")
#read csv file into data frame
cfb.df <- read.csv("CFB 2011 2016 - Sheet1.csv", header = TRUE)
#create data frame of only bowl games
bowlGames.df <- subset(cfb.df, Week == 'Bowl')
#create data of regular season(exclude bowl games - include conference champs)
regularSeason.df <- subset(cfb.df, Week != 'Bowl')
#create regular season testing data
ind <- sample(1:nrow(regularSeason.df), size=0.2*nrow(regularSeason.df))
regularSeasonTest.df <- regularSeason.df[ind,]
#check size of test set
dim(regularSeasonTest.df)
#create training data
regularSeasonTrain.df <- regularSeason.df[-ind,]
#check size of train set
dim(regularSeasonTrain.df)
#split the total data into yearly data for further analysis
str(cfb.df)
#build a model
#compare pash, rush, and penalty yards to points for winning team
lmWin <- lm(regularSeasonTrain.df$Winning.Points ~ regularSeasonTrain.df$Winning.Pass.Yards + regularSeasonTrain.df$Winning.Rush.Yards - regularSeasonTrain.df$Winning.Pen.Yards, data = regularSeasonTrain.df)
#examine the model
summary(lmWin)
#more examining
str(summary(lmWin))
#take a look at a graph
plot(lmWin)
#different models
lmWinPointsPass<-lm(regularSeasonTrain.df$Winning.Points ~ regularSeasonTrain.df$Winning.Pass.Yards, data = regularSeasonTrain.df)
lmWinPointsRush <- lm(regularSeasonTrain.df$Winning.Points ~ regularSeasonTrain.df$Winning.Rush.Yards, data = regularSeasonTrain.df)
#check out the model for rushing yards
str(lmWinPointsRush)
summary(lmWinPointsRush)
#model examination for passing yards
summary(lmWinPointsPass)
#plot it
plot(lmWinPointsRush)
plot(lmWinPointsPass)
#R values from regression weren't very convincing
#lets do some clustering
library(ggplot2)
library(cluster)
points.df<- subset(cfb.df[,c(9,30)])
#Lets look at point in a heatmap
ggplot(points.df, aes(points.df$Winning.Points, points.df$Losing.Points)) + geom_bin2d()
#Looks like a real hot spot around 30-35 points for each team
#also looks like there are a couple of anomalys 
cluster<- kmeans(points.df, centers =4)
clusplot(points.df, cluster$cluster, color = TRUE, shade = TRUE)
#slightly more interesting
#lets increase the clusters
cluster <- kmeans(points.df, centers=10)
clusplot(points.df, cluster$cluster, color = TRUE, shade = TRUE)
#not super helpful, the first one was better
#lets do some more clusterings
#what does vegas think the score will be?
#what is the actual score?
#should we play the over/under or spread?
scoring.df<- subset(cfb.df[,c(9,12,30,33,50)])
str(scoring.df)
scoring.df<-na.omit(scoring.df)
ggplot(scoring.df, aes(scoring.df$O.U.Total, scoring.df$Winning.Points+scoring.df$Losing.Points)) + geom_point()
oucluster <- kmeans(scoring.df, centers = 3)
clusplot(scoring.df, oucluster$cluster, color = TRUE)
#looks like the total points form three nice clusters
#lets look at the spreads
ggplot(scoring.df, aes(scoring.df$Winning.Points, scoring.df$Winning.Spread)) + geom_point()
ggplot(scoring.df, aes(scoring.df$Losing.Points, scoring.df$Losing.Spread)) + geom_point()
ggplot(scoring.df, aes(scoring.df$Winning.Spread, scoring.df$Losing.Spread)) + geom_point()
#This might be a good regression candidate
#Lets take another look at points scored and the prediction
ggplot(scoring.df, aes(scoring.df$O.U.Total, scoring.df$Winning.Points+scoring.df$Losing.Points)) + geom_point()
mod<-lm(scoring.df$Winning.Points+scoring.df$Losing.Points ~ scoring.df$O.U.Total, data = scoring.df)
summary(mod)
#Doesn't look like vegas is very good. Can we take advantage? 
plot(scoring.df$O.U.Total, scoring.df$Winning.Points+scoring.df$Losing.Points, main = "Actual score vs Vegas Total", xlab = "Vegas total", ylab = "Actual Total" )
abline(mod)
scoring.df$Actual.Total <- scoring.df$Winning.Points+scoring.df$Losing.Points
hist(scoring.df$O.U.Total)
hist(scoring.df$Actual.Total)
#doesnt look promising