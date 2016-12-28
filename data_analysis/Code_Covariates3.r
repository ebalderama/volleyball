library(gtools)
library(VLMC)

data = read.csv("output.csv", header = TRUE)

point_at = which(data$Play_In_Rally == 1)

p = 10 + 1 ## number of covariates we use + 1st column for the point 0/1
n = length(point_at)



Markov_Data = matrix(0, n, p)

Markov_Data[,2:p] = cbind(data$teamid[point_at], data$skty[point_at], data$skgr[point_at], data$strzn[point_at], data$hrot[point_at], data$arot[point_at], data$hpts[point_at], data$apts[point_at], data$hLibero[point_at], data$aLibero[point_at])


## I am not sure what to do with the initial score of each set. We know we are serving or not (0=not, 1=serve) but this is not the same as (1=win, 0=lose) in the previous play, not the same meaning
for (i in 1:n)
#   if ((data$hpts[point_at[i]] == 0) && (data$apts[point_at[i]] == 0)) ## it is starting the set
#      Markov_Data[i,1] = ifelse(data$team[point_at[i]] == 1, 1, 0)
#   else
     Markov_Data[i,1] = ifelse(((data$team[point_at[i]] == 1) && (data$wonlost[point_at[i]] == 1)) || ((data$team[point_at[i]] == 2) && (data$wonlost[point_at[i]] == 0)), 1, 0)


n = 500

y = Markov_Data[1:n,1]
x = Markov_Data[1:n,c(5,10)]
p = length(x[1,])




##Required functions

source("expit.R")
source("vecIn.R")
source("MaxContext.R")
source("exists_subtree.R")
source("LogisticMCMC.R")
source("PruneContext.R")


## Get Max X-ary Context tree
cmax <- MaxContext(y)
length(cmax)
head(cmax)

## Pruning
context <- PruneContext(y,x,cmax,K=.06) 
context



## compare with vlmc
draw(vlmc(y))






















