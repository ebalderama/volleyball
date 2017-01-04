# simulate context


sim_path <- function(alphabet,n,subpaths,p){
	path <- as.numeric(sample(alphabet,n,replace=TRUE))
	start <- min(unlist(lapply(subpaths,length)))+1
	for(i in start:n){
		if(!is.na(s <- Position(function(x) identical(x,tail(path[1:(i-1)],length(x))), subpaths))){
			path[i] <- sample(alphabet,1,prob=c(1-p[s],p[s]))
		}
	}
	return(path)
}


if(F){
#EXAMPLE


alphabet <- 0:1
past1 <- c(0,1,0,1) #P(1 | 0101)
past2 <- c(0,0,0) #P(1 | 000)
past3 <- c(1,0,0,1,1,0) #P(1 | 10110)
subpaths <- list(past1,past2,past3)
p <- c(.35,.70,.60)

n <- 200
x <- matrix(1,n,1)
y <- sim_path(alphabet,n,subpaths,p)


source("expit.R")
source("vecIn.R")
source("MaxContext2.R")
source("exists_subtree2.R")
source("LogisticMCMC.R")
source("PruneContext2.R")

context <- PruneContext(y,x,K=.15,bayesian=T,iters=100) 



}