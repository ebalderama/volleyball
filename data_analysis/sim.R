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
subpaths <- list(past1,past2)
p <- c(.35,.70)

sim_path(alphabet,100,subpaths,p)



}