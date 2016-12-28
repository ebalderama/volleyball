#Bayesian fitting of context probabilities

#requires expit function


llike <- function(y,XB){
	sum(dbinom(y,1,expit(XB),log=TRUE))
}



LogisticMCMC <- function(y,X,
                         iters=300,thin=1,burn=NULL,
                         prior.mn=0,prior.sd=1,
                         can.sd=2,
                         plot=FALSE
                         ){
	
    # MCMC code for the model:
    # y[i]        ~ Bern(p[i])
    # logit(p[i]) = X%*%beta
    # beta[j]     ~ N(prior.mn,prior.sd)
    
    
    #==============================================
    # Initial values
    #==============================================
    X <- cbind(1,X)
    p <- ncol(X)
    beta <- rnorm(p,0,1)

    XB  <- X%*%beta
    XB  <- ifelse(XB>10,10,XB)
    curll <- dbinom(y,1,expit(XB),log=TRUE)
    
    if(is.null(burn)) burn <- round(iters/2)
    keep.beta <- matrix(0,iters,p)
    acc <- rep(0,p)
    if(plot) rows <- ceiling(sqrt(min(p,9)))
        
        
    #==============================================
    # Start MCMC
    #==============================================
    for(i in 1:iters){for(nthin in 1:thin){
    	
    	#_______________________________________
    	#Update beta (Metropolis)
    	#
    	for(j in 1:p){
    		canbeta <- rnorm(1,beta[j],can.sd)
    		canXB <- XB + X[,j]*(canbeta-beta[j])
    		canll <- dbinom(y,1,expit(canXB),log=TRUE)
    		
    		lr <- sum(canll-curll) + dnorm(canbeta,prior.mn,prior.sd,log=TRUE) - dnorm(beta[j],prior.mn,prior.sd,log=TRUE)
    		
    		if(log(runif(1)) < lr){
    			beta[j] <- canbeta
    			XB <- canXB
    			curll <- canll
    			acc[j] <- acc[j] + 1
    		}
    	}
    	
    	}# End thinning

    	
    	#_______________________________________
    	# Keep the results thus far:
    	#
    	keep.beta[i,] <- beta
    	
    	#_______________________________________
    	# Plot the results thus far:
    	#
    	if(plot)
    	if(i%%500==0){
    		par(mfrow=c(rows,rows))
    		for(j in 1:(min(p,9))){plot(keep.beta[1:i,j],type="l")}
    	}

    }# End MCMC


    #==============================================
    # Output Results
    #==============================================
    list(beta      = keep.beta,
         coef      = colMeans(keep.beta[(burn+1):iters,]),
         acc.rate  = acc/iters
         )

}
