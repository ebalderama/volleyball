

##########################################################################################################################
##########################################################################################################################
## Functions
##########################################################################################################################
##########################################################################################################################

##########################################################################################################################
## vecIn
##########################################################################################################################
## find the starting position of a sequence "b" in a large sequence "a"
vecIn <- function(a,b){
	which(Reduce('+', lapply(seq_along(y <- lapply(b, '==', a)), function(x){y[[x]][x:(length(a)-length(b)+x)]})) == length(b))
}


##########################################################################################################################
## MaxContent
##########################################################################################################################
## 1. Fit a maximal |X|-ary context tree: search for the context function c_max() with the biggest tree such that every element is observed at least twice in the data.
#alphabet = c(0,1)
## Return example: 0100, where 010 is the context and 0 is the response (only response 0 is returned)
##version2 creates context list and prob vector separately
MaxContext <- function(y,alphabet=c(0,1)){
	
	n <- length(y)
	context = list()
	prob = vector()
	
	a <- length(vecIn(y,c(0,0)))
	b <- length(vecIn(y[-n],0))
	context[[1]] = c(0,0)
	prob[1] = a/b
	
	a <- length(vecIn(y,c(1,0)))
	b <- length(vecIn(y[-n],1))
	context[[2]] = c(1,0)
	prob[2] = a/b
	
	
	
	look_further = TRUE
	while(look_further){
		context2 = list()
		prob2 = vector()
		look_further = FALSE
		ind = 1
		for(index in 1:length(context)){
			## try to extend current with a 0
			possible_context = rbind(c(0,context[[index]]),c(1,context[[index]]))
			replaced = FALSE
			
			a <- length(vecIn(y,possible_context[1,]))
			b <- length(vecIn(y[-n],head(possible_context[1,],-1)))
			## if a == b then only the context with 0 happened
			#if((b > 1) && (a > 1) && (a != b)) # replace this context by the longer one
			if(b > 1){ #replace this context by the longer one
				look_further = TRUE
				context[[index]] <- possible_context[1,]
				prob[index] <- a/b
				replaced = TRUE
			}
			
			a <- length(vecIn(y,possible_context[2,]))
			b <- length(vecIn(y[-n],head(possible_context[2,],-1)))
			if(b > 1){ #replace this context by the longer one
				look_further = TRUE
				if(replaced){
					context2[[ind]] <- possible_context[2,]
					prob2[ind] <- a/b
					ind = ind + 1
				}else{
					context[[index]] <- possible_context[2,]
					prob[index] <- a/b
				}
			}
		}#end for loop
		
		context <- append(context, context2)
		prob <- append(prob, prob2)
		
	}#end while loop
	
	return(list(context=context,prob=prob))
}




##########################################################################################################################
## Logistic MCMC
##########################################################################################################################

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



##########################################################################################################################
## Exists Subtree
##########################################################################################################################
## checks if there is a context like this one starting with the opposite number
## Ex: 01011: is there a context 11011? Basically testing if the there is another final node. Note that the seq is inverted, and 1 is the response from the context 0101
# the goal of this function is to: if false we will delete the node 01011 and create 1011; if true we will just delete 01011
##version2 creates context list and prob vector separately

exists_subtree <- function(context, wu)
{
    result = FALSE
    objective = c(1-wu[1], wu[-1])
    for (i in 1:length(context))
       if(length(context[[i]]) > length(objective))
       {
          sub_context = head(context[[i]],-1) ## exclude the 0
          sub_context = sub_context[(length(sub_context)-length(objective)+1):(length(sub_context))]
          if (all(sub_context == objective)){
          	result = TRUE
          	break
          }
       }
    
    return(result)
}


##########################################################################################################################
## Expit
##########################################################################################################################
expit <- function(x){1/(1+exp(-x))}


##########################################################################################################################
## Prunning
##########################################################################################################################
PruneContext <- function(y,x,context=NULL,K=0.3,
                         bayesian=FALSE,iters=300,burn=NULL,plot=FALSE){


	#=============================================
	# Initial values
	#=============================================	
	start.time <- proc.time()
	if(is.null(context)){context <- MaxContext(y)$context}
	prob <- vector()
	n <- length(y)
	p <- ncol(x)
	look_further = TRUE
	
	#=============================================
	# Define cutoff value for pruning
	#=============================================	
	cutoff <- K*log(n)
	
	
	#=============================================
	# Look for nodes to prune
	#=============================================	

	while(look_further){
		prun_these_nodes = NULL
		look_further = FALSE
		
		for(index in 1:length(context))
		if(length(context[[index]]) > 2)
		{
			wu = head(context[[index]], -1)
			w = wu[-1]
			
			#=================================
			# get covariates 
			# X_t = x1_(t-1) + x2_(t-1) + x1_(t-2) + x2_(t-2) + ...
			#=================================
			where_w = vecIn(y[-c(1,n)],w)+1 ## where in the vector y w ocurred - but only look past the first point
			where_wu = vecIn(y[-n],wu) ## where in the vector y wu ocurred
			n_w = length(where_w) ## how many times w ocurred
			n_wu = length(where_wu) ## how many times wu ocurred
			
			x_context_w = matrix(0,n_w,p*length(wu)) ## all variables needed to predict this prob
			for(j in 1:length(wu))
			{
				x_context_w[,(j*p-p+1):(j*p)] <- x[where_w+j-2,]
			}
			x_context_wu <- x_context_w[where_w %in% (where_wu+1),]
			x_context_w <- x_context_w[,-(1:p)]


			#=================================
			# Compute probabilities
			#=================================
			

  ##################
  ## I will not use this part!!!!!!!!
  
  if (FALSE)
  {
			#________________________________
			# for context wu
			#
			#
			if(bayesian){
				fit <- LogisticMCMC(y[where_wu+length(wu)],x_context_wu,iters=iters,burn=burn,plot=plot)
			}else{
				fit <- glm(y[where_wu+length(wu)] ~ x_context_wu, family = "binomial")
			}
			nas = which(is.na(fit$coef))
			if(length(nas)==0){
				p_1wu <- expit(cbind(1,x_context_wu)%*%fit$coef)
			}else{
				p_1wu <- expit(cbind(1,x_context_wu[,-(nas-1)])%*%fit$coef[-nas])
			}
			
			p_0wu = 1 - p_1wu
			
			
			#________________________________
			# for context w
			#
			#
			if(bayesian){
				fit <- LogisticMCMC(y[where_w+length(w)],x_context_w,iters=iters,burn=burn,plot=plot)
			}else{
				fit = glm(y[where_w+length(w)] ~ x_context_w, family = "binomial")
			}
			nas = which(is.na(fit$coef))
			if(length(nas)==0){
				p_1w <- expit(cbind(1,x_context_w)%*%fit$coef)
			}else{
				p_1w <- expit(cbind(1,x_context_w[,-(nas-1)])%*%fit$coef[-nas])
			}
			
			p_0w = 1 - p_1w
			
			p_0wu = ifelse(p_0wu < 0.00000001, 0, p_0wu)
			p_1wu = ifelse(p_1wu < 0.00000001, 0, p_1wu)
			p_0w = ifelse(p_0w < 0.00000001, 0, p_0w)
			p_1w = ifelse(p_1w < 0.00000001, 0, p_1w)
			
			# update context probabilities
			prob[index] <- mean(p_0wu)
			
			
			#=================================
			# Prune
			#=================================
			
			value <- mean(p_0wu)*log(mean(p_0wu)/mean(p_0w))*n_wu
			if(!is.finite(value)) value <- 0
			
			value2 <- mean(p_1wu)*log(mean(p_1wu)/mean(p_1w))*n_wu
			if(is.finite(value2)) value <- value + value2
			
		}
  ################## above not used
			

			fitwu <- glm(y[where_wu+length(wu)] ~ x_context_wu, family = "binomial")
            fitw = glm(y[where_w+length(w)] ~ x_context_w, family = "binomial")
            if ((sum(fitw$residuals^2) - sum(fitwu$residuals^2))/(sum(fitwu$residuals^2)/length(fitwu$residuals)) < qchisq(.95, 1))
            #if (abs(fitw$deviance - fitwu$deviance) < 50)
            #if (anova(fitw, fitwu, test = "LRT")$"Pr(>Chi)"[2] > 0.05)
			#if(value < cutoff)
			{
				if(exists_subtree(context,wu))
				{
					prun_these_nodes <- c(prun_these_nodes,index)
				}else
				{
					context[[index]] <- c(w,0)
					#prob[index] <- mean(p_0w)
				}
				
				look_further = TRUE
			}


		}##end for loop
		
		if(!is.null(prun_these_nodes)){
			context <- context[-prun_these_nodes]
			prob <- prob[-prun_these_nodes]
		}
		
		cat("length(context)=", length(context),"; loop duration: ",(proc.time()-start.time)[3]," sec; date: ", sep="")
		print(Sys.time())
		
	}##end while loop
	
	cat("\nTotal time:", round((proc.time()-start.time)[3]/60,2),"minutes.\n")
	
	return(list(context=context))
	
}



##########################################################################################################################
## Simulated data
##########################################################################################################################


alphabet <- 0:1
past1 <- c(0,0,1,0) #P(1 | 0100) - careful with the order: from past to present 0010u
past2 <- c(1,0,1,0) #P(1 | 0101)
past3 <- c(0,1) #P(1 | 10)
subpaths <- list(past1,past2,past3)

### these paths give the context tree
#                x
#            0       1
#              1    0
#             0
#            0 1 
##
## Note that we have several final nodes: 0 is a final node if we have 00; 01 is a final node if we have 011;...

# NOTE that the length of each vector is nodes+1, where the first element is the parameter Beta_0

## for final node 0[0] - the beta is actually only beta_0 + beta1 corresponding to the 0 context. the Brackets [0] means that we dont have to look there, but we do have to look at 10. The length of each beta is 1(beta_0) + betas of the context 
beta0 = c(1,2)
## for final node 01[1]
beta01 = c(2.5,3,1)
## for final node 0100
beta0100 = c(-1,-2,-1,1,1.5)
## for final node 0101
beta0101 = c(1,3,-2,.5,2.5)
## for final node 1[1]
beta10 = c(-2.5,2)
## for final node 10
beta1 = c(2,-3)


## path = vlmc


n = 1000

#y = as.numeric(sample(alphabet,n,replace=TRUE))


# Z is the covariate
Z = rnorm(n) 
#start <- min(unlist(lapply(subpaths,length)))+1
start <- max(unlist(lapply(subpaths,length)))+1
y = as.numeric(sample(alphabet,start,replace=TRUE))

	for(i in start:n)
	{
		#if (!is.na(s <- Position(function(x) identical(x,tail(y[1:(i-1)],length(x))), subpaths)))
		#{
		#	prob = expit(as.numeric(betas[[s]]%*%c(1,Z[(i-length(betas[[s]])+1):(i-1)]))) ## c(1,Z) is to account for beta_0
		#	y[i] = sample(alphabet,1,prob=c(1-prob,prob))
		#	#cat("\n i = ", i, " prob = ", prob)
		#}
		if (identical(y[(i-4):(i-1)], c(0,0,1,0)))
   			prob = expit(as.numeric(beta0100%*%c(1,Z[(i-length(beta0100)+1):(i-1)]))) ## c(1,Z) is to account for beta_0
		else if (identical(y[(i-4):(i-1)], c(1,0,1,0)))
   			prob = expit(as.numeric(beta0101%*%c(1,Z[(i-length(beta0101)+1):(i-1)]))) ## c(1,Z) is to account for beta_0
		else if (identical(y[(i-3):(i-1)], c(1,1,0)))
   			prob = expit(as.numeric(beta01%*%c(1,Z[(i-length(beta01)+1):(i-1)]))) ## c(1,Z) is to account for beta_0
		else if (identical(y[(i-2):(i-1)], c(0,0)))
   			prob = expit(as.numeric(beta0%*%c(1,Z[(i-length(beta0)+1):(i-1)]))) ## c(1,Z) is to account for beta_0
		else if (identical(y[(i-2):(i-1)], c(0,1)))
   			prob = expit(as.numeric(beta10%*%c(1,Z[(i-length(beta10)+1):(i-1)]))) ## c(1,Z) is to account for beta_0
		else if (identical(y[(i-2):(i-1)], c(1,1)))
   			prob = expit(as.numeric(beta1%*%c(1,Z[(i-length(beta1)+1):(i-1)]))) ## c(1,Z) is to account for beta_0
		else prob = 0.5

		y[i] = sample(alphabet,1,prob=c(1-prob,prob))
	}
   

PruneContext(y,as.matrix(Z))


