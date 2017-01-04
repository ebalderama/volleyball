## PRUNING



PruneContext <- function(y,x,cmax=NULL,K=0.3){


	#=============================================
	# Define cutoff value for pruning
	#=============================================	
	
	cutoff <- K*log(n)
	
	
	#=============================================
	# Initial values
	#=============================================	
	start.time <- proc.time()
	if(is.null(cmax)){cmax <- MaxContext(y)}
	context <- cmax
	n <- length(y)
	p <- ncol(x)
	look_further = TRUE
	
	
	#=============================================
	# Look for nodes to prune
	#=============================================	

	while(look_further){
		prun_these_nodes = NULL
		look_further = FALSE
		
		for(index in 1:length(context))
		if(length(context[[index]]$context) > 2){
			wu = head(context[[index]]$context, -1)
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
			for(j in 1:length(wu)){x_context_w[,(j*p-p+1):(j*p)] <- x[where_w+j-2,]}
			x_context_wu <- x_context_w[where_w %in% (where_wu+1),]
			x_context_w <- x_context_w[,-(1:p)]


			#=================================
			# Compute probabilities
			#=================================
			
			#________________________________
			# for context wu
			#
			#fit = glm(y[where_wu+length(wu)] ~ x_context_wu, family = "binomial")
			fit <- LogisticMCMC(y[where_wu+length(wu)],x_context_wu,plot=T,iters=2000)
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
			#fit = glm(y[where_w+length(w)] ~ x_context_w, family = "binomial")
			fit <- LogisticMCMC(y[where_w+length(w)],x_context_w,plot=T,iters=5000)
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
			
			
			#=================================
			# Prune
			#=================================

			value <- mean(p_0wu)*log(mean(p_0wu)/mean(p_0w))*n_wu
			if(!is.finite(value)) value <- 0
			
			value2 <- mean(p_1wu)*log(mean(p_1wu)/mean(p_1w))*n_wu
			if(is.finite(value2)) value <- value + value2
			
			if(value < cutoff){
				if(exists_subtree(context,wu)){
					prun_these_nodes <- c(prun_these_nodes,index)
				}else{
					context[[index]]$context <- c(w,0)
				}
				
				look_further = TRUE
			}
			
		}
		
		if(!is.null(prun_these_nodes)) context <- context[-prun_these_nodes]
		
		#=============================================
		# update context probabilities
		#=============================================
		for(index in 1:length(context)){
			a <- length(vecIn(y,context[[index]]$context))
			b <- length(vecIn(y[-n],context[[index]]$context[-length(context[[index]]$context)]))
			context[[index]]$prob = a/b
		}
		cat("length(context)=", length(context),"; loop duration:",(proc.time()-start.time)[3]," sec; date: ", sep="")
		print(Sys.time())
	}
	
	cat("\nTotal time:", round((proc.time()-start.time)[3]/60,2),"minutes.")
	return(context)
	
}
