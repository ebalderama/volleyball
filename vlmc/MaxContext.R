## 1. Fit a maximal |X|-ary context tree: search for the context function c_max() with the biggest tree such that every element is observed at least twice in the data.

#alphabet = c(0,1)



MaxContext <- function(y,alphabet=c(0,1)){
	
	n <- length(y)
	context = list()
	
	a <- length(vecIn(y,c(0,0)))
	b <- length(vecIn(y[-n],0))
	context[[1]] = list(context = c(0,0), prob = a/b)
	
	a <- length(vecIn(y,c(1,0)))
	b <- length(vecIn(y[-n],1))
	context[[2]] = list(context = c(1,0), prob = a/b)
	
	
	
	look_further = TRUE
	while(look_further){
		context2 = list()
		look_further = FALSE
		ind = 1
		for(index in 1:length(context)){
			## try to extend current with a 0
			possible_context = rbind(c(0,context[[index]]$context),c(1,context[[index]]$context))
			replaced = FALSE
			
			a <- length(vecIn(y,possible_context[1,]))
			b <- length(vecIn(y[-n],head(possible_context[1,],-1)))
			## if a == b then only the context with 0 happened
			#if((b > 1) && (a > 1) && (a != b)) # replace this context by the longer one
			if(b > 1){ #replace this context by the longer one
				look_further = TRUE
				context[[index]] <- list(context=possible_context[1,],prob=a/b)
				replaced = TRUE
			}
			
			a <- length(vecIn(y,possible_context[2,]))
			b <- length(vecIn(y[-n],head(possible_context[2,],-1)))
			if(b > 1){ #replace this context by the longer one
				look_further = TRUE
				if(replaced){
					context2[[ind]] <- list(context=possible_context[2,],prob=a/b)
					ind = ind + 1
				}
				else context[[index]] <- list(context=possible_context[2,],prob=a/b)
			}
		}#end for loop
		
		context = append(context, context2)
		
	}#end while loop
	
	return(context)
}

