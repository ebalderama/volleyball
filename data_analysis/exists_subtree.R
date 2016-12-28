## checks if there is a context like this one starting with the opposite number
## Ex: 01011: is there a context 11011?
exists_subtree <- function(context, wu)
{
    result = FALSE
    objective = c(1-wu[1], wu[-1])
    for (i in 1:length(context))
       if(length(context[[i]]$context) > length(objective))
       {
          sub_context = head(context[[i]]$context,-1) ## exclude the 0
          sub_context = sub_context[(length(sub_context)-length(objective)+1):(length(sub_context))]
          if (all(sub_context == objective)){
          	result = TRUE
          	break
          }
       }
    
    return(result)
}


##EXAMPLES
if(FALSE){

#> cmax[1]
#[[1]]
#[[1]]$context
# [1] 0 0 1 0 0 1 0 0 0 0 0
#
#[[1]]$prob
#[1] 0


#Only subtrees beginning with the most recent (at tail of context)

exists_subtree(cmax[1],c(1,0,1,0,0,1,0,0,0,0)) #TRUE
exists_subtree(cmax[1],c(1,1,0,0,1,0,0,0,0)) #TRUE
exists_subtree(cmax[1],c(0,0,0,1,0,0,0,0)) #TRUE
exists_subtree(cmax[1],c(1,0,1,0,0,0,0)) #TRUE
exists_subtree(cmax[1],c(1,1,0,0,0,0)) #TRUE
exists_subtree(cmax[1],c(0,0,0,0,0)) #TRUE
exists_subtree(cmax[1],c(1,0,0,0)) #TRUE
exists_subtree(cmax[1],c(1,0,0)) #TRUE
exists_subtree(cmax[1],c(1,0)) #TRUE
exists_subtree(cmax[1],c(1)) #TRUE

#appearing in the middle of the context is NOT a subtree
exists_subtree(cmax[1],c(0)) #FALSE
exists_subtree(cmax[1],c(0,0,0,0)) #FALSE
exists_subtree(cmax[1],c(1,0,1,0,0,1)) #FALSE
exists_subtree(cmax[1],c(0,0,0,1)) #FALSE


}


