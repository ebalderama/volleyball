## checks if there is a context like this one starting with the opposite number
## Ex: 01011: is there a context 11011?
exists_subtree <- function(context, wu)
{
    result = FALSE
    objective = c(1-wu[1], wu[-1])
    for (i in 1:length(context))
       if (length(context[[i]]$context) > length(objective))
       {
          sub_context = context[[i]]$context[-length(context[[i]]$context)] ## exclude the 0
          sub_context = sub_context[(length(sub_context)-length(objective)+1):(length(sub_context))]
          if (all(sub_context == objective)){
          	result = TRUE
          	break
          }
       }
    
    return(result)
}
