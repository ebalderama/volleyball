## find the starting position of a sequence "b" in a large sequence "a"

vecIn <- function(a,b){
	which(Reduce('+', lapply(seq_along(z <- lapply(b, '==', a)), function(x){z[[x]][x:(length(a)-length(b)+x)]})) == length(b))
}
