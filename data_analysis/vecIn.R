## find the starting position of a sequence "b" in a large sequence "a"

vecIn <- function(a,b){
	which(Reduce('+', lapply(seq_along(y <- lapply(b, '==', a)), function(x){y[[x]][x:(length(a)-length(b)+x)]})) == length(b))
}
