
# Author: tim
# Trimmed from this repository's R/Functions.R -- keeps only the functions
# this archive's workflow scripts actually call: wsd() (via wmean()),
# groupN(), RR2VV(), draw.fork(). The original file has many more
# functions unrelated to this figure.
###############################################################################

wmean <- function(x,w){
	sum(x*w) / sum(w)
}
wsd   <- function(x,w){
	mn <- wmean(x,w)
	sqrt(sum((mn-x)^2*(w/sum(w))))
}
draw.fork <- function(x1,x2,x3,y1,y2,y3,y4,...){
	segments(x1,y1,x1,y2,...)
	segments(x2,y2,x3,y2,...)
	segments(x2,y2,x2,y3,...)
	segments(x3,y2,x3,y4,...)
}

# group single ages (useful for rescaling)
groupN <- function(x,y,n,fun=sum){
	tapply(x, y - y %% n, fun)
}

# Shift to PC parallelograms
RR2VV <- function(chunk){
	n             <- nrow(chunk)
	chunk1        <- chunk
	chunk1$Births <- chunk1$Births / 2
	chunk2        <- chunk1
	chunk2$Births[-1] <- chunk2$Births[-1] + chunk2$Births[-n]
	chunk2$Lexis  <- "VV"
	chunk2
}
