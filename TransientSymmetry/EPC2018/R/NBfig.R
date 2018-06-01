
# Author: tim
###############################################################################
devtools::load_all("/home/tim/workspace/DemoSurf/DemoSurf")
segments_LexRef <- function(LexRefInternalObject, ...){
	V <- LexRefInternalObject$Vertical
	H <- LexRefInternalObject$Horizontal
	D <- LexRefInternalObject$Diagonal
	segments(V$x1,V$y1,V$x2,V$y2,...)
	segments(H$x1,H$y1,H$x2,H$y2,...)
	segments(D$x1,D$y1,D$x2,D$y2,...)
}
LexWrapper <- function(years,ages,N=5,...){
	segments_LexRef(
			LexRefNinternal(Abscissae=years, Ordinate=ages, AbMeasure = "P", OrdMeasure = "A", N = N, isotropic = FALSE),
			...)
}
# make C2 move out. Program it to take steps.
C1 <- 5
C2 <- 15
census_poly <- function(C1,C2,omega=100,...){
	I <- C2 - C1
	polygon(c(C1,C2,C2,C1),c(0,I,omega+I,omega),xpd=TRUE,...)
}
census <- function(C,omega=100,N=5,n=.5,...){
	int <- seq(0,100,by=N)
	segments(C,0,C,omega,...)
	segments(C-n,int,C+n,int,...)
}
proof_plot <- function(C1,C2,omega=100,N=5,n=.5,...){
	census_poly(C1,C2,omega=omega,col="#FF000050",border="red")
	census(C1,omega=omega,N=N,n=n,col="blue")
	census(C2,omega=omega,N=N,n=n,col="blue")
}
par(mai=c(.5,.5,.5,.5))
for (I in seq(0,25,by=.5)){
plot(NULL, type = "n", ylim=c(0,100), xlim=c(0,50),axes = FALSE, xlab = "", ylab = "",asp=1)
LexWrapper(years=0:50,ages=0:100,N=5,col = gray(.8))
proof_plot(C1,C1+I)
Sys.sleep(.2)
}