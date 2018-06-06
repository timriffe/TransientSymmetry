
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
C1 <- 25
C2 <- 15
census_poly <- function(C1,C2,omega=100,...){
	I <- C2 - C1
	polygon(c(C1,C2,C2,C1),c(0,I,omega,omega),xpd=TRUE,label,pos=2,...)
}
census <- function(C,omega=100,N=5,n=.5,label=expression(paste("Census ",(t))),pos){
	int <- seq(0,100,by=N)
	segments(C,0,C,omega,col="blue",lwd=2)
	segments(C-n,int,C+n,int,col="blue")
	if (!missing(label)){
		text(C,omega+N,label,xpd=TRUE,pos=pos,cex=1.5)
	}
}
proof_plot <- function(C1,C2,omega=100,N=5,n=.5){
	census_poly(C1,C2,omega=omega,col="#FF000050",border="red")
	census(C1,omega=omega,N=N,n=n,label =  expression(paste("C",(t))),pos=2)
	census(C2,omega=omega,N=N,n=n,label = expression(paste("C",(t+delta))),pos=4)
}
par(mai=c(.5,.5,.5,.5))
for (I in seq(0,25,by=.5)){
plot(NULL, type = "n", ylim=c(0,100), xlim=c(0,75),axes = FALSE, xlab = "", ylab = "",asp=1)
LexWrapper(years=0:75,ages=0:100,N=5,col = gray(.8))
proof_plot(C1,C1+I)
Sys.sleep(.2)
}

ages <- seq(0,100)
draw_axes <- function(ages=0:100,years=0:75,aat=0,yat=0,N=10,tri=2,tri.extend=5,omega=max(ages)){
	aN <- ages[ages%%N == 0]
	yN <- years[years%%N == 0]
	segments(aat,min(ages),aat,max(ages),lwd=2,xpd=TRUE)
	segments(aat,yat,max(years),yat,lwd=2,xpd=TRUE)
	# triangles
	polygon(c(max(years),max(years)+tri,max(years)),c(yat-tri/2,yat,yat+tri/2),col="black",xpd=TRUE)
	polygon(c(aat-tri/2,aat,aat+tri/2),c(max(ages),max(ages)+tri,max(ages))+tri.extend,col="black",xpd=TRUE)
	# pointers w split axis //
	segments(aat,max(ages) + tri,aat,max(ages)+tri.extend,lwd=2,xpd=TRUE)
	segments(aat-tri/2, max(ages) + tri/2, aat + tri/2,max(ages)+ tri + tri/2, lwd=2,xpd=TRUE)
	segments(aat-tri/2, max(ages) - tri/2, aat + tri/2,max(ages)+ tri/2, lwd=2,xpd=TRUE)
	
	# labels
	text(mean(years),yat-5,"Calendar time",cex=2,xpd=TRUE)
	text(aat-5,mean(ages),expression(paste("Time in ",italic('s'))),cex=2,xpd=TRUE,srt=90)
	
	# y labels
	text(aat-4,yat,0,cex=1.5,xpd=TRUE)
	text(aat-4,max(ages),expression(omega),cex=1.5,xpd=TRUE)
}

