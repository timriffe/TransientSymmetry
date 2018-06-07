
# Author: tim
###############################################################################
setwd("/home/tim/git/TransientSymmetry/TransientSymmetry")
source("EPC2018/R/Functions.R")
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
	segments(C,0,C,omega,col="blue",lwd=3)
	segments(C-n,int,C+n,int,col="blue",lwd=2)
	if (!missing(label)){
		text(C,omega+N,label,xpd=TRUE,pos=pos,cex=1.5)
	}
}
proof_plot <- function(C1,C2,omega=100,N=5,n=.5){
	census_poly(C1,C2,omega=omega,col="#FF000050",border="red")
	census(C1,omega=omega,N=N,n=n,label =  expression(paste("C",(t))),pos=2)
	census(C2,omega=omega,N=N,n=n,label = expression(paste("C",(t+delta))),pos=4)
}
draw_axes <- function(ages=0:100,years=0:75,aat=0,yat=0,N=10,tri=2,tri.extend=5,omega=max(ages)){
	aN <- ages[ages%%N == 0]
	yN <- years[years%%N == 0]
	segments(aat,min(ages),aat,max(ages)+tri.extend,lwd=2,xpd=TRUE)
	segments(aat,yat,max(years),yat,lwd=2,xpd=TRUE)
	# triangles
	polygon(c(max(years),max(years)+tri,max(years)),c(yat-tri/2,yat,yat+tri/2),col="black",xpd=TRUE)
	polygon(c(aat-tri/2,aat,aat+tri/2),c(max(ages),max(ages)+tri,max(ages))+tri.extend,col="black",xpd=TRUE)
	# pointers w split axis //
	#segments(aat,max(ages) + tri,aat,max(ages)+tri.extend,lwd=2,xpd=TRUE)
	#segments(aat-tri/2, max(ages) + tri/2, aat + tri/2,max(ages)+ tri + tri/2, lwd=2,xpd=TRUE)
	#segments(aat-tri/2, max(ages) - tri/2, aat + tri/2,max(ages)+ tri/2, lwd=2,xpd=TRUE)
	
	# labels
	text(mean(years),yat-5,"Calendar time",cex=2,xpd=TRUE)
	text(aat-5,mean(ages),expression(paste("Time in ",italic('s'))),cex=2,xpd=TRUE,srt=90)
	
	# y labels
	text(aat-4,yat,0,cex=1.5,xpd=TRUE)
	text(aat-4,max(ages),expression(omega),cex=1.5,xpd=TRUE)
}

pdf("EPC2018/Figures/Proof0.pdf",width=11,height=7)
par(mai=c(.5,0,.5,1.5))
plot(NULL, type = "n", ylim=c(0,100), xlim=c(0,125),axes = FALSE, xlab = "", ylab = "",asp=1)
draw_axes(years=0:125)
dev.off()

pdf("EPC2018/Figures/Proof1.pdf",width=11,height=7)
par(mai=c(.5,0,.5,1.5))
plot(NULL, type = "n", ylim=c(0,100), xlim=c(0,125),axes = FALSE, xlab = "", ylab = "",asp=1)
LexWrapper(years=0:125,ages=0:100,N=15,col = gray(.8))
draw_axes(years=0:125)
dev.off()

pdf("EPC2018/Figures/Proof2.pdf",width=11,height=7)
par(mai=c(.5,0,.5,1.5))
plot(NULL, type = "n", ylim=c(0,100), xlim=c(0,125),axes = FALSE, xlab = "", ylab = "",asp=1)
LexWrapper(years=0:125,ages=0:100,N=10,col = gray(.8))
draw_axes(years=0:125)
dev.off()

pdf("EPC2018/Figures/Proof3.pdf",width=11,height=7)
par(mai=c(.5,0,.5,1.5))
plot(NULL, type = "n", ylim=c(0,100), xlim=c(0,125),axes = FALSE, xlab = "", ylab = "",asp=1)
LexWrapper(years=0:125,ages=0:100,N=5,col = gray(.8))
draw_axes(years=0:125)
dev.off()

pdf("EPC2018/Figures/Proof4.pdf",width=11,height=7)
par(mai=c(.5,0,.5,1.5))
plot(NULL, type = "n", ylim=c(0,100), xlim=c(0,125),axes = FALSE, xlab = "", ylab = "",asp=1)
LexWrapper(years=0:125,ages=0:100,N=5,col = gray(.8))
census(C1,omega=100,N=5,n=.5,label =  expression(paste("C",(t))),pos=NULL)
#census(C1,omega=omega,N=N,n=n,label = expression(paste("C",(t+delta))),pos=4)
draw_axes(years=0:125)
dev.off()

pdf("EPC2018/Figures/Proof5.pdf",width=11,height=7)
par(mai=c(.5,0,.5,1.5))
plot(NULL, type = "n", ylim=c(0,100), xlim=c(0,125),axes = FALSE, xlab = "", ylab = "",asp=1)
LexWrapper(years=0:125,ages=0:100,N=5,col = gray(.8))
census(C1,omega=100,N=5,n=.5,label =  expression(paste("C",(t))),pos=NULL)
census(C1+20,omega=100,N=5,n=.5,label =  expression(paste("C",(t+delta))),pos = NULL)
census(C1+40,omega=100,N=5,n=.5,label =  expression(paste("C",(t+2 *delta))),pos=NULL)
text(C1 + 10, 50, "=",cex=3,font=2)
text(C1 + 30, 50, "=",cex=3,font=2)
#census(C1,omega=omega,N=N,n=n,label = expression(paste("C",(t+delta))),pos=4)
draw_axes(years=0:125)
dev.off()
# increment
Is <- seq(5,100,by=5)
for (i in 1:length(Is)){
	pdf(paste0("EPC2018/Figures/Proof",i+5,".pdf"),width=11,height=7)
	par(mai=c(.5,0,.5,1.5))
plot(NULL, type = "n", ylim=c(0,100), xlim=c(0,125),axes = FALSE, xlab = "", ylab = "",asp=1)
LexWrapper(years=0:125,ages=0:100,N=5,col = gray(.8))
proof_plot(C1,C1+Is[i])
if (Is[i] >= 5){
	curlyBrace1(C1, 103, length = Is[i], radius1 = 2, radius2 = 1, top = TRUE, lwd =2)
}
text(C1+Is[i]/2,107,expression(delta),cex=2,xpd=TRUE,col="red")
text(C1+Is[i],Is[i],expression(delta),cex=2,xpd=TRUE,pos=4,col="blue")
draw_axes(years=0:125)
dev.off()
#Sys.sleep(.2)
}

# -----------------------------------
# symmetry:
ta <- dexp(0:10, rate = .5, log = FALSE)
pdf(paste0("EPC2018/Figures/Symmetry.pdf"))
par(mai=c(.5,.5,.5,.5))
plot(NULL, type = "n", ylim=c(0,.5), xlim=c(-10,10),axes = FALSE, xlab = "", ylab = "")
lines(0:10,ta,lwd=2,col="blue")
lines(0:-10,ta,lwd=2,col="red")
points(0:10,ta,pch=16,col="blue")
points(0:-10,ta,pch=16,col="red")
segments(-11,0,11,0,lwd=2,xpd=TRUE)
segments(0,-.01,0,.5)
text(0,-.01,0,pos=1,cex=2,xpd=TRUE)
text(5,-.01,"Time spent",pos=1,cex=2,xpd=TRUE)
text(-5,-.01,"Time to exit",pos=1,cex=2,xpd=TRUE)
text(-.5,.1,"Density",srt=90,cex=2,xpd=TRUE)
text(-11,-.01,expression(omega),pos=1,cex=2,xpd=TRUE)
text(11,-.01,expression(omega),pos=1,cex=2,xpd=TRUE)
# arrows
polygon(c(11,11,11.3),c(.005,-.005,0),col="black",xpd=TRUE)
polygon(c(-11,-11,-11.3),c(.005,-.005,0),col="black",xpd=TRUE)
dev.off()