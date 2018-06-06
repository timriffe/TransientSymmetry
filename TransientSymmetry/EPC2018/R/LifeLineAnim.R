
# Author: tim
###############################################################################

setwd("/home/tim/workspace/CareyIneq/Figures/LifeLineSeq")

library(tweenr)

#library(HMDHFDplus)
#flt <- readHMDweb("JPN", "fltper_1x1", us, pw)
#
#lx <- flt$lx[flt$Year == max(flt$Year)] / 1e5
lx <- c(1, 0.99804, 0.99771, 0.99752, 0.99742, 0.99731, 0.9972, 0.99712, 
		0.99706, 0.99702, 0.99695, 0.99689, 0.99682, 0.99676, 0.9967, 
		0.99662, 0.99653, 0.99645, 0.99632, 0.99617, 0.99599, 0.9958, 
		0.9956, 0.99538, 0.99512, 0.99488, 0.99463, 0.99433, 0.99403, 
		0.99375, 0.99346, 0.99312, 0.99274, 0.99239, 0.99203, 0.99163, 
		0.9912, 0.99073, 0.99027, 0.98971, 0.98912, 0.98847, 0.98776, 
		0.98697, 0.98618, 0.98533, 0.98434, 0.98325, 0.98206, 0.98086, 
		0.97944, 0.97799, 0.97635, 0.97456, 0.97264, 0.97066, 0.96838, 
		0.96602, 0.96349, 0.96087, 0.958, 0.95502, 0.95168, 0.94814, 
		0.94428, 0.94007, 0.9354, 0.93039, 0.92496, 0.9198, 0.91383, 
		0.90693, 0.89987, 0.89165, 0.88276, 0.87304, 0.86259, 0.85015, 
		0.83666, 0.82112, 0.80379, 0.78431, 0.76214, 0.73772, 0.71019, 
		0.68031, 0.64631, 0.6104, 0.57132, 0.52884, 0.48359, 0.43563, 
		0.387, 0.33789, 0.29033, 0.24375, 0.20115, 0.16201, 0.12705, 
		0.09681, 0.07152, 0.05112, 0.03529, 0.02348, 0.01504, 0.00927, 
		0.00549, 0.00313, 0.00171, 9e-04, 0.00046) * 10
a <- seq(0,11,length=111)

# 1) survival curve

pdf("chron1.pdf",height=5,width=10)
par(mai=c(.5,.5,.5,.5),xaxs="i",yaxs="i")
plot(a, lx, type = 'n', las = 1, ylab = "l(x)", xlab = "Age", axes = FALSE, asp = 1, xlim=c(-10,10))
polygon(c(a,11,0),c(lx,0,0),col=gray(.7), border=NA)
abline(v=1:10,col="white")
abline(h=seq(0,10,by=1),col="white")
axis(1,at=c(0,5,10),labels=c(0,50,100))
dev.off()

# 2) 10 discrete lives: chop
N <- 10
p  <- seq(0,10,length=N+1)
pb <- (p[1:N] + p[2:(N+1)]) / 2
L  <- round(splinefun((a)~lx)(pb))

pdf("chron2.pdf",height=5,width=10)
par(mai=c(.5,.5,.5,.5),xaxs="i",yaxs="i")
plot(a, lx, type = 'n', las = 1, ylab = "l(x)", xlab = "Age", axes = FALSE, asp = 1, xlim=c(-10,10))
#segments(0,pb,Lb,pb,col="orange")
rect(0, p[-(N+1)], L , p[-1], col = gray(.7), border="white")
abline(v=1:10,col="white")
axis(1,at=c(0,5,10))
dev.off()
# recolor by age
library(RColorBrewer)

chronocols <- rev(colorRampPalette(brewer.pal(9,"YlGn"), space = "Lab")(12)[-c(1,2)])
thanocols  <- rev(colorRampPalette(brewer.pal(9,"YlOrRd"), space = "Lab")(12)[-c(1,2)])
M10        <- matrix(0, 10, 10)
xchrono    <- col(M10) 
ychrono    <- row(M10)
chronoM    <- chronocols[xchrono]

dim(chronoM) <- dim(M10)
chronoM[xchrono > L] <- NA

# 3) chronological age coloring
pdf("chron3.pdf",height=5,width=10)
par(mai=c(.5,.5,.5,.5),xaxs="i",yaxs="i")
plot(a, lx, type = 'n', las = 1, ylab = "l(x)", xlab = "Age", axes = FALSE, asp = 1, xlim=c(-10,10))
rect(xchrono-1,ychrono-1,xchrono,ychrono,col=chronoM, border="white")
# border
polygon(c(0,rep(unique(L),each=2),0),
		c(0,0,rep(cumsum(rle(L)$length),each=2)),
		border = gray(.2))
axis(1,at=c(0,5,10))
dev.off()
# 3.1) chronological age coloring, with last year of life highlighted
pdf("chron4.pdf",height=5,width=10)
par(mai=c(.5,.5,.5,.5),xaxs="i",yaxs="i")
plot(a, lx, type = 'n', las = 1, ylab = "l(x)", xlab = "Age", axes = FALSE, asp = 1, xlim=c(-10,10))
rect(xchrono-1,ychrono-1,xchrono,ychrono,col=chronoM, border="white")
rect(L-1,0:9,L,1:10,col=thanocols[1],xpd=TRUE, border="white")
polygon(c(0,rep(unique(L),each=2),0),
		c(0,0,rep(cumsum(rle(L)$length),each=2)),
		border = gray(.2))
axis(1,at=c(0,5,10))
dev.off()

# 4) thanatological coloring in chrono age:
xthano1 <- xchrono
xthano1[xthano1 > L] <- NA
xthano  <- t(apply(xthano1,1, function(x){
			 x[!is.na(x)] <- sum(!is.na(x)):1
			 x
		   }))
thanoM              <- thanocols[xthano]
dim(thanoM)         <- dim(M10)
thanoM[xchrono > L] <- NA

pdf("chron5.pdf",height=5,width=10)
par(mai=c(.5,.5,.5,.5),xaxs="i",yaxs="i")
plot(a, lx, type = 'n', las = 1, ylab = "l(x)", xlab = "Age", axes = FALSE, asp = 1, xlim=c(-10,10))
rect(xchrono-1,ychrono-1,xchrono,ychrono,col=thanoM, border="white")
polygon(c(0,rep(unique(L),each=2),0),
		c(0,0,rep(cumsum(rle(L)$length),each=2)),
		border = gray(.2))
axis(1,at=c(0,5,10))
dev.off()

# 5) now right-align
xthano2 <- 11-xthano
xthano2 <-t(apply(xthano2,1,function(x){
			c(x[is.na(x)],x[!is.na(x)])
		}))
thanoM2              <- rev(thanocols)[xthano2]
dim(thanoM2)         <- dim(M10)

pdf("than1.pdf",height=5,width=10)
par(mai=c(.5,.5,.5,.5),xaxs="i",yaxs="i")
plot(a, lx, type = 'n', las = 1, ylab = "", xlab = "", axes = FALSE, asp = 1, xlim=c(-10,10))
rect(xthano2-1,ychrono-1,xthano2,ychrono,col=thanoM2, border="white")
polygon(10-c(0,rep(unique(L),each=2),0),
		c(0,0,rep(cumsum(rle(L)$length),each=2)),
		border = gray(.2))
axis(1,at=c(0,5,10),labels=c(10,5,0))
dev.off()

# 6) now side by side:
pdf("together.pdf",height=5,width=10)
par(mai=c(.5,.5,.5,.5),xaxs="i",yaxs="i")
plot(c(rev(-a),a),c(rev(lx),lx), type = 'n', las = 1, ylab = "", xlab = "", axes = FALSE, asp = 1, xlim=c(-10,10))
rect(xthano2-1-10,ychrono-1,xthano2-10,ychrono,col=thanoM2, border="white")
polygon(-c(0,rep(unique(L),each=2),0),
		c(0,0,rep(cumsum(rle(L)$length),each=2)),
		border = gray(.2))
rect(xchrono-1,ychrono-1,xchrono,ychrono,col=chronoM, border="white")

# border
polygon(c(0,rep(unique(L),each=2),0),
		c(0,0,rep(cumsum(rle(L)$length),each=2)),
		border = gray(.2))
segments(0,0,0,10,lwd=2,col="white")
axis(1)
dev.off()





# Finally 