
# Author: tim
###############################################################################


b1 <- seq(-12,12,by=.2)
b2 <- seq(-12,12,by=.1)
entry <- 2
exit  <- 5
l     <- 6
pdf("/home/tim/git/TransientSymmetry/TransientSymmetry/Figures/lifelinerepeated.pdf",
		height=6,width=9)
plot(0:10,0:10,type="n",axes=FALSE,asp=1,xlab="",ylab="")
segments(b1+1,1,b1+l,l,lwd=.5,col=gray(.6),lty=1)
segments(b2+entry,0+entry,b2+exit,exit,lwd=1,col=gray(.2),lty=1)

segments(5,0,5,l+1)

a    <- seq(2,5,by=.1)
segments(a,2,5,rev(a),col = "blue",lwd=1.1)
segments(a+3,5,5,rev(a),col = "red",lwd=1.1)
dev.off()



