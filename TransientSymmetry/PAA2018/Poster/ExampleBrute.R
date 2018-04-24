# Author: tim
###############################################################################
if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm", "tim-ThinkPad-L440")){
	# if I'm on the laptop
	setwd("/home/tim/git/TransientSymmetry/TransientSymmetry")
}
# -----------------

get_traj <- function(x,maxn=4){
	traj <- list()
	for (i in 1:maxn){
		li <- as.matrix(expand.grid( rep(list(c("H","S")), i)))
		traj <- c(traj,split(li,1:nrow(li)))
	}
	names(traj) <- lapply(traj,paste,collapse="")
	
	traj
}

get_probsHS <- function(traj, probs){
	traj <- c(traj,"D")
	n    <- length(traj)
	pr   <- probs[[paste0("r",traj[1])]]
	outs <- paste0(traj[1:(n-1)],traj[2:n])
	pt   <- diag(probs$Tout[,outs,drop=FALSE])
	p    <- pr * prod(pt)
	p	
}

get_TA <- function(traj, state = "S", probs, radix = 1e5,n=4){
	w   <- round(get_probsHS(traj = traj, probs = probs) * radix)
	rl  <- rle(traj)
	dur <- rl$lengths[rl$values == state]
	TA  <- c(unlist(sapply(dur,":",0)))
	tab <- table(TA) * w
	out <- rep(0,(n+1))
	names(out)      <- 0:n
	out[names(tab)] <- c(tab)
	out
}
# 
# what about an example w 6 ages?

rS <- .1
rH <- .9
# probs
HS     <- c(.1,.2,.3,.4,.6,0)
SH     <- c(.6,.5,.4,.3,.2,.0)
HD     <- c(.01,.05,.1,.2,.3,1)
SD     <- c(.2,.3,.4,.5,.7,1)
HH     <- 1 - (HS + HD)
SS     <- 1 - c(SH + SD)

Tout   <- cbind(HH, HS, HD, SS, SH, SD)

probs  <- list(rH=rH,rS=rS,Tout=Tout)


trajs    <- get_traj(c("H","S"),6)
probHS   <- lapply(trajs,get_probsHS,probs=probs)
TR       <- data.frame(traj = names(trajs),prob = unlist(probHS))


rownames(TR) <- NULL
library(xtable)
trans <- colnames(probs$Tout)
names(trans) <- c("#92a654", "#4fa9c1", "#256676", "#c99084", "#d6061a", "#84241a")

# transition rate plot
pdf("PAA2018/Poster/Figures/ToyTrans.pdf")
plot(NULL,xlim = c(0,5),ylim=c(0,1),axes=FALSE,xlab = "",ylab="")
rect(0,0,5,1,border=NA,col=gray(.91))
segments(0,seq(.1,.9,by=.1),5,seq(.1,.9,by=.1),col = "white",lwd=.5)
segments(1:4,0,1:4,1,col = "white",lwd=.5)
matplot(0:5, probs$Tout, type = 'o', 
		pch=16, 
		lty=1,
		col = names(trans),add=TRUE
		)
text(0:5,0,0:5,pos=1,xpd=TRUE)
text(0,seq(0,1,by=.2),c("0.0","0.2","0.4","0.6","0.8","1.0"),pos=2,xpd=TRUE)
dev.off()

# now show all possible trajectories:

drawTraj <- function(traj,H = "#399345", S ="#e89792",y=0,h=1){
	cols <- traj
	cols[cols == "H"] <- H
	cols[cols == "S"] <- S
	rl   <- rle(cols)
	r    <- cumsum(rl$lengths)
	n    <- length(r)
	l    <- c(0, r[-n])
	rect(l,y,r,y+h,border=NA,col=rl$values)
}

nseq <- length(trajs)
maxy <- 1
yat <- seq(maxy,0,length=(nseq+1))
#pdf("PAA2018/Poster/Figures/TrajSpace.pdf",width=5,height=12)
#plot(NULL, xlim = c(0,6+2), ylim = c(0, maxy), axes = FALSE, xlab = "", ylab = "")
#for (i in 1:nseq){
#	drawTraj(trajs[[i]], y = yat[i+1],h= maxy/nseq,H="#05872c",S=gray(.8))
#	rect(7,yat[i+1],7+TR$prob[i]*8,yat[i],border=NA,col=gray(.2))
#}
#dev.off()

asp <- 20
pdf("PAA2018/Poster/Figures/TrajSpace.pdf",width=5,height=12)
plot(NULL, xlim = c(0,8), ylim = c(0, maxy), axes = FALSE, xlab = "", ylab = "",asp=asp)
for (i in 1:nseq){
	drawTraj(trajs[[i]], y = yat[i+1],h= maxy/nseq,H="#05872c",S=gray(.8))
	rect(7,yat[i+1],7+TR$prob[i]*asp,yat[i],border=NA,col=gray(.2),xpd=TRUE)
}
segments(0:6,0,0:6,-.01,xpd=TRUE)
text(0:6,-.01,0:6,pos=1,xpd=TRUE)
segments(7,0,7+.1*asp,0,xpd=TRUE)
segments(c(7,7+.1*asp),0,c(7,7+.1*asp),-.01,xpd=TRUE)
text(c(7,7+.1*asp),-.01,c(0,"0.1"),pos=1,xpd=TRUE)
dev.off()

yat2 <- cumsum(c(0,TR$prob))
yat2 <- rev(cumsum(c(0,rev(TR$prob))))


pdf("PAA2018/Poster/Figures/TrajProbs.pdf", width = 5, height = 12)
plot(NULL, xlim = c(0, 8), ylim = c(0, maxy), axes = FALSE, xlab = "", ylab = "",asp=asp)
for (i in 1:nseq){
	drawTraj(trajs[[i]], y = yat2[i+1],h= TR$prob[i],H="#05872c",S=gray(.8))
}
segments(0:6,0,0:6,-.01,xpd=TRUE)
text(0:6,-.01,0:6,pos=1,xpd=TRUE)
dev.off()

#TR[which.max(TR$prob),]
#TAlist <- lapply(trajs,get_TA,state="S",probs=probs,radix=1e5)
#TA <- colSums(do.call("rbind",TAlist))

# now get prevalence as of a Markov model
Hx <- Sx <- rep(0,6)
for (i in 1:6){
	Hi <- unlist(lapply(trajs, function(traj,i){
				TF <- length(traj) >= i
				if (TF){
					TF <- traj[i] == "H"
				}
				TF
			},i=i))
    Hx[i] <- sum(TR$prob[Hi])
	Si <- unlist(lapply(trajs, function(traj,i){
						TF <- length(traj) >= i
						if (TF){
							TF <- traj[i] == "S"
						}
						TF
					},i=i))
	Sx[i] <- sum(TR$prob[Si])
}

pdf("PAA2018/Poster/Figures/TrajPrev.pdf", width = 5, height = 12)
plot(NULL, xlim = c(0, 8), ylim = c(0, maxy), axes = FALSE, xlab = "", ylab = "",asp=asp)
rect(0:5,0,1:6,Hx,col="#05872c",border=NA)
rect(0:5,Hx,1:6,Sx+Hx,col=gray(.8),border=NA)
polygon(c(0,rep(1:5,each=2),6,6,rep(5:1,each=2),0),
		c(rep(Sx+Hx,each=2),rep(0,12)))
segments(0:6,0,0:6,-.01,xpd=TRUE)
text(0:6,-.01,0:6,pos=1,xpd=TRUE)
segments(0,c(0,1),-.01*asp,c(0,1),xpd=TRUE)
text(0,c(0,1),c(0,1),pos=2,xpd=TRUE)
dev.off()

# how about spell duration prevalence

pdf("PAA2018/Poster/Figures/TrajExample.pdf", width = 5, height = 5)
plot(NULL, xlim=c(0,6),ylim=c(0,1),xlab="",ylab = "",axes=FALSE)
drawTraj(c("H","S","H","H","S","S"),y=.4,h=.2,H="#05872c",S=gray(.8))
axis(1)
dev.off()

drawTrajPC <-  function(traj,H = "#05872c", S =gray(.8),x=0,w=.2,...){
	cols <- traj
	cols[cols == "H"] <- H
	cols[cols == "S"] <- S
	rl   <- rle(cols)
	len  <- rl$lengths
	r    <- cumsum(rl$lengths)
	n    <- length(r)
	l    <- c(0, r[-n])
	
	X <- c(rbind(l,l+len,l+len,l,NA)) + x
	Y <- c(rbind(l,l+len,l+len+w,l+w,NA))
	polygon(X,Y,border=NA,col=rl$values,xpd=TRUE,...)
}



pdf("PAA2018/Poster/Figures/TrajExamplePC.pdf", width = 5, height = 12)
plot(NULL, xlim=c(0,13),ylim=c(0,7),xlab="",ylab = "",axes=FALSE,asp=1)
for (i in 0:8){
	drawTrajPC(traj=c("H","S","H","H","S","S"),x=i,H="#05872c",S=gray(.8),w=.5)	
}
#drawTrajPC(traj=c("H","S","H","H","S","S"),x=0,H="#05872c",S=gray(.8),w=1)
axis(1,at=0:12,pos=-.1)
axis(2,las=1,at=0:6,pos=-.1)
dev.off()

pdf("PAA2018/Poster/Figures/TrajExamplePCzoom.pdf", width = .4*5, height = 2.5*5)
par(xaxs="i",yaxs="i",mai=c(0,0,0,0))
plot(NULL, xlim=c(4.8,5.2),ylim=c(2,4.5),xlab="",ylab = "",axes=FALSE,asp=1)
for (i in 0:8){
	drawTrajPC(traj=c("H","S","H","H","S","S"),x=i,H="#05872c",S=gray(.8),w=.5)	
}
#drawTrajPC(traj=c("H","S","H","H","S","S"),x=0,H="#05872c",S=gray(.8),w=1)
#axis(1,at=0:12,pos=-.1)
#axis(2,las=1,at=0:6,pos=-.1)
dev.off()




#
#pdf("Figures/ToyDist.pdf")
#par(mai=c(.5,.2,.5,0))
#plot(NULL, xlim = c(-4,4),ylim = c(0,57000), axes=FALSE, xlab = "",ylab = "")
#segments(-4.1,TA[-1],1:4,TA[-1],col=gray(.6))
#text(-3.7, TA[2:4],TA[2:4], pos = 3)
#text(-4.1,c(-2000,TA[5]+300),c(0,TA[5]),pos=2,xpd=TRUE)
#segments(-4.1,0,-4.2,-2000,xpd=TRUE)
#segments(-4.1,0,-4.2,TA[5]+300,xpd=TRUE)
#
#segments(0:4,TA,0:4,0)
#segments(0:-4,TA,0:-4,0)
#lines(-4:4,c(rev(TA[-1]),TA))
#segments(-4,0,4,0)
#text(-4:4,0,c(4:0,1:4),pos=1,xpd=TRUE)
#text(-2,-6000,"time spent",xpd=TRUE,cex=1.5)
#text(2,-6000,"time left",xpd=TRUE,cex=1.5)
#segments(0,-4000,0,-8000,xpd=TRUE)
#text(-3.9,61000,"Count",cex=1.5,xpd=TRUE)
#dev.off()
