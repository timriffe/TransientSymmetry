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

get_TA <- function(traj, state = "S", probs, radix = 1e5){
	w   <- round(get_probsHS(traj = traj, probs = probs) * radix)
	rl  <- rle(traj)
	dur <- rl$lengths[rl$values == state]
	TA  <- c(unlist(sapply(dur,":",0)))
	tab <- table(TA) * w
	out <- rep(0,5)
	names(out) <- 0:4
	out[names(tab)] <- c(tab)
	out
}
# A small brute-force example 4 ages, 2 states.

# radixes
rS <- .1
rH <- .9
# probs
HS     <- c(.1,.2,0)
SH     <- c(.7,.5,.0)
HD     <- c(.01,.1,1)
SD     <- c(.2,.3,1)
HH     <- 1 - (HS + HD)
SS     <- 1 - c(SH + SD)

Tout   <- cbind(HH, HS, HD, SS, SH, SD)

probs  <- list(rH=rH,rS=rS,Tout=Tout)



trajs    <- get_traj(c("H","S"),3)
probHS   <- lapply(trajs,get_probsHS,probs=probs)
TR       <- data.frame(traj = names(trajs),prob = unlist(probHS))
rownames(TR) <- NULL
library(xtable)

print(xtable(probs$Tout,digits=2))
print(xtable(TR,digits=4),include.rownames=FALSE)




# what about an example w 4 ages?

rS <- .1
rH <- .9
# probs
HS     <- c(.1,.2,.3,0)
SH     <- c(.7,.5,.3,.0)
HD     <- c(.01,.1,.2,1)
SD     <- c(.2,.3,.4,1)
HH     <- 1 - (HS + HD)
SS     <- 1 - c(SH + SD)

Tout   <- cbind(HH, HS, HD, SS, SH, SD)

probs  <- list(rH=rH,rS=rS,Tout=Tout)


trajs    <- get_traj(c("H","S"),4)
length(trajs)
probHS   <- lapply(trajs,get_probsHS,probs=probs)
TR       <- data.frame(traj = names(trajs),prob = unlist(probHS))
rownames(TR) <- NULL
library(xtable)

print(xtable(probs$Tout,digits=2))
print(xtable(TR,digits=5),include.rownames=FALSE)
TR2 <- cbind(TR[1:15,],TR[16:30,])
print(xtable(TR2,digits=5),include.rownames=FALSE)



TAlist <- lapply(trajs,get_TA,state="S",probs=probs,radix=1e5)
TA <- colSums(do.call("rbind",TAlist))


pdf("Figures/ToyDist.pdf")
par(mai=c(.5,.2,.5,0))
plot(NULL, xlim = c(-4,4),ylim = c(0,57000), axes=FALSE, xlab = "",ylab = "")
segments(-4.1,TA[-1],1:4,TA[-1],col=gray(.6))
text(-3.7, TA[2:4],TA[2:4], pos = 3)
text(-4.1,c(-2000,TA[5]+300),c(0,TA[5]),pos=2,xpd=TRUE)
segments(-4.1,0,-4.2,-2000,xpd=TRUE)
segments(-4.1,0,-4.2,TA[5]+300,xpd=TRUE)

segments(0:4,TA,0:4,0)
segments(0:-4,TA,0:-4,0)
lines(-4:4,c(rev(TA[-1]),TA))
segments(-4,0,4,0)
text(-4:4,0,c(4:0,1:4),pos=1,xpd=TRUE)
text(-2,-6000,"time spent",xpd=TRUE,cex=1.5)
text(2,-6000,"time left",xpd=TRUE,cex=1.5)
segments(0,-4000,0,-8000,xpd=TRUE)
text(-3.9,61000,"Count",cex=1.5,xpd=TRUE)
dev.off()
