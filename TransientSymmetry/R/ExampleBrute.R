# Author: tim
###############################################################################

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

trajs    <- get_traj(c("H","S"),3)
probHS   <- lapply(trajs,get_probsHS,probs=probs)
TR       <- data.frame(traj = names(trajs),prob = unlist(probHS))
rownames(TR) <- NULL
library(xtable)

print(xtable(probs$Tout,digits=2))
print(xtable(TR,digits=4),include.rownames=FALSE)
