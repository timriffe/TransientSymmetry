# Author: tim
###############################################################################
if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm", "tim-ThinkPad-L440")){
	# if I'm on the laptop
	setwd("/home/tim/git/TransientSymmetry/TransientSymmetry")
}
# -----------------

# gets all possible trajectories assuming nonzero
# transition probs. x is a character vector of states (one character per state)
# results is list with character vector of state sequence. In list because of
# variable length
get_traj <- function(x,maxn=4){
	traj <- list()
	for (i in 1:maxn){
		li <- as.matrix(expand.grid( rep(list(x), i)))
		traj <- c(traj,split(li,1:nrow(li)))
	}
	names(traj) <- lapply(traj,paste,collapse="")
	
	traj
}

# for a given trajectory and set of transition probabilities
# what is the total probability of observing this trajectory.
get_probsHS <- function(traj, probs){
	traj <- c(traj,"D")  # end traj in death
	n    <- length(traj)
	# get birth state probs
	pr   <- probs[[paste0("r",traj[1])]]
	# get to/from concatenations, which is how trans probs are named
	outs <- paste0(traj[1:(n-1)],traj[2:n])
	# located in diag, get prob vector
	pt   <- diag(probs$Tout[,outs,drop=FALSE])
	# product
	p    <- pr * prod(pt)
	p	
}

get_TA <- function(traj, state = "S", probs, radix = 1e5,n=4){
	# this assumes we have a large radix, otherwise we wouldn't round!
	# ideally radix chosen to match digit precision of probabilities.
	w   <- round(get_probsHS(traj = traj, probs = probs) * radix)
	# get all spell durations
	rl  <- rle(traj)
	# select to desired state
	dur <- rl$lengths[rl$values == state]
	# all T and A values
	TA  <- c(unlist(sapply(dur,":",0)))
	# tabulate values and weight 
	tab <- table(TA) * w
	# fit into standard-length output
	out <- rep(0,(n+1))
	names(out)      <- 0:n
	out[names(tab)] <- c(tab)
	out
}

# draw a trajectory as horizontal rect, hard coded to H and S
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
# same, but right-aligned. Used to show non-symmetry in poster
drawTrajr <- function(traj,H = "#399345", S ="#e89792",y=0,h=1,maxl=7){
	cols <- traj
	cols[cols == "H"] <- H
	cols[cols == "S"] <- S
	rl   <- rle(cols)
	r    <- cumsum(rl$lengths)
	n    <- length(r)
	l    <- c(0, r[-n])
	d    <- maxl - max(r)
	r    <- r + d
	l    <- l + d
	rect(l,y,r,y+h,border=NA,col=rl$values)
}
# draw a trajectory in PC diagonal, used for step 2 in poster
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
# non-rounded TA, overwrite previous. Hard coded length!
get_TA <- function(traj, state = "S", probs, radix = 1){
	w   <- get_probsHS(traj = traj, probs = probs) * radix
	rl  <- rle(traj)
	dur <- rl$lengths[rl$values == state]
	TA  <- c(unlist(sapply(dur,":",0)))
	tab <- table(TA) * w
	out <- rep(0,7)
	names(out)      <- 0:6
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
#transition probs in a single matrix, with directions coded in
# column names
Tout   <- cbind(HH, HS, HD, SS, SH, SD)
# all necessary probabilities together
probs  <- list(rH=rH,rS=rS,Tout=Tout)

# all possible trajectories assuming non-zero transition rates
# between the specified states
trajs    <- get_traj(x=c("H","S"),6)
probHS   <- lapply(trajs,get_probsHS,probs=probs)
TR       <- data.frame(traj = names(trajs),prob = unlist(probHS))
#which(TR$traj=="HSHHSS")

rownames(TR) <- NULL
library(xtable)
# for making transition probability line plot
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

nseq <- length(trajs)
maxy <- 1
yat  <- seq(maxy,0,length=(nseq+1))

# aspect ratio shared in several plots
asp <- 20
# the trajectory space
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

# the probability-weighted trajectory space, aka trajectory composition
#yat2 <- cumsum(c(0,TR$prob))
yat2 <- rev(cumsum(c(0,rev(TR$prob))))


pdf("PAA2018/Poster/Figures/TrajProbs.pdf", width = 5, height = 12)
plot(NULL, xlim = c(0, 8), ylim = c(0, maxy), axes = FALSE, xlab = "", ylab = "",asp=asp)
for (i in 1:nseq){
	drawTraj(trajs[[i]], y = yat2[i+1],h= TR$prob[i],H="#05872c",S=gray(.8))
}
segments(0:6,0,0:6,-.01,xpd=TRUE)
text(0:6,-.01,0:6,pos=1,xpd=TRUE)
dev.off()

# same, but aligned on death, shows non symmetry
pdf("PAA2018/Poster/Figures/TrajProbsTTD.pdf", width = 5, height = 12)
plot(NULL, xlim = c(0, 8), ylim = c(0, maxy), axes = FALSE, xlab = "", ylab = "",asp=asp)
for (i in 1:nseq){
	drawTrajr(trajs[[i]], y = yat2[i+1],h= TR$prob[i],H="#05872c",S=gray(.8))
}
segments(1:7,0,1:7,-.01,xpd=TRUE)
text(1:7,-.01,6:0,pos=1,xpd=TRUE)
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
# and repeat for TTD prevalence because we can!
# to show non-symmetry
Hy <- Sy <- rep(0,6)
for (i in 1:6){
	# traj <- c("H","S","H","S","S","H")
	Hi <- unlist(lapply(trajs, function(traj,i){
						TF <- length(traj) >= i
						if (TF){
							TF <- rev(traj)[i] == "H"
						}
						TF
					},i=i))
	Hy[i] <- sum(TR$prob[Hi])
	Si <- unlist(lapply(trajs, function(traj,i){
						TF <- length(traj) >= i
						if (TF){
							TF <- rev(traj)[i] == "S"
						}
						TF
					},i=i))
	Sy[i] <- sum(TR$prob[Si])
}


# the asymptotic prevalence functions (same as those
# returned by fundamental matrix)
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

# same prevalence right aligned (non-symmatrical)
pdf("PAA2018/Poster/Figures/TrajPrevTTD.pdf", width = 5, height = 12)
plot(NULL, xlim = c(0, 8), ylim = c(0, maxy), axes = FALSE, xlab = "", ylab = "",asp=asp)
rect(0:5,0,1:6,Hy,col="#05872c",border=NA)
rect(0:5,Hy,1:6,Hy+Sy,col=gray(.8),border=NA)
polygon(c(0,rep(1:5,each=2),6,6,rep(5:1,each=2),0),
		c(rep(Sy+Hy,each=2),rep(0,12)))
segments(0:6,0,0:6,-.01,xpd=TRUE)
text(0:6,-.01,0:6,pos=1,xpd=TRUE)
segments(0,c(0,1),-.01*asp,c(0,1),xpd=TRUE)
text(0,c(0,1),c(0,1),pos=2,xpd=TRUE)
dev.off()

# get actual proportions, show non-symmetry
pdf("PAA2018/Poster/Figures/PrevPropCompare.pdf")
plot(0:5,Sx/(Sx+Hx),type='l',ylim=c(0,1),axes=FALSE,xlab="",ylab="")
lines(0:5,Sy/(Sy+Hy))
axis(1);axis(2,las=1)
dev.off()

# how about spell duration prevalence
# this is trajectory nr 113, used as example in poster
pdf("PAA2018/Poster/Figures/TrajExample.pdf", width = 5, height = 5)
plot(NULL, xlim=c(0,6),ylim=c(0,1),xlab="",ylab = "",axes=FALSE)
drawTraj(c("H","S","H","H","S","S"),y=.4,h=.2,H="#05872c",S=gray(.8))
axis(1)
dev.off()

# same, drawn in PC in even steps. Narrower traj (actually has no width)
pdf("PAA2018/Poster/Figures/TrajExamplePC.pdf", width = 5, height = 12)
plot(NULL, xlim=c(0,13),ylim=c(0,7),xlab="",ylab = "",axes=FALSE,asp=1)
for (i in 0:8){
	drawTrajPC(traj=c("H","S","H","H","S","S"),x=i,H="#05872c",S=gray(.8),w=.5)	
}
#drawTrajPC(traj=c("H","S","H","H","S","S"),x=0,H="#05872c",S=gray(.8),w=1)
axis(1,at=0:12,pos=-.1)
axis(2,las=1,at=0:6,pos=-.1)
dev.off()

# zoom in to show period equivalency
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


TAlist <- lapply(trajs,get_TA,state="S",probs=probs,radix=1)
TA     <- colSums(do.call("rbind",TAlist))
TA     <- TA / sum(TA)
# used for final symmetry figure in poster
pdf("PAA2018/Poster/Figures/TAdist.pdf")
barplot(TA, space = 0,las=1, col = gray(.8))
dev.off()

# sickness distributions.


# how about spell duration distribution instead? Not used in poster.
#traj <- trajs[[100]]
#EPL <- colSums(do.call(rbind,lapply(trajs, function(traj,state="S",probs){
#			w <- get_probsHS(traj = traj, probs = probs) 
#			durs<- rep(0,6)
#			names(durs) <- 1:6
#			rl <- rle(traj)
#			val <- rl$values
#			len <- rl$lengths
#			episodes <- table(len[val == state]) * w
#			if (length(episodes) > 0){
#				durs[names(episodes)] <- c(episodes)
#			}
#			durs
#		},probs=probs)))
#
#plot(1:6,EPL,type='o',pch=16,axes=FALSE,xlab="",ylab="")
#axis(1)
#axis(2,las=1)
#barplot(EPL,space=0,las=1)


# these were experiments for that final figure:
# really you can see variation better in logged plot,
# but trans probs end up making a straight line in log space!
#TA <- TA / sum(TA)
#plot(NULL,xlim=c(-6,6),ylim=c(0,.5),axes = FALSE,xlab="",ylab="")
#
#
#plot(-6:6,c(rev(TA),TA[-1]), type='l',
#		pch=16,
#		xlim = c(-6,6),
#		axes=FALSE, xlab = "",ylab = "",
#		log='y')
#axis(1,at=-6:6,labels=c(c(6:0,1:6)))
#axis(2,las=1,at=1/10^(0:5),labels=c("1","1/10","1/100","1/1000","1/10k","1/100k"),xpd=TRUE)
#abline(v=0)
#
#
#segments(-6,0,6,0)
#text(-6:6,0,c(6:0,1:6),pos=1,xpd=TRUE)

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
