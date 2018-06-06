
# Author: tim
###############################################################################

setwd("/home/tim/git/TransientSymmetry/TransientSymmetry/Pres1")

source("R/Functions.R")
# sampling over time.

drawDuration <- function(
		delta = 1, # example duration
		SL = 10.8, # example duration
		OA = 2,    # onset age
		AO = 2,     # time after obs
		thisK = 4
){
	
	K <- floor(SL / delta)
	
	par(xaxs = "i", yaxs = "i", mai = c(0,0,0,0))
	plot(NULL, type = "n", xlim = c(0, AO + OA + SL), ylim = c(.1,1.9), asp = 1, axes = FALSE,
			xlab = "", ylab = "")
# background dashed line
	segments(0, 1, OA + SL + AO, 1, col = gray(.6), lty = "8383", lwd = 3)
	
# duration line
	segments(OA, 1, OA + SL, 1, col = "black", lwd = 3)
	
# end segments
# segment size 
	segs <- .2
	segments(OA,1-segs,OA,1+segs, lwd = 2)
	segments(OA + SL, 1 - segs, OA + SL, 1 + segs, lwd = 2)
	
# delta slices:
	
	segments(OA + (1:K) * delta, 1 - (segs/2), OA + (1:K) * delta, 1+ (segs/2))
	
# pointer for present K
	#segments(OA + thisK * delta, 1 + (segs * 1.5), OA + thisK * delta, 1, lwd = 2, col = "red")
	#segments(OA + thisK * delta, 1 - (segs * 1.5), OA + thisK * delta, 1, lwd = 2, col = "blue")
	
# reference brackets
	
	uprad    <- ifelse(delta * thisK < .8, (delta * thisK)/4, .2)
	downrad  <- ifelse(delta / 4 < .2 & thisK == K,delta/4,.2)
	
	# top bracket
	if (thisK == 0){
		segments(OA, 1.4, OA, 1, col = "red")
	} else {
		curlyBrace1(xl = OA, y = 1 + (segs * 1.5) + .3, length = thisK * delta, top = TRUE, col = "red", radius1 = uprad, radius2 = uprad)
		
	}
	# bottom bracket
	if (thisK == K){
		segments(OA + K * delta, .6, OA + K * delta, 1, col = "blue")
	} else {
		curlyBrace1(xl = OA + thisK * delta, 
				y = 1 - (segs * 1.5) - .3, 
				length = (K - thisK) * delta, top = FALSE, col = "blue", radius1 = downrad, radius2 = downrad)
	}

}




delta  <- .7
SL     <- 10.8
OA     <- 2
AO     <- 2
maxK   <- floor(SL / delta)
thisKs <- c(0,1,2,3,maxK-1,maxK)
k <- 1
for (k in thisKs){
	path <- paste0("Figures/SingleLifeAnim1/step",k,".pdf")
	pdf(path,width = 10, height = 1)
	drawDuration(delta = delta, # example duration
			SL = SL, # example duration
			OA = OA,    # onset age
			AO = AO,
			thisK = k)
	dev.off()
}

path <- paste0("Figures/SingleLifeAnim1/stepsmalldelta.pdf")
pdf(path,width = 10, height = 1)
drawDuration(delta = delta/2, # example duration
		SL = SL, # example duration
		OA = OA,    # onset age
		AO = AO,
		thisK = floor(SL/(delta/2)))
dev.off()

# -------------------------------------------------
# likewise if we sample in diagonal.

drawDurationD <- function(
		delta = 1, # time step
		SL = 10.8, # example duration
		OA = 2,    # onset age
		AO = 2,     # time after obs
		N = 1,
		add = FALSE,
		...
){
	
	K <- floor(SL / delta)
	# segment size 
	segs <- .2
	if (!add){
	par(xaxs = "i", yaxs = "i", mai = c(0,0,0,0))
	plot(NULL, type = "n", xlim = c(0,SL*2+6), ylim = c(0,SL+2), asp = 1, axes = FALSE,
			xlab = "", ylab = "")
}
# background dashed line
	#segments(0, 1, OA + SL + AO, 1, col = gray(.6), lty = "8383", lwd = 3)
	OAi <- OA-delta
	inc <- (0:K) * delta
	for (i in 1:N){
# duration line
		OAi <- OAi + delta
		segments(OAi, 1, OAi + SL, 1 + SL, ...)
		segments(OAi + inc, 
				1 + inc - (segs/2), 
				OAi + inc, 
				1 + inc + (segs/2), 
				...)
		
	}

}

delta  <- .7
SL     <- 10.8
OA     <- 2
AO     <- 2
maxK   <- floor(SL / delta)
Ns     <- c(1,2,3,maxK,maxK+1)
for (n in Ns){
	path <- paste0("Figures/buildstationary/step",n,".pdf")
	pdf(path,width = 10, height = 5)
	drawDurationD(delta = delta, # example duration
			SL = SL, # example duration
			OA = OA,    # onset age
			AO = AO,
			N = n,
			lwd = 1,
			col = gray(.5))
	dev.off()
}


drawDurationD2 <- function(
		delta = 1, # time step
		SL = 10.8, # example duration
		OA = 2,    # onset age
		#N = 1,
		add = FALSE,
		xshift = 0,
		N,
		splittime = TRUE,
		...
){
	
	K <- floor(SL / delta)
	# segment size 
	segs <- .2
	if (!add){
		par(xaxs = "i", yaxs = "i", mai = c(0,0,0,0))
		plot(NULL, type = "n", xlim = c(0,SL*2+6), ylim = c(0,SL+2), asp = 1, axes = FALSE,
				xlab = "", ylab = "")
	}
# background dashed line
	#segments(0, 1, OA + SL + AO, 1, col = gray(.6), lty = "8383", lwd = 3)
	OAi  <- -delta + xshift
	inc  <- (0:K) * delta
	rinc <- rev(inc)
	if (missing(N)){
		N    <- K + 1
	}

	for (i in 1:N){
    # duration line
		OAi <- OAi + delta
		# time spent line
		if (splittime){
			segments(OAi, OA, OAi + rinc[1] - inc[i], OA + rinc[1] - inc[i], col = "red", lwd = 1)
			# time left line
			segments(OAi + rinc[i], OA + rinc[i], OAi + rinc[1], OA + rinc[1], col = "blue", lwd = 1)
			
			segments(OAi + rinc[1], OA + rinc[1], OAi + SL, OA + SL, ...)
		}
		if (!splittime){
		    segments(OAi, OA, OAi + SL, OA + SL, ...)
	    }
		segments(OAi + inc, 
				OA + inc - (segs/2), 
				OAi + inc, 
				OA + inc + (segs/2), 
				...)
		
	}
	
}

delta  <- .7
SL     <- 10.8
OA     <- 2
AO     <- 2
maxK   <- floor(SL / delta)
Ns     <- c(1,2,3,maxK,maxK+1)
for (n in Ns){
	path <- paste0("Figures/buildstationary/step",n,".pdf")
	pdf(path,width = 10, height = 5)
	par(xaxs = "i", yaxs = "i", mai = c(0,0,0,0))
	plot(NULL, type = "n", xlim = c(-2,22), ylim = c(0,13), asp = 1, axes = FALSE,
			xlab = "", ylab = "")
	drawDurationD2(delta = delta, # example duration
			SL = SL, # example duration
			OA = OA,    # onset age
			N = n,
			lwd = 1,
			col = gray(.5),
			splittime = FALSE,
			add = TRUE)
	dev.off()
}
path <- paste0("Figures/buildstationary/spl0.pdf")
pdf(path,width = 10, height = 5)
par(xaxs = "i", yaxs = "i", mai = c(0,0,0,0))
plot(NULL, type = "n", xlim = c(-2,22), ylim = c(0,13), asp = 1, axes = FALSE,
		xlab = "", ylab = "")
drawDurationD2(delta = delta, # example duration
		SL = 10.8, # example duration
		OA = 2,    # onset age
		lwd = 1,
		col = gray(.5),
		splittime=FALSE,
		add=TRUE)
segments(10.5,0,10.5,15)
dev.off()
path <- paste0("Figures/buildstationary/spl1.pdf")
pdf(path,width = 10, height = 5)
par(xaxs = "i", yaxs = "i", mai = c(0,0,0,0))
plot(NULL, type = "n", xlim = c(-2,22), ylim = c(0,13), asp = 1, axes = FALSE,
		xlab = "", ylab = "")
drawDurationD2(delta = delta, # example duration
		SL = 10.8, # example duration
		OA = 2,    # onset age
		lwd = 1,
		col = gray(.5),
		add=TRUE)
dev.off()

path <- paste0("Figures/buildstationary/spl2.pdf")
pdf(path,width = 10, height = 5)
par(xaxs = "i", yaxs = "i", mai = c(0,0,0,0))
plot(NULL, type = "n", xlim = c(-2,22), ylim = c(0,13), asp = 1, axes = FALSE,
		xlab = "", ylab = "")
drawDurationD2(delta = delta, # example duration
		SL = 5.7, # example duration
		OA = 4.4,    # onset age
		lwd = 1,
		col = gray(.5),
		xshift = 4.9,
		add=TRUE)
dev.off()

path <- paste0("Figures/buildstationary/spl3.pdf")
pdf(path,width = 10, height = 5)
par(xaxs = "i", yaxs = "i", mai = c(0,0,0,0))
plot(NULL, type = "n", xlim = c(-2,22), ylim = c(0,13), asp = 1, axes = FALSE,
		xlab = "", ylab = "")
drawDurationD2(delta = delta, # example duration
		SL = 10.8, # example duration
		OA = 2,    # onset age
		lwd = 1,
		col = gray(.5),
		add=TRUE)
drawDurationD2(delta = delta, # example duration
		SL = 5.7, # example duration
		OA = 4.4,    # onset age
		lwd = 1,
		col = gray(.5),
		xshift = 4.9,
		add=TRUE)
dev.off()
