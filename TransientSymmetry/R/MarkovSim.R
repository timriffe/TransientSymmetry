
# Author: tim
###############################################################################

library(markovchain)

# here I just read in the first matrix given in the supplementary material to:
# Christian Dudel & Mikko Myrskylä (2017)
# Working Life Expectancy at Age 50 in the United States and the Impact of the Great Recession
# Demography https://link.springer.com/article/10.1007/s13524-017-0619-6


# get a transition matrix
TM <- as.matrix(
				read.csv("/home/tim/git/TransientSymmetry/TransientSymmetry/Data/Pmat_b_f_1994.csv",
				check.names=FALSE)
)


# transpose for standard MC stuff.
# demographers do stuff backwards I guess
TM <- t(TM)

# make s4 transition matrix from markovchain package
mcEmpl <- new("markovchain", states = rownames(TM),
		byrow = TRUE, transitionMatrix = TM,
		name = "Empl")

# how many sequences should we generate?
N      <- 1e4

# each assuming a start in employment at age 50.
RTraj  <- replicate(N,
		    rmarkovchain(n = 50, object = mcEmpl, t0 = "50::Employed", parallel = TRUE)
		  ) 
RTraj                 <- rbind(rep("50::Employed",N), RTraj)
RTraj_clean           <- gsub(".*:","",RTraj)
rownames(RTraj_clean) <- 50:100
# each row is an age, and each column a person.

# so this is like a stationary population of people born employed at age 50.
# what form does the data need to be in to demonstrate symmetry?

# these people are from a single cohort, so we need to repeat this cohort 50+ times,
# but in the cross-section, we'd obviously just end up with the same thing.

# neat twist: could see how well distributions match if each cohort is separately generated
# w  random markov process.


# write a function, where given a selected point, we look at the state and how much is before or after it?
# either total time in state or only present spell. Both should be symmetrical in limit.

get_a_t <- function(x, pos, spent = TRUE, spell = TRUE){
	sec              <- rle(x)
	exit.from        <- cumsum(sec$lengths) 
	this.state       <- x[pos]
	names(exit.from) <- sec$values
	entry.to         <- exit.from - sec$lengths + 1
	
	# look in this spell or total time in state?
	if (spent){
		if (!spell){
			ti       <- sum(x[1:pos] == this.state) - .5
		}
		if (spell){
			this.spell <- which(exit.from >= pos & entry.to <= pos)
			ti       <- pos - entry.to[this.spell] + .5
		}
	} else {
		if (!spell){
			ti         <- sum(x[pos:length(x)] == this.state) - .5
		}
		if (spell){
			this.spell <- which(exit.from >= pos & entry.to <= pos)
			ti         <- exit.from[this.spell] - pos + .5
		}
	}
	ti
}

# ranges between 0 and 1. 1 is complete separation, 0 is complete overlap.
dist.diff <- function(x,y){
	ref <- sort(unique(c(x, y)))
	X   <- c(table(factor(x,levels = ref)))
	Y   <- c(table(factor(y,levels = ref)))
	X   <- X / sum(X)
	Y   <- Y / sum(Y)
	sum(abs(X - Y)) / 2
}

# OK, now the symmetry test, draw N inactive people with replacement
#

NN             <- 1000
state          <- "Inactive"

inactives      <- which(RTraj_clean == state, arr.ind = TRUE)
SampleInactive <- sample(1:nrow(inactives), size = NN, replace = TRUE)

# this isn't too slow
time_left_spell <- apply(inactives[SampleInactive, ], 1, function(y, RTraj_clean){
			x <- RTraj_clean[,y[2]]
			get_a_t(x, pos = y[1], spent = FALSE, spell = TRUE)
		} ,RTraj_clean = RTraj_clean)
time_spent_spell <- apply(inactives[SampleInactive, ], 1, function(y, RTraj_clean){
			x <- RTraj_clean[,y[2]]
			get_a_t(x, pos = y[1], spent = TRUE, spell = TRUE)
		}, RTraj_clean = RTraj_clean)
time_left_total <- apply(inactives[SampleInactive, ], 1, function(y, RTraj_clean){
			x <- RTraj_clean[,y[2]]
			get_a_t(x, pos = y[1], spent = FALSE, spell = FALSE)
		}, RTraj_clean = RTraj_clean)
time_spent_total <- apply(inactives[SampleInactive, ], 1, function(y, RTraj_clean){
			x <- RTraj_clean[,y[2]]
			get_a_t(x, pos = y[1], spent = TRUE, spell = FALSE)
		}, RTraj_clean = RTraj_clean)


# change N and see how the distribution difference drops
dist.diff(time_left_spell, time_spent_spell)
dist.diff(time_left_total, time_spent_total)

#plot(density(time_left_spell,adjust=2))
#lines(density(time_spent_spell,adjust=2))


#sum(time_left_spell)
#sum(time_spent_spell)
#sum(time_left_total)
#sum(time_spent_total)

#breaks <- 0:30
#par(mfrow = c(1,2))
#hist(time_left_total,breaks=breaks,ylim=c(0,200))
#hist(time_spent_total,breaks=breaks,ylim=c(0,200))
#
#par(mfrow=c(1,2))
#hist(time_left_spell,breaks=breaks,ylim=c(0,250))
#hist(time_spent_spell,breaks=breaks,ylim=c(0,250))

# -------------------------------
# other neat stuff:
# -------------------------------

#plot(density(time_left_spell,adjust=3))
#lines(density(time_spent_spell,adjust=3))

# in case we want to re-estimate transition matrices: age patterns
# won't be smooth tho
#SeqMat <- createSequenceMatrix(stringchar = c(RTraj), sanitize = FALSE)
#
#EmplMcFitMle <- markovchainFit(data = c(RTraj), method = "mle", name = "Empl")
#
#TMest <- EmplMcFitMle$estimate@transitionMatrix
#TMblank <- TM * 0
#TMblank[rownames(TMest), colnames(TMest)] <- TMest



