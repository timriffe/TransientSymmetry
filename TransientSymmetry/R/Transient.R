	
# Author: tim
###############################################################################
# does carey's inequality hold for transient states?
# the main difference is that transient states are not
# aligned on entry or exit, much less obvious.
# this simulation is set up for an illness-death model. i.e., the only
# transition out of the state is death. Actually the transition type
# does not matter, and it's easy to see that if this one holds that
# intermediate transient states would have the same property.
###############################################################################
# 1) set up parameters by designing the model cohort that gets cloned:

# number of people in each cohort
Nindv   <- 100
# proportion of cohort that experiences transition
PropS   <- .5
# mean duration of life:
e0      <- 80
# mean duration of condition:
Dur     <- 10
# year of cohort:
CohBase <- 1900
# how many cohorts we replicate?
Ncohs   <- 300
# decide census date in the middle (can be any date, even exact.
Census <- CohBase + Ncohs / 2

# now simulate cohort
set.seed(1)
# person ids
ids   <- 1:Nindv
# proportion of year completed at birth (e.g. exact date)
pyr   <- runif(Nindv) # this distribution doesn't matter, as long as between 0 and 1
# lifespans. generation method also doesn't matter, as long as positive
L     <- rnorm(n=Nindv,mean=e0,sd=15)
# nr indv with transition. doesn't matter if all or only some experience state transition
Ns    <- round(PropS*Nindv)
# who has transition?
has   <- sample(ids,size=Ns,replace=FALSE) 

# and how long do they have it for
DurS  <- rpois(Ns,Dur) + runif(Ns)
DS    <- rep(0,Nindv)
DS[ids%in%has] <- DurS
DateS    <- (CohBase + pyr + L - DS) * (ids%in%has)
DateS[DateS == 0] <- NA
# The only important thing is that DS < L, conicidentlally true in this setup.

# Make data object for cohort
# this is the cohort that gets cloned
DatC  <- data.frame(id = ids,
		            dob = CohBase + pyr,
					dod = CohBase + pyr + L,
					trans = ids %in% has,
					onset = DateS,
					Dur =DS )

# combine to Ncohs overlapping generations		
Dat <- do.call(rbind,lapply(1:Ncohs,function(i,Dat){
			Dat[,c("dob","dod","onset")] <- Dat[,c("dob","dod","onset")] + i
			Dat
		}, Dat = DatC))

# At census, who is alive?
inCensus <-  Dat$dob <= Census & Dat$dod >= Census

# Calculate All 'time since onset, and TTD at census'
Dat$TimeSinceOnset <- Census - Dat$onset
# death in this case
Dat$TimeUntilExit <- Dat$dod - Census
# should all be strictly positive for this set

#all(Dat$onset > Dat$dob & Dat$onset < Dat$dod, na.rm=TRUE)
Target <- Dat[Dat$TimeSinceOnset >= 0 & Dat$TimeUntilExit >= 0 & Dat$trans, ]
head(Target)
nrow(Target)

# round down onset ages, etc.
Target$tso <- floor(Target$TimeSinceOnset)
Target$tte <- floor(Target$TimeUntilExit)

# tabulate:
TSO   <- table(Target$tso)
TTE   <- table(Target$tte)
times <- as.integer(names(TSO))
tso   <- as.integer(TSO)
tte   <- as.integer(TTE)

# and the symmetrical result
data.frame(times,tso,tte)