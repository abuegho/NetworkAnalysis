library(foreach)
library(doParallel)
library(tidyverse)

set.seed(90)

InfoSimulator <- function(n, alpha, beta, steps = .05) {
	players <- 1:n
	for (i in players) {
		players[i] <- paste('player', i)
	}
	
	#alpha <- runif(1)  ### P() a player receives a signal
	#beta <- runif(1)   ### P() signal recieved is informative
	
	signal <- rbinom(n, 1, alpha)
	#signal
	
	index <- seq(0, 1, steps)
	
	z <- numeric(21)
	z[round(runif(1, 0, 20)) + 1] <- 1.2
	#z
	#index[z>0]
	p1AssetIndex <- ifelse(signal[1] == 0, 0,
						   ifelse(rbinom(1, 1, beta) == 1, index[z > 0],
						   	   index[round(runif(1, 0, 20) + 1)]))
	
	playersAssets <- numeric(n)
	playersAssets[1] <- p1AssetIndex      ### Rule 1
	vifelse <- Vectorize(ifelse)
	for (i in 2:n) {
		if (signal[i] == 1) {  ## e.g, sig' is .3
			sigPrime <- ifelse(rbinom(1, 1, beta) == 1, index[z > 0],
							   index[round(runif(1, 0, 20) + 1)])
			nonzeros <- playersAssets != 0
			a <- max(sigPrime == playersAssets)
			b <- max(table(playersAssets[vifelse(sum(nonzeros) == 0, c(1, rep(0, n - 1)), nonzeros)]))
			cond <- nonzeros & playersAssets < max(playersAssets)
			last <- length(unique(as.character(playersAssets[cond])))
			lastMax <- length(unique(as.character(playersAssets[nonzeros])))
			r3 <- max(sort(table(playersAssets[vifelse(sum(cond) == 0, c(1, rep(0, n - 1)), cond)])))
			ihat <- as.numeric(names(sort(table(playersAssets[cond]))[ifelse(last == 0, 1, last)]))
			r4 <- max(sort(table(playersAssets[vifelse(sum(nonzeros) == 0, c(1, rep(0, n - 1)), nonzeros)])))
			imax <- as.numeric(names(sort(table(playersAssets[nonzeros])))[lastMax])
			if (b < 2 | a == 1) {              ### Rule 2
				playersAssets[i] = sigPrime
			} else if (a == 0 & r3 > 1) {      ### Rule 3
				playersAssets[i] <- ihat
			} else if (a == 0 & r4 > 1) {      ### Rule 4
				playersAssets[i] <- imax
			}
		} else if (signal[i] == 0) {           ### Rule 5
			nonzeros <- playersAssets != 0
			cond <- nonzeros & playersAssets < max(playersAssets)
			last <- length(unique(as.character(playersAssets[cond])))
			lastMax <- length(unique(as.character(playersAssets[nonzeros])))
			r5a <- max(sort(table(playersAssets[vifelse(sum(cond) == 0, c(1, rep(0, n - 1)), cond)])))
			ihat <- as.numeric(names(sort(table(playersAssets[cond]))[ifelse(last == 0, 1, last)]))
			r5b <- max(sort(table(playersAssets[vifelse(sum(nonzeros) == 0, c(1, rep(0, n - 1)), nonzeros)])))
			imax <- as.numeric(names(sort(table(playersAssets[nonzeros]))[lastMax]))
			if (sum(playersAssets) == 0) {
				playersAssets[i] <- 0
			} else if (r5a > 1) {
				playersAssets[i] <- ihat
			} else {
				playersAssets[i] <- imax
			}
		}
	}
	
	return(data.frame(players, signal, playersAssets, iStar = index[z > 0]))
}


betaH <- .85
betaL <- .15

alphaH <- .85
alphaL <- .15
table(playersAssets)
cbind(index, z)

HH <- InfoSimulator(40, alphaH, betaH)
HH
table(HH$playersAssets)
max(table(HH$playersAssets, HH$iStar))
sims <- numeric(1000)

HH <- list()
for (i in 1:1000) {
	HH[i] <- InfoSimulator(40, alphaH, betaH)
	#sims[i] <- max(table(HH$playersAssets, HH$iStar))/40
}
sims
HH

hist(as.vector(ss))
plot(density(ss))
HHRes <- as.vector(ss)

HH <- list()

for (i in 1:3) {
	HH[[i]] <- InfoSimulator(40, alphaH, betaH)
}

table(HH[[1]]$signal, HH[[1]]$playersAssets, HH[[1]]$iStar)



################################################################################
HL <- list()
system.time(for (i in 1:1000) {
	HL[i] <- InfoSimulator(40, alphaH, betaL)
	#sims[i] <- max(table(HL$playersAssets, HL$iStar))/40
})
#HLRes <- sims

hist(HLRes)
plot(density(HLRes))

################################################################################
LH <- list()
for (i in 1:1000) {
	LH <- InfoSimulator(40, alphaL, betaH)
	sims[i] <- max(table(LH$playersAssets, LH$iStar))/40
}
hist(LHRes)
plot(density(LHRes))



################################################################################
LL <- list()
for (i in 1:1000) {
	LL <- InfoSimulator(40, alphaL, betaL)
	sims[i] <- max(table(LL$playersAssets, LL$iStar))/40
}
LLRes <- sims
hist(LLRes)
plot(density(LLRes))
LL
