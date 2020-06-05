### Replication of Banerjee's 1992 paper on Herd Behaviour

library(ggthemes)
library(tidyverse)

set.seed(90)

InfoSimulator <- function(n, alpha, beta, steps = .05) {
  #' Generates a simulation of how information (signal) spreads
  #' 
  #' There are five Decision rules
  #' 1) The first player follows his signal if he has one and chooses zero-th asset otherwise
  #' 2) The next players will follow their signals 
  #'   (a) if their signal at least one player has chosen an asset matching their signal
  #' 
  #' Inspired by Banerjee's 1992 paper on Modelling Herd Behaviour
  #' 
  #' @description This function takes a given number of players, a signal 
  #' prevalence, quality of the signal, and maps out how a signal disseminates
  #' through a population using simple decision rules. Signal describes 
  #' which asset amongst a set of assets carries excess returns. See Banerjee
  #' 1992 for more details.
  #' 
  #' 
  #' @param n number of players
  #' @param alpha probability a given player receives a signal (Prevalence)
  #' @param beta probability that a signal is informative (Quality);
  #' uniform across all players
  #' @param steps increment of the sequence (check details below)
  #' @usage InfoSimulator(50, alpha = 0.65, beta = runif(1), steps = 0.1)
  #' @return Dataframe containing players, whether they received a signal, which
  #' asset they ended up choosing (a number between [0, 1] based of steps argument),
  #' and iStart, which is the asset that had the excess returns
  #' @references Abhijit V. Banerjee. A simple model of herd behavior. The Quarterly Journal of
  #' Economics, 107(3):797–817, 1992.
  
	players <- 1:n
	for (i in players) {
		players[i] <- paste('player', i)
	}
	
	#alpha <- runif(1)  ### P() a player receives a signal
	#beta <- runif(1)   ### P() signal recieved is informative
	
	signal <- rbinom(n, 1, alpha)
	
	index <- seq(0, 1, steps)
	
	z <- numeric(21)
	z[rdunif(1, 1, 20) + 1] <- 1.2
	#z
	#index[z>0]
	p1AssetIndex <- ifelse(signal[1] == 0, 0,
						   ifelse(rbinom(1, 1, beta) == 1, index[z > 0],
						   	   index[rdunif(1, 0, 20) + 1]))
	
	playersAssets <- numeric(n)
	Signal_Prime <- numeric(n)
	playersAssets[1] <- p1AssetIndex      ### Rule 1
	vifelse <- Vectorize(ifelse)
	for (i in 2:n) {
		if (signal[i] == 1) {  ## e.g, sig' is .3
			sigPrime <- ifelse(rbinom(1, 1, beta) == 1, index[z > 0],
							   index[rdunif(1, 0, 20) + 1])
			Signal_Prime[i] <- sigPrime
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
	
	#optimal <- ifelse(as.numeric(index[z > 0]) == as.numeric(playersAssets), 1, 0)
	#committed <- ifelse(playersAssets == 0, 0, 1)
	
	return(data.frame(players, signal, AssetChosen = playersAssets, 
	                  iStar = index[z > 0], Signal_Prime))
}



betaH <- .85
betaL <- .15
betaM <- .5

alphaH <- .85
alphaL <- .15
alphaM <- .51
#table(playersAssets)
#cbind(index, z)

HH <- InfoSimulator(40, alphaL, betaH)
HH 
 # summarise(rate = ))
HH$optimal <- ifelse(HH$iStar == HH$AssetChosen, 1, 0)
HH$committed <- ifelse(HH$AssetChosen == 0, 0, 1)

table(HH$playersAssets)
max(table(HH$playersAssets, HH$iStar))
sims <- numeric(1000)

HH$match <- ifelse(HH$playersAssets == HH$iStar, 1, 0)

table(HH$match, HH$playersAssets)

HH <- list()
for (i in 1:3) {
	HH[[i]] <- InfoSimulator(40, alphaH, betaH)
	#sims[i] <- max(table(HH$playersAssets, HH$iStar))/40
}

agg2 <- NULL
for (i in 1:10000) {
  tmp <- InfoSimulator(40, alphaH, betaH)
  tmp$optimal <- ifelse(tmp$iStar == tmp$AssetChosen, 1, 0)
  tmp$committed <- ifelse(tmp$AssetChosen == 0, 0, 1)
  agg <- tmp %>% 
    summarise(commit_Rate = mean(committed), optimal_rate = mean(optimal),
              sig_prevalence = mean(signal))
  agg2 <- rbind(agg2, agg)
}

aggHH <- NULL
for (i in 1:2000) {
  tmp <- InfoSimulator(40, alphaH, betaH)
  agg <- tmp %>% 
    mutate(Influenced = if_else(sum(signal[1:2]) == 0, 'Leaderless Eq', 'Leader Eq.'),
           optimal = if_else(iStar == AssetChosen, 1, 0),
           committed = if_else(AssetChosen == 0, 0, 1)) %>% 
    group_by(Influenced) %>% 
    summarise(commit_Rate = mean(committed), optimal_rate = mean(optimal),
              sig_prevalence = mean(signal),
              Herd_Index = sum(signal * (Signal_Prime != AssetChosen)) / sum(signal))
  aggHH <- rbind(aggHH, agg)
}


ggplot(aggHH) +
  geom_density(aes(sig_prevalence, fill = Herd_Index)) +
  geom_density(aes(optimal_rate, fill = Influenced, alpha = .6)) +
  geom_density(aes(Herd_Index))
  #geom_density(aes(sig_prevalence, fill = 'signal', alpha = .6)) +
  #geom_density(aes(commit_Rate))
ggplot(aggHH, aes(Herd_Index, fill = Influenced, alpha = .4)) + geom_density()
# InfoSimulator(40, alphaL, betaL) %>% 
#   mutate(Influenced = if_else(sum(signal[1:2]) == 0, 'No', 'Yes'),
#          optimal = if_else(iStar == AssetChosen, 1, 0),
#          committed = AssetChosen == 0, 0, 1) %>% 
#   group_by(Influenced) %>% 
#   summarise(commit_Rate = mean(committed), optimal_rate = mean(optimal),
#             sig_prevalence = mean(signal))

################################################################################
aggHL <- NULL
system.time(for (i in 1:2000) {
  tmp <- InfoSimulator(40, alphaH, betaL)
  agg <- tmp %>% 
    mutate(Influenced = if_else(sum(signal[1:2]) == 0, 'No', 'Yes'),
           optimal = if_else(iStar == AssetChosen, 1, 0),
           committed = if_else(AssetChosen == 0, 0, 1),
           defied = Signal_Prime != AssetChosen) %>% 
    group_by(Influenced) %>% 
    summarise(commit_Rate = mean(committed), optimal_rate = mean(optimal),
              sig_prevalence = mean(signal),
              Herd_Index = sum(signal * defied) / sum(signal))
  aggHL <- rbind(aggHL, agg)
})
#HLRes <- sims

hist(HLRes)
plot(density(HLRes))

ggplot(aggHL, aes(sig_prevalence, fill = 'Signal')) +
  geom_density() +
  geom_density(aes(optimal_rate, fill = Influenced, alpha = .6)) +
  geom_density(aes(sig_prevalence, fill = 'Signal', alpha = .6)) +
  geom_density(aes(commit_Rate))

################################################################################
agg <- NULL
system.time(for (i in 1:2000) {
  tmp <- InfoSimulator(40, alphaL, betaH)
  agg1 <- tmp %>% 
    mutate(Influenced = if_else(sum(signal[1:2]) == 0, 'No', 'Yes'),
           run = paste('run', i))
  agg <- rbind(agg, agg1)
})
LHagg <- agg
aggLH <- LHagg %>% 
  mutate(optimal = if_else(iStar == AssetChosen, 1, 0),
                        committed = if_else(AssetChosen == 0, 0, 1),
                        defied = Signal_Prime != AssetChosen) %>% 
  group_by(run, Influenced) %>% 
  summarise(commit_Rate = mean(committed), optimal_rate = mean(optimal),
              sig_prevalence = mean(signal),
              Herd_Index = sum(signal * defied) / sum(signal)) %>% 
  ungroup() %>% select(-run)
    

aggLH <- aggLH %>% select(-run)
system.time(for (i in 1:2000) {
  tmp <- InfoSimulator(40, alphaL, betaH)
  agg <- tmp %>% 
    mutate(Influenced = if_else(sum(signal[1:2]) == 0, 'No', 'Yes'),
           optimal = if_else(iStar == AssetChosen, 1, 0),
           committed = if_else(AssetChosen == 0, 0, 1),
           defied = Signal_Prime != AssetChosen) %>% 
    group_by(Influenced) %>% 
    summarise(commit_Rate = mean(committed), optimal_rate = mean(optimal),
              sig_prevalence = mean(signal),
              Herd_Index = sum(signal * defied) / sum(signal))
  aggLH <- rbind(aggLH, agg)
})
hist(LHRes)
plot(density(LHRes))

ggplot(aggLH, aes(sig_prevalence, fill = 'Signal')) +
  geom_density() +
  geom_density(aes(optimal_rate, fill = Influenced, alpha = .6)) +
  geom_density(aes(sig_prevalence, fill = Influenced, alpha = .6)) +
  geom_density(aes(commit_Rate))


################################################################################
LLagg <- NULL
system.time(for (i in 1:2000) {
  tmp <- InfoSimulator(40, alphaL, betaL)
  agg1 <- tmp %>% 
    mutate(Influenced = if_else(sum(signal[1:2]) == 0, 'No', 'Yes'),
           run = paste('run', i))
  LLagg <- rbind(LLagg, agg1)
})

aggLL <- LLagg %>% 
  mutate(optimal = if_else(iStar == AssetChosen, 1, 0),
         committed = if_else(AssetChosen == 0, 0, 1),
         defied = Signal_Prime != AssetChosen) %>% 
  group_by(run, Influenced) %>% 
  summarise(commit_Rate = mean(committed), optimal_rate = mean(optimal),
            sig_prevalence = mean(signal),
            Herd_Index = sum(signal * defied) / sum(signal)) %>% 
  ungroup() %>% select(-run)

aggLL <- NULL
for (i in 1:2000) {
	tmp <- InfoSimulator(40, alphaL, betaL)
	agg <- tmp %>% 
	  mutate(Influenced = if_else(sum(signal[1:2]) == 0, 'No', 'Yes'),
	         optimal = if_else(iStar == AssetChosen, 1, 0),
	         committed = if_else(AssetChosen == 0, 0, 1),
	         defied = Signal_Prime != AssetChosen) %>% 
	  group_by(Influenced) %>% 
	  summarise(commit_Rate = mean(committed), optimal_rate = mean(optimal),
	            sig_prevalence = mean(signal),
	            Herd_Index = sum(signal * defied) / sum(signal))
	aggLL <- rbind(aggLL, agg)
}

ggplot(aggLL, aes(sig_prevalence, fill = 'Signal')) +
  geom_density() +
  geom_density(aes(optimal_rate, fill = Influenced, alpha = .6)) +
  #geom_density(aes(sig_prevalence, fill = 'signal', alpha = .6)) +
  geom_density(aes(commit_Rate))

LLRes <- sims
hist(LLRes)
plot(density(LLRes))
LL
#################################################################################################
MMagg <- NULL
system.time(for (i in 1:2000) {
  tmp <- InfoSimulator(40, alphaM, betaM)
  agg1 <- tmp %>% 
    mutate(Influenced = if_else(sum(signal[1:2]) == 0, 'No', 'Yes'),
           run = paste('run', i))
  MMagg <- rbind(MMagg, agg1)
})

aggMM <- MMagg %>% 
  mutate(optimal = if_else(iStar == AssetChosen, 1, 0),
         committed = if_else(AssetChosen == 0, 0, 1),
         defied = Signal_Prime != AssetChosen) %>% 
  group_by(run, Influenced) %>% 
  summarise(commit_Rate = mean(committed), optimal_rate = mean(optimal),
            sig_prevalence = mean(signal),
            Herd_Index = sum(signal * defied) / sum(signal)) %>% 
  ungroup() %>% select(-run)


aggMM <- NULL
for (i in 1:2000) {
  tmp <- InfoSimulator(40, alphaM, betaM)
  agg <- tmp %>% 
    mutate(Influenced = if_else(sum(signal[1:2]) == 0, 'No', 'Yes'),
           optimal = if_else(iStar == AssetChosen, 1, 0),
           committed = if_else(AssetChosen == 0, 0, 1),
           defied = Signal_Prime != AssetChosen) %>% 
    group_by(Influenced) %>% 
    summarise(commit_Rate = mean(committed), optimal_rate = mean(optimal),
              sig_prevalence = mean(signal),
              Herd_Index = sum(signal * defied) / sum(signal))
  aggMM <- rbind(aggMM, agg)
}

p2 <- ggplot(aggMM, aes(sig_prevalence)) +
  geom_density() +
  geom_density(aes(optimal_rate, fill = 'optimal', alpha = .6))

aggHH$Influenced <- ifelse(aggHH$Influenced == 'Leaderless Eq','No' , 'Yes')
aggHH$Combo <- 'High Prevalence/High Quality'
aggHL$Combo <- 'High Prevalence/Low Quality'
aggLH$Combo <- 'Low Prevalence/ High Quality'
aggLL$Combo <- 'Low Prevalence/ Low Quality'
aggMM$Combo <- 'Medium Prevalence/ Medium Quality'

df <- rbind(aggHH, aggHL, aggLH, aggLL, aggMM)
p <- ggplot(df) + 
  geom_density(aes(optimal_rate, fill = Influenced, alpha = .5)) +
  geom_density(aes(Herd_Index), col = 'violetred2', size = 1) +
  facet_wrap(. ~ Combo, nrow = 5, scales = 'free') +
  theme_base()

