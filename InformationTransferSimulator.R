### Replication of Banerjee's 1992 paper on Herd Behaviour
library(ggthemes)
library(ggplot2)
library(purrr)
library(infer)

#load('NA.RData')

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
	
	signal <- rbinom(n, 1, alpha)   # P(player receives a signal) = alpha
	
	index <- seq(0, 1, steps)       # index of assets to pick from
	
	z <- numeric(21)
	z[sample(1:20, 1) + 1] <- 1.2  # assigning profitable asset at random, to index i*
	#z
	#index[z>0]
	p1AssetIndex <- ifelse(signal[1] == 0, 0,                  ## First player makes a move
						   ifelse(rbinom(1, 1, beta) == 1, index[z > 0], ## Given a signal, he'll pick i* with probability beta
						   	   index[sample(0:20, 1) + 1]))             ## Otherwise, he'll pick any stock with probability 1 - beta
	
	playersAssets <- numeric(n)
	Signal_Prime <- numeric(n)
	playersAssets[1] <- p1AssetIndex      ### Rule 1
	vifelse <- Vectorize(ifelse)
	for (i in 2:n) {
		if (signal[i] == 1) {  ## e.g, sig' is .3
			sigPrime <- ifelse(rbinom(1, 1, beta) == 1, index[z > 0],  ## signal player receives about which asset to pick
							   index[sample(1:20, 1) + 1])
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

# HH <- InfoSimulator(40, alphaL, betaH)
# HH 
#  # summarise(rate = ))
# HH$optimal <- ifelse(HH$iStar == HH$AssetChosen, 1, 0)
# HH$committed <- ifelse(HH$AssetChosen == 0, 0, 1)
# 
# HH$match <- ifelse(HH$playersAssets == HH$iStar, 1, 0)
# 
# table(HH$match, HH$playersAssets)


HHagg <- NULL
system.time(for (i in 1:2000) {
  tmp <- InfoSimulator(40, alphaH, betaH)
  agg1 <- tmp %>% 
    mutate(Influenced = if_else(sum(signal[1:2]) == 0, 'No', 'Yes'),
           run = paste('run', i))
  HHagg <- rbind(HHagg, agg1)
})

aggHH <- HHagg %>% 
  mutate(optimal = if_else(iStar == AssetChosen, 1, 0),
         committed = if_else(AssetChosen == 0, 0, 1),
         defied = Signal_Prime != AssetChosen) %>% 
  group_by(run, Influenced) %>% 
  summarise(commit_Rate = mean(committed), optimal_rate = mean(optimal),
            sig_prevalence = mean(signal),
            Herd_Index = sum(signal * defied) / sum(signal)) %>% 
  ungroup() %>% select(-run)


################################################################################
agg <- NULL
system.time(for (i in 1:2000) {
  tmp <- InfoSimulator(40, alphaH, betaL)
  agg1 <- tmp %>% 
    mutate(Influenced = if_else(sum(signal[1:2]) == 0, 'No', 'Yes'),
           run = paste('run', i))
  agg <- rbind(agg, agg1)
})
HLagg <- agg
aggHL <- HLagg %>% 
  mutate(optimal = if_else(iStar == AssetChosen, 1, 0),
         committed = if_else(AssetChosen == 0, 0, 1),
         defied = Signal_Prime != AssetChosen) %>% 
  group_by(run, Influenced) %>% 
  summarise(commit_Rate = mean(committed), optimal_rate = mean(optimal),
            sig_prevalence = mean(signal),
            Herd_Index = sum(signal * defied) / sum(signal)) %>% 
  ungroup() %>% select(-run)

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


########################################################################

p2 <- ggplot(aggMM, aes(sig_prevalence)) +
  geom_density() +
  geom_density(aes(optimal_rate, fill = 'optimal', alpha = .6))

aggHH$Influenced <- ifelse(aggHH$Influenced == 'Leaderless Eq','No' , 'Yes')
aggHH$Signal_Type <- 'High Prevalence/High Quality'
aggHL$Signal_Type <- 'High Prevalence/Low Quality'
aggLH$Signal_Type <- 'Low Prevalence/ High Quality'
aggLL$Signal_Type <- 'Low Prevalence/ Low Quality'
aggMM$Signal_Type <- 'Medium Prevalence/ Medium Quality'

df <- rbind(aggHH, aggHL, aggLH, aggLL, aggMM)
#names(df)[6] <- 'Signal Type'

p <- ggplot(df) + 
  geom_density(aes(optimal_rate, fill = Influenced, alpha = .5)) +
  geom_density(aes(Herd_Index), col = 'violetred2', size = 1) +
  facet_wrap(. ~ `Signal Type`, nrow = 5, scales = 'free') +
  theme_tufte()

ggplot(df, aes(Herd_Index, fill = `Signal Type`)) +
  geom_density(position = 'stack', alpha = .5) +
  scale_fill_brewer(palette = 'Set2') +
  labs(title = 'Herding By Signal Type') +
  theme_pander() +
  theme(plot.title = element_text(hjust = .5, face = 'bold'),
        plot.subtitle = element_text(hjust = .5, face = 'italic'), 
        legend.position = 'bottom',
        legend.title.align = .5) +
  xlab('Herding Index')
  #theme(panel.background = element_rect(fill = 'white'), plot.background = element_blank())
  #ggsave('stacked.png', type = 'cairo', width = 9, height = 5.4)
ggplot(df, aes(optimal_rate)) +
  stat_density(aes(col = `Signal Type`), geom = 'line')
p + scale_color_tableau()

ggplot(rbind(aggLH, aggLL), aes(Herd_Index, fill = Combo)) +
  geom_density(alpha = .4, position = 'identity')

t1 <- t.test(optimal_rate ~ Influenced, data = aggHH)
t2 <- t.test(optimal_rate ~ Influenced, data = aggHL)
t3 <- t.test(optimal_rate ~ Influenced, data = aggLH)
t4 <- t.test(optimal_rate ~ Influenced, data = aggLL)
ggplot(df, aes(optimal_rate, fill = Signal_Type, alpha = .9)) +
  geom_density(position = 'stack') +
  scale_fill_brewer(palette = 'Set2') +
  #scale_fill_pander() +
  theme_pander()
df %>% group_by(Signal_Type) %>% filter(optimal_rate == 1) %>% 
  count()

ggplot(rbind(aggHH, aggHL), aes(optimal_rate, fill = Combo, alpha = .4)) +
  geom_density(position = 'stack') +
  facet_wrap(. ~ Influenced)

ggplot(rbind(aggHH, aggHL), aes(Herd_Index, fill = Combo, alpha = .4)) +
  geom_density() +
  facet_wrap(. ~ Influenced)

??leveneTest
fligner.test(Herd_Index ~ Influenced, data = df)
bartlett.test(Herd_Index ~ `Signal Type`, data = df)
t.test(optimal_rate ~ Influenced, data = df)
ggplot(df, aes(optimal_rate)) +
  geom_density(position = 'stack', aes(fill = Influenced), 
               col = 'grey', alpha = .6) +
  stat_density(aes(sig_prevalence, col = 'Signal Prevalence'), geom = 'line') +
  scale_colour_manual(values = c('Signal Prevalence' = 'black')) +
  facet_wrap(. ~ `Signal Type`, nrow = 1) + 
  labs(col = '', x = 'Optimal Rate', 
       title = 'Distrbution of Optimal Rate Across Signal Types') +
  theme_tufte() +
  theme(plot.title = element_text(hjust = .5, face = 'bold'))#,
       # plot.subtitle = element_text(hjust = .5, face = 'italic'))
DescTools::LeveneTest()

library(stargazer)
stargazer(tav, summary = F, style = 'jpam')

df %>% group_by(Signal_Type, Influenced) %>% 
  summarise_all(mean, na.rm = T) %>% select(optimal_rate, Herd_Index) %>% arrange(desc(Influenced))

write.csv(df %>% group_by(Influenced, `Signal Type`) %>% 
            summarise_all(mean, na.rm = T) %>% 
            select(-commit_Rate), 'tab.csv')
stargazer(read.csv('tab.csv'), summary = F)
read.csv('tab.csv') %>% View()

aggHH %>% group_by(Influenced) %>% summarise(m = mean(1 - optimal_rate))
LHagg %>% group_by(run) %>% filter(Influenced == 'Yes' & Signal_Prime == 0 & AssetChosen != Signal_Prime
                                   & signal == 1 & AssetChosen != iStar) %>% View()

aggLH %>% filter(Herd_Index == 1)

dftest %>% ggplot(aes(Herd_Index, fill = Influenced)) +
  geom_density(alpha = .5) +
  facet_wrap(Combo ~ .) +
  labs(title = 'Comparison of Low Prevalence Outcomes', 
       subtitle = 'Simulation with two different seeds') +
  theme_tufte() +
  theme(plot.title = element_text(hjust = .5, face = 'bold'),
        plot.subtitle = element_text(hjust = .5, face = 'italic')) +
  xlab('Herding Index')

ks.test(aggHH$Herd_Index, aggHL$Herd_Index)
for (i in unique(df$`Signal Type`)) {
  x <- ks.test(df$optimal_rate[df$`Signal Type` == i], df$optimal_rate[df$`Signal Type` == 'Low Prevalence/ High Quality'])
  print(x)
}

stargazer(t1, t2, t3, t4)
stargazer(t1)

t1 <- t_test(data = aggHH, Herd_Index ~ Influenced, order = c('Yes', 'No'))
t2 <- t_test(aggHL, Herd_Index ~ Influenced, order = c('Yes', 'No'))
t3 <- t_test(aggMM, Herd_Index ~ Influenced, order = c('Yes', 'No'))
t4 <- t_test(aggLH, Herd_Index ~ Influenced, order = c('Yes', 'No'))
t5 <- t_test(aggLL, Herd_Index ~ Influenced, order = c('Yes', 'No'))

stargazer(bind_rows(t1, t2, t3, t4, t5), summary = F)

ggplot(rbind(aggLH, aggLL, aggMM), aes(Herd_Index)) +
  geom_density(position = 'identity', aes(fill = Influenced), 
               col = 'grey', alpha = .6) +
  stat_density(aes(sig_prevalence, col = 'Signal Prevalence'), geom = 'line') +
  scale_colour_manual(values = c('Signal Prevalence' = 'black')) +
  facet_wrap(. ~ Signal_Type, nrow = 1) + 
  labs(col = '', x = 'Herding Rate') + #, 
       #title = 'Distrbution of Herding Across Signal Types') +
  theme_tufte() +
  #theme(plot.title = element_text(hjust = .5, face = 'bold')) +
  scale_fill_brewer(palette = 'Accent')
#
lv1 <- LeveneTest(optimal_rate ~ Influenced, data = aggHH)
lv2 <- LeveneTest(optimal_rate ~ Influenced, data = aggHL)
lv5 <- LeveneTest(optimal_rate ~ Influenced, data = aggMM)
lv3 <- LeveneTest(optimal_rate ~ Influenced, data = aggLH)
lv4 <- LeveneTest(optimal_rate ~ Influenced, data = aggLL)
stargazer::stargazer(lv1, lv2, lv5, lv3, lv4)
