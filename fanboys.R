library(readxl)
#library(tidyverse)
library(ggplot2) ; library(tidyr) ; library(dplyr)
library(stringi) ; library(stringr) ; library(igraph)
library(ggraph) ; library(ggfortify) ; library(lubridate)

twtr <- read_excel('tweets.xlsx')

twtr$tweets <- gsub('\n', ' ', twtr$tweets, fixed = T)     ## gets rid of new lines

mentions <- twtr[grepl('.*@.*', twtr$tweets), ]            ## saves a new df that only contains mentions  

str_extract_all(mentions$tweets, '@\\w+') %>% head()      

extracts <- str_extract_all(mentions$tweets, '@\\w+')     ## extracts usernames
#View(extracts)


mat <- matrix(nrow = 9878, ncol = 11)
for (i in 1:9878) {                                     ## loads mentions from each tweet onto  row
  for (j in 1:11) {
    mat[i, j] <- ifelse(is.na(extracts[[i]][j]), 0, extracts[[i]][j]) 
  }
}


wideEdges <- cbind(user = mentions$username, data.frame(mat))  ## combines users with respective mentions as columns
gsub('@', '', wideEdges, fixed = T) %>% head()
for (i in 2:11) {
  wideEdges[, i] <- gsub('@', '', wideEdges[, i], fixed = T)
}

edges <- wideEdges %>% 
  gather(key = user, value = mentions, X1:X11) %>% 
  select(-user) %>% cbind(user = wideEdges$user) %>% select(user, X1 = mentions)
head(wideEdges)

uEdges <- filter(edges, X1 != '0')
edgesDF <- uEdges %>% group_by(user, X1) %>% summarise(men = n())  ## mentions of all time
id <- seq(1, length(nodes$user))
nodes <- data.frame(id, user = unique(edgesDF$user))
nodes$user <- as.character(nodes$user)    ## removing factors


edgesDF$user <- as.character(edgesDF$user)
irrDF <- edgesDF %>% left_join(nodes) %>%
  select(user, from = id, X1, men) %>%
  left_join(nodes, by = c('X1' = 'user')) %>% 
  filter(!is.na(id)) %>% ungroup(user) %>% 
  select(from, to = id, men)

twitG <- graph_from_data_frame(d = irrDF, vertices = nodes) #### the MAIN graph
plot(twitG)


#### Clean up here

############ Graph attributes
V(twitG)
E(twitG)
twitG$name <- 'Fanboys'
twitG$name
V(twitG)$id <- 1:vcount(twitG)
E(twitG)$weight <- irrDF$men

attributes(twitG)


infl <- eigen_centrality(twitG)
infl$vector %>% max()
infl$options

ggraph(twitG, layout = 'with_kk') +
  geom_edge_link(aes(alpha = weight)) +
  geom_node_point() +
  scale_fill_gradient()
#  geom_text(data = V(twitG)['Uncle_SamCoco'])

sort(degree(twitG), decreasing = T) %>% head(15) %>% enframe() %>% View()

top3 <- V(twitG) == c('Uncle_SamCoco', 'RamiAlLolah', 'WarReporter1')

str(twitG)
sort(betweenness(twitG, weights = irrDF$men), decreasing = T)
sc <- closeness(twitG)
igraph::centralize(sc)
closeness(twitG) %>% sort(decreasing = T) %>% head(15)

