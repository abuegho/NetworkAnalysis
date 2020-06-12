library(readxl)
library(tidyverse)
library(stringi)
library(igraph)
library(ggraph)

twtr <- read_excel('tweets.xlsx')

twtr$tweets <- gsub('\n', ' ', twtr$tweets, fixed = T)

mentions <- twtr[grepl('.*@.*', twtr$tweets), ]

str_extract_all(mentions$tweets, '@\\w+') %>% head()

extracts <- str_extract_all(mentions$tweets, '@\\w+')
View(extracts)


mat <- matrix(nrow = 9878, ncol = 11)
for (i in 1:9878) {
  for (j in 1:11) {
    mat[i, j] <- ifelse(is.na(extracts[[i]][j]), 0, extracts[[i]][j])
  }
}


wideEdges <- cbind(user = mentions$username, data.frame(mat))
gsub('@', '', wideEdges, fixed = T) %>% head()
for (i in 2:11) {
  wideEdges[, i] <- gsub('@', '', wideEdges[, i], fixed = T)
}

wideEdges %>% gather(key = user, value = mentions,) %>% head(13)
head(wideEdges)
fir <- wideEdges[, c(1, 2)]
sec <- wideEdges[, c(1, 3)]
thi <- wideEdges[, c(1, 4)]
fou <- wideEdges[, c(1, 5)]
fic <- wideEdges[, c(1, 6)]
six <- wideEdges[, c(1, 7)]
sev <- wideEdges[, c(1, 8)]
eig <- wideEdges[, c(1, 9)]
nin <- wideEdges[, c(1, 10)]
ten <- wideEdges[, c(1, 11)]
ele <- wideEdges[, c(1, 12)]
colnames(ele)[2] <- 'X1'
edges <- bind_rows(fir, sec, thi, fou, fic, six, sev, eig, nin, ten, ele)
uEdges <- filter(edges, X1 != '0')
edgesDF <- uEdges %>% group_by(user, X1) %>% summarise(men = n())
nodes <- data.frame(user = unique(edgesDF$user))
nodes$user <- as.character(nodes$user)
edgesDF$user <- as.character(edgesDF$user)
twitG <- graph_from_data_frame(d = edgesDF)
twoWays <- edgesDF %>% semi_join(nodes, by = c('X1' = 'user'))
edgesDF %>% semi_join(nodes)
nodes %>% anti_join(twoWays)
plot(twitG)
siG3 <- graph_from_data_frame(d = filter(edgesDF, men > 2))
plot(siG3)
V(siG3)
E(siG3)

nodes <- nodes %>% select(-id)
#dfEdges <- left_join(edgesDF, nodes) %>% select(id, user, shout = X1, men)
twitG <- graph_from_data_frame(d = edgesDF, directed = T)

V(twitG)
E(twitG)
twitG$name <- 'Fanboys'
twitG$name
V(twitG)$id <- 1:vcount(twitG)
E(twitG)$weight <- edgesDF$men
ggraph::autograph(twitG)
plot(twitG)
nodes
twoWays <- edgesDF %>% semi_join(nodes) %>% semi_join(nodes, by = c('X1' = 'user'))
tGs <- graph_from_data_frame(twoWays, vertices = nodes, directed = T)
tGs$name <- 'Fanboys'
tGs$name
V(tGs)$id <- 1:vcount(tGs)
E(tGs)$weight <- twoWays$men
ggraph::autograph(tGs)
plot(tGs)


ggraph(twitG, layout = 'with_kk') +
  geom_edge_link(aes(alpha = weight)) +
  geom_node_point() +
  scale_color_grey()
#  geom_text(data = V(twitG)['Uncle_SamCoco'])

sort(degree(twitG), decreasing = T) %>% head(15) %>% enframe() %>% View()

top3 <- V(twitG) == c('Uncle_SamCoco', 'RamiAlLolah', 'WarReporter1')


'Uncle_SamCoco     RamiAlLolah    WarReporter1      mobi_ayubi   MaghrabiArabi        warrnews    _IshfaqAhmad   NaseemAhmed50      wayf44rerr 
deg
3335 3334 3333 3332 3331 3330 3329 3328 3327 3326 3325 3324 3323 3322'

str(twitG)
sort(betweenness(twitG, weights = edgesDF$men), decreasing = T)
sc <- closeness(twitG)
igraph::centralize(sc)
closeness(twitG) %>% sort(decreasing = T) %>% head(15)
geomed
