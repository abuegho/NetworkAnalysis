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

wideEdges <- NULL
wideEdges <- cbind(user = mentions$username, data.frame(mat))  ## combines users with respective mentions as columns
#gsub('@', '', wideEdges, fixed = T) %>% head()
for (i in 2:11) {
  wideEdges[, i] <- gsub('@', '', wideEdges[, i], fixed = T)
}
wideEdges <- cbind(wideEdges, followers = mentions$followers)
edges <- melt(wideEdges,id.vars=c("user", 'followers'),measured.vars=c(2:12))[-3]
names(edges)[3] <- 'X1'
# edges <- wideEdges %>% 
#   gather(key = user, value = mentions, X1:X11) %>% 
#   select(-user) %>% cbind(user = wideEdges$user) %>% select(user, X1 = mentions)
# head(wideEdges)


uEdges <- filter(edges, X1 != '0')
edgesDF <- uEdges %>% group_by(user, X1) %>% 
  summarise(men = n(), fol = median(followers))  ## mentions of all time
id <- seq(1, length(unique(edgesDF$user)))
nodes <- data.frame(id, user = unique(edgesDF$user))
nodes$user <- as.character(nodes$user)    ## removing factors
nodes <- nodes[-c(9, 4, 96:107),]

edgesDF$user <- as.character(edgesDF$user)
irrDF <- edgesDF %>% left_join(nodes) %>%
  select(user, from = id, X1, men, fol) %>%
  left_join(nodes, by = c('X1' = 'user')) %>% 
  filter(!is.na(id)) %>% ungroup(user) %>% 
  select(from, to = id, men, fol)

ranker <- irrDF %>% select(1, 4) %>% distinct() %>% arrange(fol)  ## will be used to generate ranks
ranker$r <- 1:nrow(ranker)
ranker$fol <- as.integer(ranker$fol)

irrDF <- irrDF %>% left_join(ranker)
irrDF <- irrDF %>% 
  mutate(r = ifelse(is.na(r), lag(r), r)) %>% 
  mutate(wgt = log(r * men)) %>% 
  select(from, to, wgt, r, fol, men)
irrDF <- irrDF[!is.na(irrDF$from), ]
tG <- graph_from_data_frame(d = irrDF, vertices = nodes) #### the MAIN graph
plot(tG)

### Clean up here

############ Graph attributes
V(tG)
E(tG)
tG$name <- 'Fanboys'
tG$name
V(tG)$id <- 1:vcount(tG)
E(tG)$weight <- irrDF$wgt

inv_wgt <- 1 / E(tG)$weight
#broker <- edge_betweenness(twitG, weights = inv_wgt)

infl <- eigen_centrality(tG)
infl$vector %>% max()
infl$options

ggraph(tG, layout = 'with_kk') +
  geom_edge_link(aes(alpha = weight)) +
  geom_node_point() + labs(title = 'gg') +
  geom_node_text(aes(label = id), repel = T)
  #scale_color_continuous(low = "#132B43", high = "#56B1F7")
#  geom_text(data = V(twitG)['Uncle_SamCoco'])
c('randomly', 'on_sphere', "with_sugiyama", "with_dh")
#E(tG)$weight <- irrDF[-del,]$wgt

#sort(degree(twitG), decreasing = T) %>% head(15) %>% enframe() %>% View()

#top3 <- V(twitG) == c('Uncle_SamCoco', 'RamiAlLolah', 'WarReporter1')

str(tG)
sort(betweenness(tG, weights = irrDF$men), decreasing = T)
sc <- closeness(tG)
igraph::centralize(sc)
closeness(tG) %>% sort(decreasing = T) %>% head(15)

# degree(twitG)
# strength(twitG)
#clean = irrDF[-c(9, 4, 96:107), ]
#del <- nodes_augmented[96:107, ]$id
#tG <- graph_from_data_frame(d = irrDF[-c(del,9, 4), ], vertices = nodes[-c(del,9, 4), ]) #### the MAIN graph

#E(tG)$weight <- clean$wgt/max(clean$wgt)      #irrDF[-c(del,9, 4),]$wgt
inv_t <- 1 / E(tG)$weight

E(tG)$weight
nodes_augmented <- nodes %>% 
  mutate(deg = degree(tG), Strength = strength(tG), 
         bwn = betweenness(tG, weights = inv_t)) %>%   #### consider weight carefully
  arrange(id)
`Edge Betweenness` <- edge_betweenness(tG, weights = inv_t) 
tG %>% 
  ggraph(layout = 'with_sugiyama') +
  geom_edge_link(aes(alpha = `Edge Betweenness` )) + #alpha = irrDF[-c(del,9, 4),]$fol, 
  geom_node_point(aes(size = betweenness(tG, weights = inv_t),
                      col = strength(tG))) +
  geom_node_text(aes(label = id), repel = T) +
  scale_colour_viridis_c() + theme_classic() +
  labs(size = 'Betweenness', col = 'Strength')
eigenV <- eigen_centrality(tG)$vector
nodes %>% 
  mutate(deg = degree(tG)/224, Strength = strength(tG), 
         bwn = betweenness(tG, weights = inv_t)) %>% cbind(eigenV) %>% 
  arrange(desc(bwn), desc(Strength), desc(deg)) %>% head(8) %>% stargazer::stargazer(summary = F)



irrDF %>% mutate(edge = edge_betweenness(tG, weights = inv_t)) %>%   ## clean is th edges df
  arrange(desc(edge)) %>% 
  left_join(nodes, by = c('from' = 'id')) %>% 
  left_join(nodes, by = c('to' = 'id')) %>% arrange(desc(fol)) %>% 
  count(ceiling(wgt)) %>% 
  mutate(perc = 100 * n/nrow(irrDF)) %>% View()                       #% of ties


attributes(V(tG))
`Strong Tie` <- irrDF$wgt > (median(irrDF$wgt) + 2)
tG %>% 
  ggraph(layout = 'randomly') +
  geom_edge_link(aes(alpha = `Edge Betweenness`, 
                     filter = `Edge Betweenness` > median(`Edge Betweenness`),
                     col = `Strong Tie`)) + #alpha = irrDF[-c(del,9, 4),]$fol, 
  geom_node_point(aes(size = degree(tG),
                      col = strength(tG))) +
  geom_node_text(aes(label = id), repel = T) +
  scale_colour_viridis_c() + theme_classic() +
  labs(size = 'Degree', col = 'Strength')
reversed <- clean %>% mutate(temp = to, to = from, from = temp) %>% 
  select(-temp) %>% bind_rows(clean)
as.matrix(eigenV, nrow = 1)

W <- as_adjacency_matrix(tG)
#W^900 %*% as.matrix(nodes_augmented$bwn, nrow = 1)
for (i in 1:93) {
  W[i, W[i, ]> 0] <- exp(W[i, W[i, ]> 0])/sum(exp(W[i,W[i, ]> 0 ]))  ## Softmax!!!
}

X <- matrix(ifelse(nodes_augmented$Strength > median(nodes_augmented$Strength), 1, 0)
            , nrow = 93, ncol = 1)
W^30 %*% X
COMPLETE <- clean %>% inner_join(clean, by = c('from' = 'to', 'to' = 'from'))
g <- graph_from_data_frame(COMPLETE[, 1:3], vertices = nodes[unique(COMPLETE$from),])


quickplot(betweenness(tG, weights = inv_t)) + geom_point()

ggplot(irrDF, aes(from, to, fill = factor(ceiling(wgt)))) +
  geom_raster()
ggplot(data.frame(id = 1:93, bns), aes(x = id, y = bns)) +
   ylab('Betweenness Centrality') + theme_bw()+
  geom_text(aes(label = nodes$user))

bns <- betweenness(tG, weights = inv_t)
# for (i in x[-1]) {
#   try(p[i] <- print(ggraph(tG, layout = i) +
#         geom_edge_link(aes(alpha = irrDF[-c(del,9, 4),]$fol)) +
#         geom_node_point(aes(size = betweenness(tG, weights = inv_t),
#                             col = strength(tG))) +
#         geom_node_text(aes(label = id), repel = T) +
#         scale_colour_viridis_c() + theme_classic() +
#         labs(title = i)))
# }
X2 <- matrix(numeric(93^2), nrow = 93, ncol = 93) 
for (i in 1:93) {
  X2[i, ] <- W[i, ]
}
for (i in 1:93) {
  for (j in 1:93) {
    if (W[i, j] == 0) {
      X2[i, j] <- NA
    }
  }
}

na.omit(X2)
X2
W <- as.matrix(W)
cov(W)

diag(S) <- 0
h <- graph_from_adjacency_matrix(S, mode = "undirected", weighted = TRUE)
sim_df <- igraph::as_data_frame(h, what = "edges")

# Convert sim_df to a tibble
sim_tib <- as_tibble(sim_df)
sim_tib$from <- as.integer(sim_tib$from)
sim_tib$to <- as.integer(sim_tib$to)

sim_joined <- sim_tib %>% 
  # Left join to nodes matching "from" to "id"
  left_join(nodes_augmented,by = c("from" = "id")) %>% 
  # Left join to nodes matching "to" to "id", setting suffixes
  left_join(nodes_augmented, by = c('to' = 'id'), suffix = c('_from', '_to')) 
range(sim_joined$deg_from)
sim_joined %>% arrange(weight) %>%
  filter(deg_from >= 10 & deg_to >= 10)
sim_filteredneg <- sim_joined %>% 
  # Filter on similarity greater than 0.6
  filter(weight < -.1)

# Convert to an undirected graph
filtered_network <- graph_from_data_frame(sim_filtered, directed = F)

# Plot with Kamada-Kawai layout
ggraph(filtered_network, layout = 'with_kk') + 
  # Add an edge link geom, mapping transparency to similarity
  geom_edge_link(aes(alpha = weight))
W
S <- cov(W)

D <- 1 - S
D
d <- as.dist(D)
cc <- hclust(d, method = 'complete')
plot(cc)
cc$merge[1, ]


cls <- cutree(cc, k = 5)
table(cls)
km <- kmeans(d, 5)
table(km$cluster)
cluster <- factor(km$cluster)

tG %>% 
  ggraph(layout = 'randomly') +
  geom_edge_link(aes(alpha = `Edge Betweenness`, 
                     filter = `Edge Betweenness` > median(`Edge Betweenness`),
                     col = `Strong Tie`)) + #alpha = irrDF[-c(del,9, 4),]$fol, 
  geom_node_point(aes(size = degree(tG),
                      col = factor(cls))) +
  geom_node_text(aes(label = id), repel = T) + theme_classic() +
  labs(size = 'Degree')
nodes_augmented$cls <- cls
nodes_augmented$eig <- eigenV
filter(nodes_augmented, cls == 3)
E(tG)$weight
V(tG)
nodes_augmented
library(visNetwork)

data <- toVisNetworkData(tG)
visNetwork(nodes = data$nodes,
           edges = data$edges) %>% 
  visOptions(highlightNearest = T, nodesIdSelection = T)
test <- nodes_augmented %>% left_join(irrDF, by = c('id' = 'from')) %>% filter(!is.na(fol))
mod <- randomForest(test[, 5:7], y = test$fol)
summary(mod)
mod
plot(mod)
hist(predict(mod))
hist(test$fol)
varImpPlot(mod)

sort(eigenV, decreasing = T)
