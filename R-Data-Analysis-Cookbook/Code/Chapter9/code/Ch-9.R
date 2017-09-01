#1. Downloading social network data using public APIs
# From www.meetup.com download groups data into groups.json

library(jsonlite)
g <- fromJSON("groups.json")
groups <- g$results
head(groups)

source("rdacb.getusers.R")
#Add your apikey within quotes in the command below before executing

members <- rdacb.getusers(groups, 'apikey within double quotes')
library(data.table)
users <- setDT(members)[,.SD[.N > 16], by = user_id]
save(users,file="meetup_users.Rdata")

#2. Creating adjacency matrices and edge lists
install.packages("Matrix")
load("meetup_users.Rdata") 

library(Matrix)

grp.membership = sparseMatrix(users$group_id, users$user_id, x = TRUE)

adjacency = t(grp.membership) %*% grp.membership

users.edgelist <- as.data.frame(summary(adjacency))

names(users.edgelist)

users.edgelist.upper <- users.edgelist[users.edgelist$i < users.edgelist$j,]

save(users.edgelist.upper, file = "users_edgelist_upper.Rdata")

#3. Plotting social network data
install.packages("igraph")
load("users_edgelist_upper.Rdata")

edgelist.filtered <- users.edgelist.upper[users.edgelist.upper$x > 16,]
edgelist.filtered  
nrow(edgelist.filtered)
#If you get 0 rows in the above command, adjust the filter above suitably.
# save(edgelist.filtered, file="filtered_edgelist.Rdata")
uids <- unique(c(edgelist.filtered$i, edgelist.filtered$j))

i <- match(edgelist.filtered$i, uids)
j <- match(edgelist.filtered$j, uids)

nw.new <- data.frame(i, j, x = edgelist.filtered$x)

library(igraph)

g <- graph.data.frame(nw.new, directed=FALSE)
g
save(g, file = "undirected-graph.Rdata")

plot.igraph(g, vertex.size = 20)

plot.igraph(g,layout=layout.circle, vertex.size = 20)

plot.igraph(g,edge.curved=TRUE,vertex.color="pink", edge.color="black")

V(g)$size=degree(g) * 4

plot.igraph(g,edge.curved=TRUE,vertex.color="pink", edge.color="black")

color <- ifelse(degree(g) > 5,"red","blue")

size <- degree(g)*4

plot.igraph(g,vertex.label=NA,layout= layout.fruchterman.reingold,vertex.color=color,vertex.size=size)

E(g)$x

plot.igraph(g,edge.curved=TRUE,edge.color="black", edge.width=E(g)$x/5)

dg <- graph.data.frame(nw.new)

save(dg, file = "directed-graph.Rdata")

plot.igraph(dg,edge.curved=TRUE,edge.color="black", edge.width=E(dg)$x/10,vertex.label.cex=.6)

nw.weights <- nw.new

names(nw.weights) <- c("i","j","weight")

g.weights <- graph.data.frame(nw.weights, directed=FALSE)

g.weights

get.adjacency(g,type="upper")

get.adjacency(g, type = "lower", attr = "x")

y <- get.data.frame(g)

y <- get.data.frame(g,"vertices")

set.seed(2015)

g1 <- rbinom(10,1,.5)
g2 <- rbinom(10,1,.5)
g3 <- rbinom(10,1,.5)
g4 <- rbinom(10,1,.5)

membership <- data.frame(g1, g2, g3, g4)

names(membership)

rownames(membership) = c("u1", "u2", "u3", "u4", "u5", "u6", "u7", "u8", "u9", "u10")

rownames(membership)

bg <- graph.incidence(membership)
bg

V(bg)$type

V(bg)$name

lay <- layout.bipartite(bg)

plot(bg, layout=lay, vertex.size = 20)

save(bg, file = "bipartite-graph.Rdata")

p <- bipartite.projection(bg)
p

plot(p$proj1, vertex.size = 20)
plot(p$proj2, vertex.size = 20)

#4. Computing important network metrics

load("undirected-graph.Rdata")
load("directed-graph.Rdata")
load("bipartite-graph.Rdata")

degree(dg)
degree(g)
degree(dg, "7")
degree(dg, 9, mode = "in")
degree(dg, 9, mode = "out")

options(digits=3)
degree.distribution(bg)

betweenness(dg)
betweenness(g)
betweenness(dg, 5)
edge.betweenness(dg)
edge.betweenness(dg,10)

options(digits=3)
closeness(dg,mode="in")
closeness(dg,mode="out")
closeness(dg,mode="all")
closeness(dg)

E(dg)

neighbors(g, 1)
neighbors(bg, "u1")
V(bg)$name[neighbors(bg,"g1")]
neighborhood(dg, 1, 1)
neighborhood(dg, 2, 1)

g.new <- g + vertex(19)
g.new <- g + vertices(19, 20)

g.new <- g.new + edge(15, 20)

g.new <- delete.vertices(g.new, V(g.new)[ degree(g.new)==0 ])

g.new <- delete.vertices(g.new,12)

g.sub <- induced.subgraph(g, c(5, 10, 13, 14, 17, 11, 7))
E(dg)

eids <- c(1:2, 9:15)
dg.sub <- subgraph.edges(dg, eids)

install.packages(c("twitteR","igraph","graphTweets"))

library(twitteR)
library(graphTweets) 
library(igraph)
library(dplyr)

api_key <- "xgBgAyuBo8Fpmpj9n1W47jQEW"
api_secret <- "0dc8sIs1ExWltVnXC20j5Y225Qa81tS4x4oyj1fBiicOkEHSEC"
access_token <- "1127738809-i11uTmZrRHRjHMQk5MhIG1nWloX9zDc7GKal0bA"
access_token_secret <- "Oy7AJ0DcQqT8wprLIzLw2aMBL9aouXvSxOd5JiMBpOPtV"
  
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)


alltweets <- searchTwitter("demonetization", n = 1500)
alltweets <- twListToDF(alltweets)

tweets <- alltweets[1:500,]


split_point = split(tweets, tweets$isRetweet)

reTweets = mutate(split_point[['TRUE']], sender = substr(text, 5, regexpr(':', text) - 1))

edge_list = as.data.frame(cbind(sender = tolower(reTweets$sender), receiver = tolower(reTweets$screenName)))
edge_list = count(edge_list, sender, receiver)
edge_list[1:5,]

reTweets_graph <- graph_from_data_frame(d=edge_list, directed=T)

save(reTweets_graph, file = "retweet-graph.Rdata")

par(bg="white", mar=c(1,1,1,1))
plot(reTweets_graph, layout=layout.graphopt,
     vertex.color="yellow",
     vertex.size=(degree(reTweets_graph, mode = "in")), #sized by in-degree centrality
     vertex.label = NA,
     edge.arrow.size=0.8,
     edge.arrow.width=0.5,
     edge.width=edge_attr(reTweets_graph)$n/10, #sized by edge weight
     edge.color=hsv(h=.95, s=1, v=.7, alpha=0.5))
title("Retweet Network", cex.main=1, col.main="black")



load("retweet-graph.Rdata") 


plot(reTweets_graph, layout=layout.fruchterman.reingold,
     vertex.color="blue",
     vertex.size=(degree(reTweets_graph, mode = "in")), #sized by in-degree centrality
     vertex.label = NA,
     edge.arrow.size=0.8,
     edge.arrow.width=0.5,
     edge.width=edge_attr(reTweets_graph)$n/10, #sized by edge weight
     edge.color=hsv(h=.95, s=1, v=.7, alpha=0.5))
title("Retweet Network using Force Layout", cex.main=1, col.main="black")



install.packages("devtools") 
library(devtools)
devtools::install_github("analyxcompany/ForceAtlas2")
library(ForceAtlas2)

layout <- layout.forceatlas2(reTweets_graph, iterations=2000, plotstep=100)
plot(g, layout=layout)


load("retweet-graph.Rdata")
write.graph(reTweets_graph, "~/Desktop/reTweet_graph.graphml", format="graphml")