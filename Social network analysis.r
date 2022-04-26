library(dplyr)
library(igraph)
library(sqldf)

#Read files
products <- read.csv('products.csv')
co_purchase <- read.csv('copurchase.csv')

book_products <- products[products$group == "Book" & (products$salesrank<150000 & products$salesrank>-1), ]
book_id <- book_products$id
co_purchase_books <- co_purchase[co_purchase$Source %in% book_id & co_purchase$Target %in% book_id,]

#In degree
graph <- graph.data.frame(co_purchase_books,directed=T)
# V(graph)
# E(graph)
indegree <- degree(graph,v = V(graph),mode = "in")
head(sort(indegree,decreasing = TRUE))

#Outdegree
outdegree <- degree(graph,v = V(graph),mode = "out")
head(sort(outdegree,decreasing = TRUE))

#Total Degree
tot_degree <- degree(graph,v = V(graph),mode = "all")
which.max(tot_degree) 
head(sort(tot_degree,decreasing = TRUE))
prod_4429 <- subcomponent(graph,"4429",mode="all")
#prod_4429

sub_4429 <- induced_subgraph(graph, prod_4429)
V(sub_4429)$label <- V(sub_4429)$name 
V(sub_4429)$degree <- degree(sub_4429)
diameter(sub_4429, directed = T, weights = NA)
d <- get_diameter(sub_4429, weights = NULL)
d

plot(sub_4429,
     vertex.color=rainbow(52),
     edge.color='red',
     vertex.size= V(sub_4429)$degree*0.2,
     edge.arrow.size=0.2,
     vertex.label.cex=0.05,
     layout=layout.kamada.kawai)

in_degree <- degree(sub_4429, mode="in")
hist(indegree, main='In Degree Distribution of Subcomponent')
out_degree = degree(sub_4429, mode="out")
hist(out_degree, main='Out Degree Distribution of Subcomponent')


#Density
ecount(sub_4429)/(vcount(sub_4429)*(vcount(sub_4429)-1))
# centrality
reciprocity(sub_4429)
#Closeness
closeness <- closeness(sub_4429, mode='all', weights=NA)
hist(closeness)
head(sort(closeness(sub_4429, mode='all', weights=NA), decreasing = TRUE))
#Betweenness
betweenness <- betweenness(sub_4429, directed='T', weights=NA)
hist(betweenness)
head(sort(betweenness(sub_4429, directed='T', weights=NA), decreasing = TRUE))

#Hub & Authority Score
hs1 <- hub.score(sub_4429)$vector
as1 <- authority.score(sub_4429)$vector
head(sort(hs1, decreasing = TRUE))
head(sort(as1, decreasing = TRUE))

nghb_mn_rating <-co_purchase_books %>%
        group_by(Target) %>%
        inner_join(book_products, by=c("Source"="id"))%>%
        transmute(nghb_mn_rating=mean(rating))
head(nghb_mn_rating)

nghb_mn_rank <- co_purchase_books %>%
        group_by(Target) %>%
        inner_join(book_products, by=c("Source"="id"))%>%
        transmute(nghb_mn_rank=mean(salesrank))
head(nghb_mn_rank)

nghb_mn_review_cnt<- co_purchase_books %>%
        group_by(Target) %>%
        inner_join(book_products, by=c("Source"="id"))%>%
        transmute(nghb_mn_review_cnt=mean(review_cnt))
head(nghb_mn_review_cnt)

mean <- co_purchase_books %>% 
        group_by(Target) %>% 
        inner_join(book_products, by=c("Source"="id")) %>%
        summarise(nghb_mn_rating=mean(rating),
                  nghb_mn_salesrank=mean(salesrank),
                  nghb_mn_review_cnt=mean(review_cnt))

in_degree1 <- as.data.frame(in_degree)
in_degree1 <- cbind(newColName = rownames(in_degree1), in_degree1)
rownames(in_degree1) <- 1:nrow(in_degree1)
colnames(in_degree1) <- c("Nodes", "in_degree")

out_degree1 <- as.data.frame(out_degree)
out_degree1 <- cbind(newColName = rownames(out_degree1), out_degree1)
rownames(out_degree1) <- 1:nrow(out_degree1)
colnames(out_degree1) <- c("Nodes", "out_degree")

closeness1 <- as.data.frame(closeness)
closeness1 <- cbind(newColName = rownames(closeness1), closeness1)
rownames(closeness1) <- 1:nrow(closeness1)
colnames(closeness1) <- c("Nodes", "closeness")

betweeness1 <- as.data.frame(betweenness)
betweeness1 <- cbind(newColName = rownames(betweeness1), betweeness1)
rownames(betweeness1) <- 1:nrow(betweeness1)
colnames(betweeness1) <- c("Nodes", "betweeness")

hub_score2 <- as.data.frame(hs1)
hub_score2 <- cbind(newColName = rownames(hub_score2), hub_score2)
rownames(hub_score2) <- 1:nrow(hub_score2)
colnames(hub_score2) <- c("Nodes", "hub_score")

authority_score2 <- as.data.frame(as1)
authority_score2 <- cbind(newColName = rownames(authority_score2), authority_score2)
rownames(authority_score2) <- 1:nrow(authority_score2)
colnames(authority_score2) <- c("Nodes", "authority_score")


poisson_data <- sqldf("SELECT hub_score2.Nodes, hub_score, betweeness, 
        authority_score, closeness, in_degree, out_degree, nghb_mn_rating, 
        nghb_mn_salesrank, nghb_mn_review_cnt, products.id, products.review_cnt,
        products.rating, products.salesrank
        FROM hub_score2, betweeness1, authority_score2, closeness1, in_degree1,
        out_degree1, mean, products
        WHERE hub_score2.Nodes = betweeness1.Nodes 
        and hub_score2.Nodes = authority_score2.Nodes
        and hub_score2.Nodes = closeness1.Nodes
        and hub_score2.Nodes = in_degree1.Nodes
        and hub_score2.Nodes = out_degree1.Nodes
        and hub_score2.Nodes = mean.Target
        and hub_score2.Nodes = products.id")

str(poisson_data)

#run poisson regression
summary(salesrating_prediction<- glm(salesrank ~ review_cnt + rating + hub_score
        + betweeness + authority_score + closeness + in_degree + out_degree + 
        nghb_mn_rating+ nghb_mn_salesrank + nghb_mn_review_cnt, 
        family="poisson", data=poisson_data))

#Experimenting with Log transformation, yields similar results
summary(salesrating_prediction<- glm(salesrank ~ log(review_cnt+1) + log(rating+1) + hub_score
        + log(betweeness+1) + authority_score + closeness + log(in_degree+1) + out_degree
        + nghb_mn_rating+ log(nghb_mn_salesrank+1) + log(nghb_mn_review_cnt+1), 
        family="poisson", data=poisson_data))
