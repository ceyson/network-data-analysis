setwd("C:/Users/Rigel/Documents/Network Analysis/staff")
#setwd("C:/Users/Rigel/Documents/Network Analysis/SAS Compare")
#setwd("C:/Users/Rigel/Documents/Network Analysis/Dunn")


# +++++++++++++++++++++++++++++
# Program starts here
# +++++++++++++++++++++++++++++

## Grab memory
#memory.limit(size=4000)

## Load dependencies
library(igraph)
 
## Select .csv file
cat('\nPlease choose the csv file for analysis.\n')
fn <- normalizePath(file.choose())
 
## Count number of fields
tot.fields <- count.fields(fn, sep=",")[1]
 
## Loop
for (i in seq(from=1, to=tot.fields-1)) {

## Read data
data <- eval(parse(text=paste("read.csv(fn, header=TRUE)[c(1,",i+1,")]")))
data[data == ""] <- NA
data <- na.omit(data)
 
## Look
# print(dim(data))
# print(head(data))
# print(names(data))
 
## Assign Frequencies
nc1 <- length(unique(data[,1]))
nc2 <- length(unique(data[,2]))

## Establish graph
assign(paste("Graph_", i, sep=""),graph.data.frame(data, directed=F))

## Add attributes
eval(parse(text=paste("V(Graph_",i,")","[1:",nc1,"]$kind"," <- ","'",names(data)[1],"'",sep="")))
eval(parse(text=paste("V(Graph_",i,")","[",nc1+1,":",nc1+nc2,"]$kind"," <- ","'",names(data)[2],"'",sep="")))

## Get attribute vectors
a <- eval(parse(text=paste("V(Graph_",i,")$kind[1:",nc1,"]",sep="")))
b <- eval(parse(text=paste("V(Graph_",i,")$kind[",nc1+1,":",nc1+nc2,"]",sep="")))

## Attribute Vector concatenation
if (i==1) {
  attributes <- c(a,b)
  # print(attributes)
}
else {
  attributes <- c(attributes,b)
  # print(attributes)
}

## Free up memory - drop 'data'
#rm(data)
 
}
## End loop

## Merge graphs
Graphs <- as.symbol(paste(ls()[grep("Graph_", ls())], sep=" ", collapse=", "))
analysis.graph <- eval(parse(text=paste("graph.union(",Graphs,")")))

## Free up memory - remove graphs
#eval(parse(text=paste("rm(",Graphs,")")))

## Add labels to graph
V(analysis.graph)$kind <- attributes
V(analysis.graph)$name <- iconv(V(analysis.graph)$name, to='ASCII', sub="")
V(analysis.graph)$Label <- V(analysis.graph)$name

## Remove kind attribute
analysis.graph <- remove.vertex.attribute(analysis.graph, 'kind')

## Duplicate 'kind_1' as 'kind'
V(analysis.graph)$kind <- V(analysis.graph)$kind_1

## Remove 'kind_1'
analysis.graph <- remove.vertex.attribute(analysis.graph, 'kind_1')

## Begin loop
for (j in seq(from=1, to=tot.fields-2)) {

## Determine NAs in kind (primary column)
na.pos <- is.na(V(analysis.graph)$kind)

## For false positions on the primary column assign existing values
eval(parse(text=paste("V(analysis.graph)$kind[na.pos] <- V(analysis.graph)$kind_",j+1,"[na.pos]",sep="")))

## Remove uncessary 'kind_' attributes
eval(parse(text=paste("analysis.graph <- remove.vertex.attribute(analysis.graph, 'kind_",j+1,"')",sep="")))

} ## End loop


## Cluster IDs
V(analysis.graph)$cluster_id <- as.character(clusters(analysis.graph)$membership)


## Cluster size
V(analysis.graph)$cluster_size <- as.character(clusters(analysis.graph)$csize[clusters(analysis.graph)$membership])


## Export 
## Gephi 
write.graph(analysis.graph, 'analysis.graphml', format="graphml")
## Cytoscape 
write.graph(analysis.graph, 'analysis.gml', format="gml")
 

# ++++++++++++++++++++++++++++
# Program Ends Here
# ++++++++++++++++++++++++++++

# +++++++++++++++++++++++++++++
# Program starts here
# +++++++++++++++++++++++++++++

#cat('\nPlease choose the graphml file containing the network.\n')
#fn <- normalizePath(file.choose())
 
## read the graph
#g <- read.graph(fn,format='graphml')

 
## create the attributes data frame
attributes <- list()
for(thisAttribute in list.vertex.attributes(analysis.graph)){
    attributes[thisAttribute] <- list(get.vertex.attribute(analysis.graph,thisAttribute))
}
attributes <- as.data.frame(attributes)
 
## create the edge list
V(analysis.graph)$name <- V(analysis.graph)$uid
edgelist <- get.edgelist(analysis.graph)
 
## write to files
#d <- dirname(fn)
d <- getwd()
attrPath <- paste(d,'analysis_graph_attributes.csv',sep='/')
edgePath <- paste(d,'analysis_graph_edges_edges.csv',sep='/')
write.csv(attributes,file=attrPath,row.names=FALSE)
write.csv(edgelist,file=edgePath,row.names=FALSE,quote=FALSE)
cat(sprintf("\nwrote two files:\n  %s\n  %s\n",attrPath,edgePath))

## Remove analysis.graph
#rm(analysis.graph)

## Remove ALL object
#rm(list=ls())

# ++++++++++++++++++++++++++++
# Program Ends Here
# ++++++++++++++++++++++++++++


## ++++++++++++++++++++++++++
## Bonus Code
## ++++++++++++++++++++++++++

# Frequency by cluster size
cluster.freq <- unique(data.frame(cluster_id=V(analysis.graph)$cluster_id, cluster_size=V(analysis.graph)$cluster_size))
cluster.freq[order(cluster.freq$cluster_size),]

# Peel off cluster of interest to graph
drop.clusters <- V(analysis.graph)[V(analysis.graph)$cluster_size != "15"]
interest.graph <- delete.vertices(analysis.graph, drop.clusters)

## Write graph
write.graph(interest.graph, 'interest-graph.graphml', format="graphml")

## Add color attributes
V(interest.graph)[V(interest.graph)$kind=="person"]$color <- "green"
V(interest.graph)[V(interest.graph)$kind=="office"]$color <- "red"
V(interest.graph)[V(interest.graph)$kind=="location"]$color <- "blue"

## Set size of nodes by degree
V(interest.graph)$size <- degree(interest.graph)*2

## Interactive plot (from R)
tkplot(interest.graph)


## possibly loop through clusters above a threshold and write to graph files.

# giant.component <- function(graph, ...) {
#  cl <- clusters(graph, ...)
#  subgraph(graph, which(cl$membership == which.max(cl$csize)-1)-1)
# }
#
# tail.component <- function(graph, ...) {
#  cl <- clusters(graph, ...)
#  subgraph(graph, which(cl$membership != which.max(cl$csize)-1)-1)
# }






