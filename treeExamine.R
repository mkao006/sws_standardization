########################################################################
## Title: Examine the test tree
## Date: 2013-05-24
########################################################################

library(data.table)
library(plyr)
library(gdata)
library(igraph)
igraph.options(label.cex = 0.8)


fullTree.dt = data.table(read.csv(file = "tree_test.csv", header = TRUE,
  stringsAsFactors = FALSE))
## fullTree.dt = data.table(read.xls(xls = "tree_test.xlsx", header = TRUE,
##   stringsAsFactors = FALSE))
setkeyv(fullTree.dt, "Item.Code")

fullTree.graph = graph.data.frame(fullTree.dt[, list(Item.Name, New.Parent.Name)])
uniqueParent = unique(fullTree.dt[Item.Name == Parent.Name, New.Parent.Name])
pdf(file = "commodity_trees.pdf")
for(i in 1:length(uniqueParent)){
tmp.graph = subgraph(fullTree.graph, V(fullTree.graph)
  [which(is.finite(shortest.paths(fullTree.graph, to = uniqueParent[i])))]$name)
if(length(V(tmp.graph)) >= 2)
  plot(tmp.graph)
}
graphics.off()
system("evince commodity_trees.pdf&")



FBSTree.graph = graph.data.frame(fullTree.dt[, list(Item.Name, FBS.Parent.Name)])
uniqueParent = unique(arrange(fullTree.dt[FBS.Parent.Code >= 2511, ],
  FBS.Parent.Code)$FBS.Parent.Name)
FBSTree.graph = set.edge.attribute(FBSTree.graph, "color",
  value = ifelse(fullTree.dt[, Weight], "black", "red"))
## uniqueParent = unique(fullTree.dt[FBS.Parent.Code >= 2511, FBS.Parent.Name])
pdf(file = "fbs_trees.pdf", width = 12, height = 12)
for(i in 1:length(uniqueParent)){
tmp.graph = subgraph(FBSTree.graph, V(FBSTree.graph)
  [which(is.finite(shortest.paths(FBSTree.graph, to = uniqueParent[i])))]$name)
## if(length(V(tmp.graph)) >= 2)
  plot.igraph(tmp.graph, layout=layout.fruchterman.reingold)
}
graphics.off()
system("evince fbs_trees.pdf&")
