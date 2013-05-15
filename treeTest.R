########################################################################
## Title: Test for tree type computation
## Date: 2013-05-14
########################################################################

## Using networks
test.df = data.frame(com = c("Wheat", "Flour", "Germ", "Bran", "Bread",
                       "Macaroni", "Rice"),
  target = c("Wheat", "Wheat", "Wheat", "Wheat", "Flour", "Flour", "Milled Rice"),
  rate = rnorm(7))
  
library(network)
test.net = network(test.df[, 1:2])
set.edge.attribute(test.net, "weight", test.df[, 3])
gplot(test.net)


library(igraph)
test.graph = graph.data.frame(test.df[, 1:2], directed = TRUE)
set.graph.attribute(test.graph, "weight", test.df[, 3])
print(test.graph, e = TRUE, v = TRUE)
plot(test.graph)


exp(shortest.paths(graph = test.graph, to = "Wheat", weights = test.df[, 3]))

library(FAOSTAT)
library(igraph)
library(data.table)
## NOTE (Michael): There is a problem when the extraction rate is less
##                than one, because when the log is take, the weight
##                becomes negative.
## NOTE (Michael): Need to use the average path rather than the
##                 shortest path since there can be multiple path.
computeDirectExtractRate = function(Data, child, parent, ExtractionRate,
  primary, k = 10){
  Data$cf = (k * 10000)/Data[, ExtractionRate]
  tmp.graph = graph.data.frame(Data[, c(child, parent)], directed = TRUE)
  wldist = shortest.paths(graph = tmp.graph,
    to = primary, weights = log(Data[, "cf"]), algorithm = "johnson")
  dist = shortest.paths(graph = tmp.graph, to = primary, algorithm = "johnson")
  wdist = exp(wldist - dist * log(k))
  finite = which(is.finite(wdist))
  tmp = data.frame(as.numeric(rownames(wdist)[finite]),
    as.numeric(rep(primary, length(finite))),
    wdist[finite], stringsAsFactors = FALSE)
  colnames(tmp) = c(child, "Primary", "Primary.Extraction.Rate")
  tmp
}

## Read the wheat tree
wheatTree.dt = data.table(read.csv(file = "wheatTree.csv", header = TRUE,
  stringsAsFactors = FALSE))
wheatTree.dt = wheatTree.dt[Item.Code != 15, ]
setkeyv(wheatTree.dt, "Item.Code")
wheatTree.graph = graph.data.frame(wheatTree.dt[, list(Item.Name, Parent.Name)])
plot(wheatTree.graph)

## Test whether the tree has multiple or loop edge and find which one
has.multiple(wheatTree.graph)
E(wheatTree.graph)[which(is.multiple(wheatTree.graph))]

## Read the wheat sua tree data
wheatData.df = read.csv(file = "wheatFullTreeData.csv", header = TRUE,
  stringsAsFactors = FALSE)

## Process the full data into trade and extraction 
wheatProcess.df = subset(wheatData.df, Item.Code != 15)
wheatExtract.dt = data.table(subset(wheatProcess.df, subset = Element.Code == 41,
  select = c(Area.Code, Item.Code, Num_2010)))
setnames(wheatExtract.dt, old = "Num_2010", new = "ExtractRate")
setkeyv(wheatExtract.dt, "Item.Code")
wheatExtract.lst = split(wheatExtract.dt, wheatExtract.dt[, list(Area.Code)])

wheatExtractTree.lst = lapply(X = wheatExtract.lst,
  FUN = function(x) merge(wheatTree.dt, x, all.x = TRUE, allow.cartesian = TRUE,
    by = "Item.Code"))


setdiff(unique(wheatExtract.dt$Item.Code), wheatTree.dt$Item.Code)
unique(FAOmetaTable$itemTable[FAOmetaTable$itemTable$itemCode %in%
                              c(51, 633, 168, 171, 50), "itemName"])

wheatDirectExtractTree.lst = vector(mode = "list",
  length = length(wheatExtractTree.lst))
for(i in 1:length(wheatExtractTree.lst)){
  wheatDirectExtractTree.lst[[i]] = merge(computeDirectExtractRate(Data =
        data.frame(wheatExtractTree.lst[[i]]),
                                 child = "Item.Code", parent = "Parent.Code",
                                 rate = "ExtractRate", primary = "15"),
        wheatExtractTree.lst[[i]])
}

wheatDirectExtractTree.lst = lapply(X = wheatExtractTree.lst[1:2],
  FUN = function(x) computeDirectExtractRate(Data = data.frame(x),
    child = "Item.Code", parent = "Parent.Code", rate = "Conv.Factor",
    primary = "15"))

wheatDirectExtractTree.dt = unique(data.table(subset(do.call("rbind",
  wheatDirectExtractTree.lst),
  select = c("Item.Code", "Item.Name", "Primary", "Primary.Rate", "Area.Code"))))
  
  

## Take the trade data
wheatTrade.dt = data.table(subset(wheatProcess.df, Element.Code %in% c(61, 91)))

wheatFinal.dt = merge(wheatTrade.dt, wheatDirectExtractTree.dt, all.x = TRUE,
  by = c("Area.Code", "Item.Code"))


wheatStandard.dt = wheatFinal.dt[, sum(Num_2010 * Primary.Rate, na.rm = TRUE),
  by = "Area.Code"]


## Issues:
## (1) Need to restrict to only a single path
## (2) Need to account for cases where extraction rate are not available.
