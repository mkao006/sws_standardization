########################################################################
## Title: Test example for wheat
## Date: 2013-05-14
########################################################################

library(FAOSTAT)
library(igraph)
library(data.table)
library(Matrix)
source("computeDirectExtractRate.R")
source("read.sua.R")
source("toExtractTree.R")
source("fillMissingCartesian.R")


## Data preperation
## ---------------------------------------------------------------------


## Using the full tree
fullTree.dt = data.table(read.csv(file = "tree_test.csv",
  stringsAsFactors = FALSE))
subTree.dt = fullTree.dt[, list(Item.Code, Item.Name, FBS.Parent.Code,
  Default.Extraction.Rates, Weight, Use.Calorie)]
fullTree.graph = graph.data.frame(fullTree.dt[, list(Item.Code, FBS.Parent.Code)])
tmp.graph = subgraph(fullTree.graph, V(fullTree.graph)
  [which(is.finite(shortest.paths(fullTree.graph, to = "2511")))]$name)


## Test whether the tree has multiple or loop edge and find which one
## has.multiple(wheatTree.graph)
## E(wheatTree.graph)[which(is.multiple(wheatTree.graph))]

## Read the wheat sua tree data
wheatData.df = read.sua(file = "wheatFullTreeData.csv", stringsAsFactors = FALSE,
  keys = c("Area.Code", "Item.Code", "Element.Code", "Element.Name"))

## Elements in wheat tree
unique(FAOmetaTable$itemTable[FAOmetaTable$itemTable$itemCode %in%
                       unique(wheatData.df$Item.Code), c("itemCode", "itemName")])


## Only take data within the specified tree and only take value not symble
wheatProcess.df = subset(wheatData.df, Type == "Value")
wheatProcess.df$value = as.numeric(wheatProcess.df$value)

## Process the full data into trade and extraction 
wheatExtract.dt = data.table(subset(wheatProcess.df,
  subset = Element.Code == 41,
  select = c(Area.Code, Item.Code, Year, value)))
wheatExtract.dt[, value := as.numeric(value)]
setnames(wheatExtract.dt, old = "value", new = "ExtractRate")
setkeyv(wheatExtract.dt, c("Area.Code", "Item.Code", "Year"))

## This is a hack to change the yield to extraction rate
wheatExtract.dt[Item.Code == 15, ExtractRate := 10000]
