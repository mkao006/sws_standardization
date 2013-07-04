########################################################################
## Title: Test example for wheat
## Date: 2013-05-14
########################################################################

library(FAOSTAT)
library(igraph)
library(data.table)
library(Matrix)
source("read.sua.R")


## Data preperation
## ---------------------------------------------------------------------


## Using the full tree
fullTree.dt = data.table(read.csv(file = "tree_test.csv",
  stringsAsFactors = FALSE))
subTree.dt = fullTree.dt[, list(Item.Code, FBS.Parent.Code,
  Default.Extraction.Rates)]
fullTree.graph = graph.data.frame(fullTree.dt[, list(Item.Code, FBS.Parent.Code)])
tmp.graph = induced.subgraph(fullTree.graph, V(fullTree.graph)
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
wheatProcess.df = subset(wheatData.df,
  subset = Element.Code %in% c(41, 61, 91) & Type == "Value")
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

## Take the trade data
wheatTrade.dt = data.table(subset(wheatProcess.df,
  Element.Code %in% c(61, 91),  
  select = c(Area.Code, Item.Code, Element.Code, Year, value)))
wheatTrade.dt[, value := as.numeric(value)]
setnames(wheatTrade.dt, old = "value", new = "Trade")
wheatTrade.dt[, Trade := Trade/1000]
wheatTrade.dt[Element.Code == 61, Element.Code := as.integer(5611)]
wheatTrade.dt[Element.Code == 91, Element.Code := as.integer(5911)]
