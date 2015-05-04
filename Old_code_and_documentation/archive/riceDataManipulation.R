########################################################################
## Title: Test example for rice
## Date: 2013-07-03
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
## has.multiple(riceTree.graph)
## E(riceTree.graph)[which(is.multiple(riceTree.graph))]

## Read the rice sua tree data
riceData.df = read.sua(file = "riceFullTreeData.csv", stringsAsFactors = FALSE,
  keys = c("Area.Code", "Item.Code", "Element.Code", "Element.Name"))

## Elements in rice tree
unique(FAOmetaTable$itemTable[FAOmetaTable$itemTable$itemCode %in%
                       unique(riceData.df$Item.Code), c("itemCode", "itemName")])


## Only take data within the specified tree and only take value not symble
riceProcess.df = subset(riceData.df,
  subset = Element.Code %in% c(41, 61, 91) & Type == "Value")
riceProcess.df$value = as.numeric(riceProcess.df$value)

## Process the full data into trade and extraction 
riceExtract.dt = data.table(subset(riceProcess.df,
  subset = Element.Code == 41,
  select = c(Area.Code, Item.Code, Year, value)))
riceExtract.dt[, value := as.numeric(value)]
setnames(riceExtract.dt, old = "value", new = "ExtractRate")
setkeyv(riceExtract.dt, c("Area.Code", "Item.Code", "Year"))

## This is a hack to change the yield to extraction rate
riceExtract.dt[Item.Code == 27, ExtractRate := 10000]

## Take the trade data
riceTrade.dt = data.table(subset(riceProcess.df,
  Element.Code %in% c(61, 91), 
  select = c(Area.Code, Item.Code, Element.Code, Year, value)))
riceTrade.dt[, value := as.numeric(value)]
setnames(riceTrade.dt, old = "value", new = "Trade")
riceTrade.dt[, Trade := Trade/1000]
riceTrade.dt[Element.Code == 61, Element.Code := as.integer(5611)]
riceTrade.dt[Element.Code == 91, Element.Code := as.integer(5911)]
