########################################################################
## Title: Test example for wheat
## Date: 2013-05-14
########################################################################

library(FAOSTAT)
library(igraph)
library(data.table)
library(Matrix)
library(reshape2)
source("read.sua.R")


## Data preperation
## ---------------------------------------------------------------------


## Read and construct the full FBS network
completeFBSnetwork.dt = data.table(read.csv(file = "tree_test.csv",
  stringsAsFactors = FALSE))
subNetwork.dt = completeFBSnetwork.dt[, list(Item.Code, FBS.Parent.Code,
    Default.Extraction.Rates)]
completeFBSnetwork.graph =
    graph.data.frame(
        completeFBSnetwork.dt[, list(Item.Code, FBS.Parent.Code)]
        )
wheat.graph = induced.subgraph(completeFBSnetwork.graph,
    V(completeFBSnetwork.graph)
    [which(is.finite(shortest.paths(completeFBSnetwork.graph,
                                    to = "2511")))]$name)


## Test whether the tree has multiple or loop edge and find which one
## has.multiple(wheat.graph)
## E(wheat.graph)[which(is.multiple(wheat.graph))]

## Read the wheat sua data
wheatData.dt = data.table(read.sua(file = "wheatFullTreeData.csv",
    stringsAsFactors = FALSE,
    keys = c("Area.Code", "Item.Code", "Element.Code", "Element.Name")))


## Items in wheat network
with(FAOmetaTable,
     arrange(unique(itemTable[itemTable$itemCode %in%
                              unique(wheatData.dt$Item.Code),
                              c("itemCode", "itemName")]),
             as.numeric(itemCode))
     )


## Only take data within the specified tree and only take value not
## symble
wheatProcess.dt = subset(wheatData.dt,
  subset = Element.Code %in% c(41, 61, 91) & Type == "Value")
wheatProcess.dt[, value := as.numeric(value)]
wheatProcess.dt[grepl("Yield", Element.Name), value := 10000]


## Seperate the data into trade and extraction
## Extraction
wheatExtract.dt = subset(wheatProcess.dt,
    subset = Element.Code %in% 41,
    select = c(Area.Code, Item.Code, Year, value))
setnames(wheatExtract.dt, old = "value", new = "ExtractRate")
setkeyv(wheatExtract.dt, c("Area.Code", "Item.Code", "Year"))
               

## Take the trade data
wheatTrade.dt = data.table(subset(wheatProcess.df,
    Element.Code %in% c(61, 91),  
    select = c(Area.Code, Item.Code, Element.Code, Year, value)))
setnames(wheatTrade.dt, old = "value", new = "Trade")
wheatTrade.dt[, Trade := Trade/1000]
wheatTrade.dt[Element.Code == 61, Element.Code := as.integer(5611)]
wheatTrade.dt[Element.Code == 91, Element.Code := as.integer(5911)]
