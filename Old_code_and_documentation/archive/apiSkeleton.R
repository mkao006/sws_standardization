########################################################################
## Title: Skeleton code for the trade standardization procedure
## Date: 2014-06-04
########################################################################

## Steps:
##
## (1) Read Extraction rate data
## (2) Read tree/network control file
## (3) Read Trade data
## (4) Compute direct extraction rate
## (5) Perform standardization

## load libraries
library(data.table)
library(igraph)
library(reshape2)
source("computeDirectExtractRate.R")
source("toExtractTree.R")
source("fillMissingCartesian.R")

wheatCaseStudy = TRUE

## (1) Read Extraction rate
##
## NOTE (Michael): Need to becareful of the fact that element 41 is
##                 yield for primary commodity and the extraction rate
##                 needs to be set to 10000.
##
## ---------------------------------------------------------------------
if(wheatCaseStudy)
    extractionRate.dt = data.table(read.csv("wheatExtractRate.csv"))

## extractionRate.dt = extractionRate.dt[Year %in% c(2008), ]

## (2) Read tree/network control file
##
## ---------------------------------------------------------------------
completeFBSnetwork.dt = data.table(read.csv(file = "tree_test.csv",
  stringsAsFactors = FALSE))
subNetwork.dt =
    completeFBSnetwork.dt[, list(Item.Code, FBS.Parent.Code,
                                 Default.Extraction.Rates)]
completeFBSnetwork.graph =
    graph.data.frame(
        completeFBSnetwork.dt[, list(Item.Code, FBS.Parent.Code)]
        )

## if(wheatCaseStudy){
##     standardization.graph = induced.subgraph(completeFBSnetwork.graph,
##         V(completeFBSnetwork.graph)
##         [which(is.finite(shortest.paths(completeFBSnetwork.graph,
##                                         to = "2511")))]$name)
##     plot(standardization.graph)
## }

## (3) Read Trade data
##
## ---------------------------------------------------------------------
if(wheatCaseStudy)
    trade.dt = data.table(read.csv("wheatTrade.csv"))

## trade.dt = trade.dt[Year %in% c(2008), ]


## (4) Compute direct extraction rate
##
## NOTE (Michael): Current computation only support one path between
##                 two verticex (includes all structure specified by
##                 Maarten). Multiple path such as processed cheese to
##                 (skim milk/fresh cream) then to full milk are not
##                 supported. One way to account for this is to
##                 seperate the item to skim milk processed cheese and
##                 fresh cream processed cheese by their shares, then
##                 perform the standardization. This implies that an
##                 additional control file for the split and new codes
##                 are required.
##
## ---------------------------------------------------------------------

## split and merge the extraction rate data with the tree
extract.lst = toExtractTree(extractData = extractionRate.dt,
    treeData = subNetwork.dt,
    splitKey = extractionRate.dt[, list(Area.Code, Year)],
    itemKey = "Item.Code", areaCol = "Area.Code", yearCol = "Year",
    extractSpecificCol = "ExtractRate",
    extractDefaultCol = "Default.Extraction.Rates")

foo = function(extractList){
    tmp = merge(computeDirectExtractRate(extractList,
        child = "Item.Code", parent = "FBS.Parent.Code",
        ExtractionRate = "ExtractRate"), extractList, all.x = TRUE)
    data.table(tmp)
}

directExtract.dt =
    subset(do.call("rbind", lapply(extract.lst, foo)),
           select = c("Area.Code", "Item.Code", "Year",
               "Primary", "Primary.Extraction.Rate"))




## (5) Perform standardization
##
## ---------------------------------------------------------------------
## Merge trade data with direct rates
tradeExtract.dt = merge(trade.dt, directExtract.dt, all.x = TRUE,
  by = c("Area.Code", "Item.Code", "Year"))
final.dt = merge(tradeExtract.dt,
    completeFBSnetwork.dt[, list(Item.Code, Weight, Use.Calorie)],
    all.x = TRUE, by = "Item.Code")

## Standardize the data
if(wheatCaseStudy){
    standardized.dt = final.dt[Primary == 2511,
        sum(Trade * Primary.Extraction.Rate * Weight, na.rm = TRUE),
        by = c("Area.Code", "Element.Code", "Year")]
    setnames(standardized.dt, old = "V1", new = "standardized")
    standardized.df =
        dcast(standardized.dt, Area.Code + Year ~ Element.Code,
              value.var = "standardized")
    colnames(standardized.df) =
        c("Area.Code", "Year", "import", "export")
}
