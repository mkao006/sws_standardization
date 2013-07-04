########################################################################
## Title: Script to pre-standardize all data
## Date: 2013-06-17
########################################################################

source("wheatDataManipulation.R")
source("riceDataManipulation.R")
source("computeDirectExtractRate.R")
source("toExtractTree.R")
source("fillMissingCartesian.R")


allProcess.df = unique(rbind(riceProcess.df, wheatProcess.df))
allExtract.dt = unique(rbind(riceExtract.dt, wheatExtract.dt))
allTrade.dt = unique(rbind(riceTrade.dt, wheatTrade.dt))

## Pre-standardization
## ---------------------------------------------------------------------

## split and merge the extraction rate data with the tree
allExtract.lst = toExtractTree(extractData = allExtract.dt,
  treeData = subTree.dt,
  splitKey = allExtract.dt[, list(Area.Code, Year)],
  itemKey = "Item.Code", areaCol = "Area.Code", yearCol = "Year",
  extractSpecificCol = "ExtractRate",
  extractDefaultCol = "Default.Extraction.Rates")

## Compute the direct extraction rate
##
## NOTE (Michael): Sometimes this does not work, since the extraction
##                 rate is zero which makes it not connected to the
##                 primary product.
allDirectExtract.lst =
  vector(mode = "list", length = length(allExtract.lst))
for(i in 1:length(allExtract.lst)){
  allDirectExtract.lst[[i]] =
    data.table(merge(computeDirectExtractRate(Data = allExtract.lst[[i]],
                                   child = "Item.Code", parent = "FBS.Parent.Code",
                                   ExtractionRate = "ExtractRate"),
          allExtract.lst[[i]], all.x = TRUE))
}


## If the data has multiple paths, then the unique rate needs to be taken. 
allDirectExtract.dt =
  data.table(subset(do.call("rbind", allDirectExtract.lst),
  select = c("Area.Code", "Item.Code", "Year", "Primary",
    "Primary.Extraction.Rate")))


## Merge trade data with direct rates
allFinal.dt = merge(allTrade.dt, allDirectExtract.dt, all.x = TRUE,
  by = c("Area.Code", "Item.Code", "Year"))
allFinal.dt = merge(allFinal.dt,
  fullTree.dt[, list(Item.Code, Weight, Use.Calorie)],
  all.x = TRUE, by = "Item.Code")

## Standardize the data
allStandard.dt = allFinal.dt[, sum(Trade * Primary.Extraction.Rate * Weight,
  na.rm = TRUE), by = c("Area.Code", "Primary", "Element.Code", "Year")]
setnames(allStandard.dt, old = "V1", new = "standardValue")
setnames(allStandard.dt, old = "Primary", new = "Item.Code")
