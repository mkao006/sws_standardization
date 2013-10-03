########################################################################
## Title: Script to pre-standardize wheat data
## Date: 2013-06-17
########################################################################

source("wheatDataManipulation.R")
source("computeDirectExtractRate.R")
source("toExtractTree.R")
source("fillMissingCartesian.R")

## Pre-standardization
## ---------------------------------------------------------------------

## split and merge the extraction rate data with the tree
wheatExtract.lst = toExtractTree(extractData = wheatExtract.dt,
  treeData = subTree.dt,
  splitKey = wheatExtract.dt[, list(Area.Code, Year)],
  itemKey = "Item.Code", areaCol = "Area.Code", yearCol = "Year",
  extractSpecificCol = "ExtractRate",
  extractDefaultCol = "Default.Extraction.Rates")

## Compute the direct extraction rate
##
## NOTE (Michael): Sometimes this does not work, since the extraction
##                 rate is zero which makes it not connected to the
##                 primary product.
wheatDirectExtract.lst =
  vector(mode = "list", length = length(wheatExtract.lst))
for(i in 1:length(wheatExtract.lst)){
  wheatDirectExtract.lst[[i]] =
    data.table(merge(computeDirectExtractRate(Data = wheatExtract.lst[[i]],
                                   child = "Item.Code", parent = "FBS.Parent.Code",
                                   ExtractionRate = "ExtractRate"),
          wheatExtract.lst[[i]], all.x = TRUE))
}


## If the data has multiple paths, then the unique rate needs to be taken. 
wheatDirectExtract.dt =
  data.table(subset(do.call("rbind", wheatDirectExtract.lst),
  select = c("Area.Code", "Item.Code", "Year", "Primary",
    "Primary.Extraction.Rate")))

## Merge trade data with direct rates
wheatFinal.dt = merge(wheatTrade.dt, wheatDirectExtract.dt, all.x = TRUE,
  by = c("Area.Code", "Item.Code", "Year"))
wheatFinal.dt = merge(wheatFinal.dt,
  fullTree.dt[, list(Item.Code, Weight, Use.Calorie)],
  all.x = TRUE, by = "Item.Code")

## Standardize the data
wheatStandard.dt = wheatFinal.dt[Primary == 2511,
  sum(Trade * Primary.Extraction.Rate * Weight,
  na.rm = TRUE), by = c("Area.Code", "Element.Code", "Year")]
setnames(wheatStandard.dt, old = "V1", new = "wheatStand")
wheatStandard.df = dcast(wheatStandard.dt, Area.Code + Year ~ Element.Code,
  value.var = "wheatStand")
colnames(wheatStandard.df) = c("Area.Code", "Year", "wheatImport", "wheatExport")
