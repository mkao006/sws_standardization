########################################################################
## Title: Script to pre-standardize wheat data
## Date: 2013-06-17
########################################################################

source("wheatDataManipulation.R")

## Pre-standardization
## ---------------------------------------------------------------------

## split and merge the extraction rate data with the tree
wheatExtractTree.lst = toExtractTree(extractData = wheatExtract.dt,
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
wheatDirectExtractTree.lst =
  vector(mode = "list", length = length(wheatExtractTree.lst))
for(i in 1:length(wheatExtractTree.lst)){
  wheatDirectExtractTree.lst[[i]] =
    try(merge(computeDirectExtractRate(Data = wheatExtractTree.lst[[i]],
                                   child = "Item.Code", parent = "FBS.Parent.Code",
                                   ExtractionRate = "ExtractRate"),
          wheatExtractTree.lst[[i]], all.x = TRUE))
}

## If the data has multiple paths, then the unique rate needs to be taken. 
wheatDirectExtractTree.dt = data.table(subset(do.call("rbind",
  wheatDirectExtractTree.lst),
  select = c("Area.Code", "Item.Code", "Item.Name", "Year", "Primary",
    "Primary.Extraction.Rate", "Weight")))

## Take the trade data
wheatTrade.dt = data.table(subset(wheatProcess.df,
  Element.Code %in% c(61, 91)))
wheatTrade.dt[, value := as.numeric(value)]
wheatFinal.dt = merge(wheatTrade.dt, wheatDirectExtractTree.dt, all.x = TRUE,
  by = c("Area.Code", "Item.Code", "Year"))

## Standardize the data
wheatStandard.dt = wheatFinal.dt[Primary == 2511,
  sum(value * Primary.Extraction.Rate * Weight,
  na.rm = TRUE), by = c("Area.Code", "Element.Code", "Year")]
setnames(wheatStandard.dt, old = "V1", new = "wheatStand")
wheatStandard.df = dcast(wheatStandard.dt, Area.Code + Year ~ Element.Code,
  value.var = "wheatStand")
colnames(wheatStandard.df) = c("Area.Code", "Year", "wheatImport", "wheatExport")
