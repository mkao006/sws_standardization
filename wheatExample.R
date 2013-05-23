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

## Data preperation
## ---------------------------------------------------------------------

## Read the wheat tree
wheatTree.dt = data.table(read.csv(file = "wheatTree.csv", header = TRUE,
  stringsAsFactors = FALSE))
setkeyv(wheatTree.dt, "Item.Code")
wheatTree.graph = graph.data.frame(wheatTree.dt[, list(Item.Name, Parent.Name)])
plot(wheatTree.graph)

## Test whether the tree has multiple or loop edge and find which one
has.multiple(wheatTree.graph)
## E(wheatTree.graph)[which(is.multiple(wheatTree.graph))]

## Read the wheat sua tree data
wheatData.df = read.sua(file = "wheatFullTreeData.csv", stringsAsFactors = FALSE,
  keys = c("Area.Code", "Item.Code", "Element.Code", "Element.Name"))


## Only take data within the specified tree and only take value not symble
wheatProcess.df = subset(wheatData.df, Item.Code %in%
  unique(wheatTree.dt$Item.Code) & Type == "Value")

## Process the full data into trade and extraction 
wheatExtract.dt = data.table(subset(wheatProcess.df, subset = Element.Code == 41,
  select = c(Area.Code, Item.Code, Year, value)))
wheatExtract.dt[, value := as.numeric(value)]
setnames(wheatExtract.dt, old = "value", new = "ExtractRate")
setkeyv(wheatExtract.dt, c("Area.Code", "Item.Code", "Year"))

## Pre-standardization
## ---------------------------------------------------------------------

## split and merge the extraction rate data with the tree
wheatExtractTree.lst = toExtractTree(extractData = wheatExtract.dt,
  treeData = wheatTree.dt,
  splitKey = wheatExtract.dt[, list(Area.Code, Year)],
  itemKey = "Item.Code", areaCol = "Area.Code", yearCol = "Year",
  extractSpecificCol = "ExtractRate",
  extractDefaultCol = "Default.Extraction.Rate")


## Compute the direct extraction rate
wheatDirectExtractTree.lst = vector(mode = "list",
  length = length(wheatExtractTree.lst))
for(i in 1:length(wheatExtractTree.lst)){
  wheatDirectExtractTree.lst[[i]] =
    merge(computeDirectExtractRate(Data = wheatExtractTree.lst[[i]],
                                   child = "Item.Code", parent = "Parent.Code",
                                   ExtractionRate = "ExtractRate"),
          wheatExtractTree.lst[[i]])
}

## If the data has multiple paths, then the unique rate needs to be taken. 
wheatDirectExtractTree.dt = data.table(subset(do.call("rbind",
  wheatDirectExtractTree.lst),
  select = c("Area.Code", "Item.Code", "Item.Name", "Year", "Primary",
    "Primary.Extraction.Rate", "weight")))
  
  

## Take the trade data
wheatTrade.dt = data.table(subset(wheatProcess.df,
  Element.Code %in% c(61, 91)))
wheatTrade.dt[, value := as.numeric(value)]
wheatFinal.dt = merge(wheatTrade.dt, wheatDirectExtractTree.dt, all.x = TRUE,
  by = c("Area.Code", "Item.Code", "Year"))

## Standardize the data
wheatStandard.dt = wheatFinal.dt[, sum(value * Primary.Extraction.Rate * weight,
  na.rm = TRUE), by = c("Area.Code", "Element.Code", "Year")]
setnames(wheatStandard.dt, old = "V1", new = "wheatStand")
wheatStandard.df = dcast(wheatStandard.dt, Area.Code + Year ~ Element.Code,
  value.var = "wheatStand")
colnames(wheatStandard.df) = c("Area.Code", "Year", "wheatImport", "wheatExport")


## Checks
## ---------------------------------------------------------------------

## Download the FBS data
wheatFBS.lst = getFAOtoSYB(name = c("wheatProductProd", "wheatProductImp",
                             "wheatProductExp"), domainCode = rep("FB", 3),
  elementCode = c(5511, 5611, 5911), itemCode = rep(2511, 3))
wheatFBS.df = subset(wheatFBS.lst$entity, Year %in% wheatStandard.df$Year)
colnames(wheatFBS.df)[1] = "Area.Code"

## Download the production of the primary commodity
wheatProd.lst = getFAOtoSYB(name = "wheatProd", domainCode = "QC",
  elementCode = 5510, itemCode = 15)
wheatProd.df = subset(wheatProd.lst$entity, Year %in% wheatStandard.df$Year)
colnames(wheatProd.df)[1] = "Area.Code"

## Merge the data and check
wheatAll.df = merge(wheatStandard.df, wheatProd.df, by = c("Area.Code", "Year"),
  all = TRUE)

check = arrange(merge(wheatAll.df, wheatFBS.df, by = c("Area.Code", "Year"),
  all = TRUE), Area.Code, Year)
check$impDiff = with(check, (wheatImport/1000 - wheatProductImp)/
  (wheatImport/1000))
check$exDiff = with(check, (wheatExport/1000 - wheatProductExp)/
  (wheatExport/1000))


hist(check$impDiff, breaks = 100)
hist(check$exDiff, breaks = 100)

table(check$impDiff < 1, useNA = "always")
table(check$exDiff < 1, useNA = "always")
