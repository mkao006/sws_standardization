########################################################################
## Title: Test for tree type computation
## Date: 2013-05-14
########################################################################

library(FAOSTAT)
library(igraph)
library(data.table)
source("computeDirectExtractRate.R")
source("read.sua.R")

## Read the wheat tree
wheatTree.dt = data.table(read.csv(file = "wheatTree.csv", header = TRUE,
  stringsAsFactors = FALSE))
setkeyv(wheatTree.dt, "Item.Code")
wheatTree.graph = graph.data.frame(wheatTree.dt[, list(Item.Name, Parent.Name)])
plot(wheatTree.graph)

## Test whether the tree has multiple or loop edge and find which one
has.multiple(wheatTree.graph)
E(wheatTree.graph)[which(is.multiple(wheatTree.graph))]

## Read the wheat sua tree data
wheatData.df = read.sua(file = "wheatFullTreeData.csv", header = TRUE,
  stringsAsFactors = FALSE, keys = c("Area.Code", "Item.Code", "Element.Code"))


## Only take data within the specified tree and only take value not symble
wheatProcess.df = subset(wheatData.df, Item.Code %in%
  unique(wheatTree.dt$Item.Code) & Type == "Value")

## Process the full data into trade and extraction 
wheatExtract.dt = data.table(subset(wheatProcess.df, subset = Element.Code == 41,
  select = c(Area.Code, Item.Code, Year, value)))
wheatExtract.dt[, value := as.numeric(value)]
setnames(wheatExtract.dt, old = "value", new = "ExtractRate")
setkeyv(wheatExtract.dt, c("Area.Code", "Item.Code", "Year"))


## Split the data by country and year then merge with the tree
wheatExtract.lst = split(wheatExtract.dt, wheatExtract.dt[, list(Area.Code, Year)])
wheatExtractTree.lst = lapply(X = wheatExtract.lst,
  FUN = function(x) merge(wheatTree.dt, x, all.x = TRUE, allow.cartesian = TRUE,
    by = "Item.Code"))

## Items which are not in the tree
setdiff(unique(wheatExtract.dt$Item.Code), wheatTree.dt$Item.Code)
unique(FAOmetaTable$itemTable[FAOmetaTable$itemTable$itemCode %in%
                              setdiff(unique(wheatExtract.dt$Item.Code),
                                      wheatTree.dt$Item.Code), "itemName"])

## Fill the extraction with the defaultif missing
fillDefaultExtractRate = function(Data, specific, default){
  setnames(Data, old = c(specific, default), new = c("specific", "default"))
  Data[is.na(specific), specific := as.numeric(default)]
  setnames(Data, new = c(specific, default), old = c("specific", "default"))
}

wheatExtractTree.lst = lapply(X = wheatExtractTree.lst,
  FUN = fillDefaultExtractRate, specific = "ExtractRate",
  default = "Default.Extraction.Rate")


## Fill the missing country as a result of the join
fillCountry = function(Data, country){
  setnames(Data, old = country, new = "country")
  Data[is.na(country), country := unique(na.omit(Data[, country]))]
  setnames(Data, new = country, old = "country")
}

wheatExtractTree.lst = lapply(X = wheatExtractTree.lst,
  FUN = fillCountry, country = "Area.Code")



## Fill the missing year as a result of the join
fillYear = function(Data, year){
  setnames(Data, old = year, new = "year")
  Data[is.na(year), year := unique(na.omit(Data[, year]))]
  setnames(Data, new = year, old = "year")
}

wheatExtractTree.lst = lapply(X = wheatExtractTree.lst,
  FUN = fillYear, year = "Year")




## Compute the direct extraction rate
wheatDirectExtractTree.lst = vector(mode = "list",
  length = length(wheatExtractTree.lst))
for(i in 1:length(wheatExtractTree.lst)){
  wheatDirectExtractTree.lst[[i]] = merge(computeDirectExtractRate(Data =
        data.frame(wheatExtractTree.lst[[i]]),
                                 child = "Item.Code", parent = "Parent.Code",
                                 ExtractionRate = "ExtractRate"),
        wheatExtractTree.lst[[i]])
}

## Take the unique extraction rate table
wheatDirectExtractTree.dt = unique(data.table(subset(do.call("rbind",
  wheatDirectExtractTree.lst),
  select = c("Area.Code", "Item.Code", "Item.Name", "Year", "Primary",
    "Primary.Extraction.Rate", "weight"))))
  
  

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
wheatFBS.lst = getFAOtoSYB(name = c("wheatProductProd", "wheatProductImp",
                             "wheatProductExp"), domainCode = rep("FB", 3),
  elementCode = c(5511, 5611, 5911), itemCode = rep(2511, 3))
wheatFBS.df = subset(wheatFBS.lst$entity, Year %in% wheatStandard.df$Year)
colnames(wheatFBS.df)[1] = "Area.Code"

wheatProd.lst = getFAOtoSYB(name = "wheatProd", domainCode = "QC",
  elementCode = 5510, itemCode = 15)
wheatProd.df = subset(wheatProd.lst$entity, Year %in% wheatStandard.df$Year)
colnames(wheatProd.df)[1] = "Area.Code"

wheatAll.df = merge(wheatStandard.df, wheatProd.df, by = c("Area.Code", "Year"),
  all = TRUE)


check = arrange(merge(wheatAll.df, wheatFBS.df, by = c("Area.Code", "Year"),
  all = TRUE), Area.Code, Year)
check$impDiff = with(check, (wheatImport/1000 - wheatProductImp)/
  (wheatImport/1000))
check$exDiff = with(check, (wheatExport/1000 - wheatProductExp)/
  (wheatExport/1000))
