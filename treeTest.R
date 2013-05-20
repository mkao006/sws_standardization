########################################################################
## Title: Test for tree type computation
## Date: 2013-05-14
########################################################################

library(FAOSTAT)
library(igraph)
library(data.table)

## NOTE (Michael): Need to use the average path rather than the
##                 shortest path since there can be multiple path.
##
## NOTE (Michael): The function assues a single primary product
##

computeDirectExtractRate = function(Data, child, parent, ExtractionRate){
  k = max(Data[, ExtractionRate], na.rm = TRUE)/10000 + 1
  Data$cf = (k * 10000)/Data[, ExtractionRate]
  tmp.graph = graph.data.frame(Data[, c(child, parent)], directed = TRUE)
  primary = names(which(degree(tmp.graph, mode = "out") == 0))
  wldist = shortest.paths(graph = tmp.graph,
    to = primary, weights = log(Data[, "cf"]), algorithm = "johnson")
  dist = shortest.paths(graph = tmp.graph, to = primary, algorithm = "johnson")
  wdist = exp(wldist - dist * log(k))
  finite = which(is.finite(wdist))
  tmp = data.frame(as.numeric(rownames(wdist)[finite]),
    as.numeric(rep(primary, length(finite))),
    wdist[finite], stringsAsFactors = FALSE)
  colnames(tmp) = c(child, "Primary", "Primary.Extraction.Rate")
  tmp
}

## Read the wheat tree
wheatTree.dt = data.table(read.csv(file = "wheatTree.csv", header = TRUE,
  stringsAsFactors = FALSE))
wheatTree.dt = wheatTree.dt[Item.Code != 15, ]
setkeyv(wheatTree.dt, "Item.Code")
wheatTree.graph = graph.data.frame(wheatTree.dt[, list(Item.Name, Parent.Name)])
plot(wheatTree.graph)


## ## Read the whole tree
## fullTree.dt = data.table(read.csv(file = "item_tree_raw.csv", header = TRUE,
##   stringsAsFactors = FALSE))
## setkeyv(fullTree.dt, "FirstCod")
## fullTree.graph = graph.data.frame(fullTree.dt[, list(FirstNam, TargNAm)])
## plot(fullTree.graph)

## Test whether the tree has multiple or loop edge and find which one
has.multiple(wheatTree.graph)
E(wheatTree.graph)[which(is.multiple(wheatTree.graph))]

## Read the wheat sua tree data

## Function to read and convert SUA data format to long format
read.sua = function(file, keys, ...){
  tmp = read.csv(file = file, ...)
  mtmp = melt(tmp, id.var = keys)
  mtmp$Year = as.integer(gsub("[^0-9]", "", mtmp$variable))
  mtmp$Type = ifelse(grepl("Num", mtmp$variable), "Value", "Symb")
  mtmp$variable = NULL
  mtmp
}

wheatData.df = read.sua(file = "wheatFullTreeData.csv", header = TRUE,
  stringsAsFactors = FALSE, keys = c("Area.Code", "Item.Code", "Element.Code"))


## Only take data within the specified tree and only take value not symble
wheatProcess.df = subset(wheatData.df, Item.Code %in%
  c(unique(wheatTree.dt$Item.Code)) & Type == "Value")

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

## Fill in Extraction rate when not available


## NOTE (Michael): This is for data.frame
## fillDefaultExtractRate = function(Data, specific, default){
##   ind = which(is.na(Data[, specific]))
##   Data[ind, specific] = Data[ind, default]
##   Data
## }


## Fill the extraction with the defailt if missing
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


wheatStandard.dt = wheatFinal.dt[, sum(value * Primary.Extraction.Rate * weight,
  na.rm = TRUE), by = c("Area.Code", "Year")]
setnames(wheatStandard.dt, old = "V1", new = "wheatStand")


## Check
wheatFBS.lst = getFAOtoSYB(name = c("wheatProductProd", "wheatProductImp",
                             "wheatProductExp"), domainCode = rep("FB", 3),
  elementCode = c(5511, 5611, 5911), itemCode = rep(2511, 3))
wheatFBS.df = wheatFBS.lst$entity
colnames(wheatFBS.df)[1] = "Area.Code"

wheatProd.lst = getFAOtoSYB(name = "wheatProd", domainCode = "QC",
  elementCode = 5510, itemCode = 15)
wheatProd.df = wheatProd.lst$entity
colnames(wheatProd.df)[1] = "Area.Code"

wheatAll.df = merge(wheatStandard.dt, wheatProd.df, by = c("Area.Code", "Year"))
wheatAll.df$wheatStandProd = with(wheatAll.df, wheatStand + wheatProd)

test = merge(wheatAll.df, wheatFBS.df, by = c("Area.Code", "Year"))
test$diff = with(test, (wheatStand/1000 - wheatProductImp - wheatProductExp)/
  (wheatStand/1000))
