library(data.table)
library(faosws)

GetTestEnvironment(
    baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
    token = "c1a2cc38-a9e9-4977-ae6e-4eed6a9568e7"
    ## baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
    ## token = "90bb0f92-e345-4401-945d-1e43af801167"
)

extractionRateData = fread("~/Documents/Github/sws_standardization/faoswsStandardization/data/extractionRate2011.csv")
shareData = fread("~/Documents/Github/sws_standardization/faoswsStandardization/data/shares2011.csv")

setnames(extractionRateData,
         old = c("measuredItemFS", "timePointYears",
                 "Value", "Status"),
         new = c("measuredItemChildFS", "timePointYearsSP",
                 "extractionRate", "flagExtractionRate"))
setnames(shareData, old = "Value", new = "share")
commodityTree = merge(shareData, extractionRateData,
                      by = c("geographicAreaFS", "measuredItemChildFS",
                             "timePointYearsSP"))
commodityTree[, c("Geographic Area.x", "Year.x", "Year.y") := NULL, with = FALSE]
setnames(commodityTree, "Geographic Area.y", "geographicAreaName")
setnames(commodityTree, "Item Parent", "itemParentName")
setnames(commodityTree, "Item Child", "itemChildName")
setnames(commodityTree, "Aupus Required", "aupusRequired")
save(commodityTree, file = "~/Documents/Github/sws_standardization/faoswsStandardization/data/commodityTree2011.RData")
load("~/Documents/Github/sws_standardization/faoswsStandardization/data/commodityTree2011.RData")
oldCommodityTree = copy(commodityTree)
commodityMap = fread("~/Documents/Github/sws_standardization/Map_HS2FCL/HS2FCL_Valentina.csv",
                     colClasses = rep("character", 7))
setnames(commodityMap, old = colnames(commodityMap),
         new = c("itemCodeHS2007", "itemNameHS2007", "itemCodeHS2012",
                 "itemNameHS2012", "itemCodeFCL", "itemNameFCL", "bad"))
commodityMap[, bad := NULL]
commodityMap = commodityMap[, list(itemCodeHS2012, itemCodeFCL)]
newColname = "itemCodeHS2012"
oldColname = "itemCodeFCL"


## Check that everything works:
load("/home/josh/Documents/Github/Working/FBS Example/tradeData.RData")
oldCommodityTree[, measuredItemParentFS :=
                      formatC(as.numeric(measuredItemParentFS), width = 4,
                              flag = "0", format = "d")]
oldCommodityTree[, measuredItemChildFS :=
                      formatC(as.numeric(measuredItemChildFS), width = 4,
                              flag = "0", format = "d")]
newCommodityTree = mapCommodityTree(oldCommodityTree = oldCommodityTree,
                                    commodityMap = commodityMap,
                                    oldColname = "itemCodeFCL",
                                    newColname = "itemCodeHS2012")
tradeData[, measuredItemHS := formatC(as.numeric(measuredItemHS), width = 6,
                                      flag = "0", format = "d")]
## Filter trade data to keep it simple (just look at cereals):
tradeData = tradeData[grepl("^100", measuredItemHS), ]
smallCommodityTree = newCommodityTree[grepl("^100", childItemCodeHS2012) |
                                      grepl("^100", parentItemCodeHS2012), ]
tradeData = tradeData[1:10000, ]
tradeData[, calorieRate := 100]

levels = getCommodityLevel(smallCommodityTree, "parentItemCodeHS2012",
                           "childItemCodeHS2012")

standardizedTrade = standardize(nodeData = tradeData,
                                idColname = "measuredItemHS",
                                quantityColname = "Value",
                                calorieRateColname = "calorieRate",
                                commodityTree = newCommodityTree,
                                parentColname = "parentItemCodeHS2012",
                                childColname = "childItemCodeHS2012",
                                extractionColname = "extractionRate",
                                shareColname = "share",
                                targetNodes = levels[level == 0, node])
