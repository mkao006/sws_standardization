library(data.table)
library(faoswsAupus)
library(faosws)

GetTestEnvironment(
    ## baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
    baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
    token = "d91934e8-6bf9-4f44-b051-504a18c885fc"
)

aupusParam = getAupusParameter(areaCode = "4", assignGlobal = FALSE)
aupusData = getAupusDataset(aupusParam = aupusParam, assignGlobal = FALSE)
edges = buildEdges(dataList = aupusData)

hs2fcl = fread("~/Documents/Github/sws_standardization/Map_HS2FCL/HS2FCL_Valentina.csv",
               colClasses = rep("character", 7))
hs2fcl[, 'Last update: 12 May 2014' := NULL]
setnames(hs2fcl, old = c("HS 2007 code", "HS 2007 title", "HS 2012 code",
                         "HS 2012 title", "FCL Item code", "FCL Title"),
         new = c("itemCodeHS2007", "itemNameHS2007", "itemCodeHS2012",
                 "itemNameHS2012", "itemCodeFCL", "itemNameFCL"))
## How many unique HS codes per FCL code?
hs2fcl[, length(unique(itemCodeHS2012)), by = "itemCodeFCL"][, .N, V1]
hs2fcl[, length(unique(itemCodeHS2012)), by = "itemCodeFCL"][V1 > 10, ]
hs2fcl[itemCodeFCL == "1232", ]
hs2fcl[itemCodeFCL == "1293", ]
hs2fcl[itemCodeFCL == "0623", ]
## How many unique FCL codes per HS code?
hs2fcl[, length(unique(itemCodeFCL)), by = "itemCodeHS2012"][, .N, V1]
hs2fcl[, length(unique(itemCodeFCL)), by = "itemCodeHS2012"][V1 > 15]
hs2fcl[itemCodeHS2012 == "121490", ]
hs2fcl[itemCodeHS2012 == "410120", ]
hs2fcl[itemCodeHS2012 == "410150", ]
hs2fcl[itemCodeHS2012 == "410190", ]