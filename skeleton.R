########################################################################
## Title: This is a skeleton code for computing the standardiztation
##        of the trade of SUA.
## Date: 2013-01-18
########################################################################

## Explore the tree
itemTree.df = read.csv(file = "item_tree.csv", header = TRUE,
    stringsAsFactors = FALSE)


## Inc._Tot: Whether to include in the total. 43 items are discarded.
table(itemTree.df[, "Inc._Tot"])

## Agg_com
table(itemTree.df[, "Agg_Com"])

## Weight: Represents whether the item to be used in the aggregation,
##         if w=0 then the item is not aggregated due to the item
##         being produced simultaneously with another item.
table(itemTree.df[, "Weight"])

## Target:
table(itemTree.df[, "Target"])


table(itemTree.df[, "FBS_C"])

itemTree2.df = read.csv(file = "item_tree_standard.csv", head = TRUE,
    stringsAsFactors = FALSE)

## 96 FBS target
FBStarget = unique(itemTree2.df$Tar_Cod)

## 710
subtree.df = subset(itemTree.df, !(Item_C %in% FBStarget))

## 667
st2.df = subset(subtree.df, Inc._Tot == 1)


## Psuedo code
## (1) Merge the SUA data, conversion, relation file and maybe share together
## (2) Set FBS target (e.g. Wheat) to have conversion factor 1
## (3) Subset the entries which are not required based on the relation file.
## (4) Compute the equivalen FBS target quantity by multiplying the
##     item quantity with conversion.
## (5) ddply the data to obtain the aggregates


library(FAOSTAT)


## France
## ---------------------------------------------------------------------

## Set the conversion table, need to obtain this from Amanda
convTabFR = data.frame(itemCode = c(15, 16, 18, 20, 22, 23, 41, 110),
                       name = c("WHEAT", "FLOUR_WHEAT", "MACARONI", "BREAD",
                       "PASTRY", "WHEAT_STARCH","BREAKF_CERLS", "WAFERS"),
                       convFactor = c(1, 1.298701, 1.298701, 1.298701, 1.298701,
                                      1.52, 1.25, 1.3889))

## save the query
exQuery = list(name = c("WHEAT", "FLOUR_WHEAT", "MACARONI", "BREAD", "PASTRY",
               "WHEAT_STARCH","BREAKF_CERLS", "WAFERS"),
               domainCode = rep("TP", 8),
               itemCode = c(15, 16, 18, 20, 22, 23, 41, 110),
               elementCode = rep(5910, 8))

imQuery = list(name = c("WHEAT", "FLOUR_WHEAT", "MACARONI", "BREAD", "PASTRY",
               "WHEAT_STARCH","BREAKF_CERLS", "WAFERS"),
               domainCode = rep("TP", 8),
               itemCode = c(15, 16, 18, 20, 22, 23, 41, 110),
               elementCode = rep(5610, 8))

qcQuery = list(name = "WHEAT", domainCode = "QC", itemCode = 15,
    elementCode = 5510)

## Download the data
exRaw.lst = getFAOtoSYB(query = exQuery, outputFormat = "long")
imRaw.lst = getFAOtoSYB(query = imQuery, outputFormat = "long")
qcRaw.lst = getFAOtoSYB(query = qcQuery, outputFormat = "long")

## Subset and melt the data
exFR.df = subset(exRaw.lst$entity, FAOST_CODE == 68)
exFR.df$Type = "Export"

imFR.df = subset(imRaw.lst$entity, FAOST_CODE == 68)
imFR.df$Type = "Import"

qcFR.df = subset(qcRaw.lst$entity, FAOST_CODE == 68)
qcFR.df$Type = "Production"

## bind the data and compute the standardised value
rawFR.df = rbind(exFR.df, imFR.df, qcFR.df)
finalFR.df = merge(rawFR.df, convTabFR, all.x = TRUE)
finalFR.df$stand = with(finalFR.df, Value * convFactor)

## Compute the standardization.
## NOTE(Michael): The difference is verified as the unavailable
##                information on WHEAT, STARCH.
## std.df = ddply(.data = finalFR.df[finalFR.df$itemCode != 41, ],
##                .variables = .(Year, Type),
##                .fun = function(x) sum(x$stand, na.rm = TRUE)/1000)
std.df = ddply(.data = finalFR.df[finalFR.df$itemCode != 41, ],
               .variables = .(Year, elementCode),
               .fun = function(x) sum(x$stand, na.rm = TRUE)/1000)
ggplot(data = std.df, aes(x = Year, y = V1)) + geom_line(aes(col = Type))

fbQuery = list(name = c("Production", "Import", "Export"),
               domainCode = rep("FB", 3),
               itemCode = rep(2511, 3),
               elementCode = c(5511, 5611, 5911))

fbRaw.lst = getFAOtoSYB(query = fbQuery, outputFormat = "long")
check = fbRaw.lst$entity[fbRaw.lst$entity$FAOST_CODE == 68, ]



## CHECK(Michael):
##    (1) Why is breakfast cereal deducted?
##    (2) Why is Wheat starch a balancing item? and how do you obtain it?

## NOTE(Michael): The units and meta-data might be a problem
## NOTE(Michael): I think 5610 and 5910 are tonnes while 5611 and 5911
##                are thousand tonnes


## Tunisia
## ---------------------------------------------------------------------

## Set the conversion table
convTabTN = data.frame(itemCode = c(15, 16, 18, 20, 22, 23, 41, 110),
                       name = c("WHEAT", "FLOUR_WHEAT", "MACARONI", "BREAD",
                       "PASTRY", "WHEAT_STARCH","BREAKF_CERLS", "WAFERS"),
                       convFactor = c(1, 1.25, 1.25, 1.04, 1.25, 1.634, 1, 1.3889))

## save the query
exQuery = list(name = c("WHEAT", "FLOUR_WHEAT", "MACARONI", "BREAD", "PASTRY",
               "WHEAT_STARCH","BREAKF_CERLS", "WAFERS"),
               domainCode = rep("TP", 8),
               itemCode = c(15, 16, 18, 20, 22, 23, 41, 110),
               elementCode = rep(5910, 8))

imQuery = list(name = c("WHEAT", "FLOUR_WHEAT", "MACARONI", "BREAD", "PASTRY",
               "WHEAT_STARCH","BREAKF_CERLS", "WAFERS"),
               domainCode = rep("TP", 8),
               itemCode = c(15, 16, 18, 20, 22, 23, 41, 110),
               elementCode = rep(5610, 8))

qcQuery = list(name = "WHEAT", domainCode = "QC", itemCode = 15,
    elementCode = 5510)

## Download the data
exRaw.lst = getFAOtoSYB(query = exQuery)
imRaw.lst = getFAOtoSYB(query = imQuery)
qcRaw.lst = getFAOtoSYB(query = qcQuery)

## NOTE (Michael): Area sown may not be public but is probably not
##                 needed given we are only working with production
##                 and trade.

## Subset and melt the data
exTN.df = subset(exRaw.lst$entity, FAOST_CODE == 222)
mexTN.df = melt(exTN.df, id.vars = c("FAOST_CODE", "Year"))
mexTN.df$Type = "Export"

imTN.df = subset(imRaw.lst$entity, FAOST_CODE == 222)
mimTN.df = melt(imTN.df, id.vars = c("FAOST_CODE", "Year"))
mimTN.df$Type = "Import"

qcTN.df = subset(qcRaw.lst$entity, FAOST_CODE == 222)
mqcTN.df = melt(qcTN.df, id.vars = c("FAOST_CODE", "Year"))
mqcTN.df$Type = "Production"

## bind the data and compute the standardised value
rawTN.df = rbind(mexTN.df, mimTN.df, mqcTN.df)
finalTN.df = merge(rawTN.df, convTabTN, by.x = "variable", by.y = "name",
    all.x = TRUE)
finalTN.df$stand = with(finalTN.df, value * convFactor)

## Compute the standardization.
## NOTE(Michael): The difference is verified as the unavailable
##                information on WHEAT, STARCH.
std.df = ddply(.data = finalTN.df[finalTN.df$itemCode != 41, ],
               .variables = .(Year, Type),
               .fun = function(x) sum(x$stand, na.rm = TRUE)/1000)
ggplot(data = std.df, aes(x = Year, y = V1)) + geom_line(aes(col = Type))





## This is to extract data for conversion SUA estimation
## ---------------------------------------------------------------------

ieQuery = list(name = c("milletImport", "milletExport"),
             domainCode = c("TP", "TP"),
             itemCode = c(80, 80),
             elementCode = c(5610, 5910))

ie.lst = getFAOtoSYB(query = ieQuery)
ieNiger.df = subset(ie.lst$entity, FAOST_CODE == 158)

prodQuery = list(name = "production", domainCode = "QC", itemCode = 79,
              elementCode = 5510)
prod.lst = getFAOtoSYB(query = prodQuery)
prodNiger.df = subset(prod.lst$entity, FAOST_CODE == 158)


bieQuery = list(name = c("branImport", "branExport"),
             domainCode = c("TP", "TP"),
             itemCode = c(81, 81),
             elementCode = c(5610, 5910))
bie.lst = getFAOtoSYB(query = bieQuery)
bieNiger.df = subset(bie.lst$entity, FAOST_CODE == 158)


mer.df[mer.df$country_codes == 158 & mer.df$Year == 1981 &
       grepl("Millet", mer.df$products), ]


## Can not find production of millet flour or bran.


## Inspect the item tree, relationship and extraction rate
## ---------------------------------------------------------------------

er.df = read.csv(file = "extraction_rate.csv", header = TRUE,
    stringsAsFactors = FALSE)
mer.df = melt(er.df, id.vars = c("countries", "country_codes",
                     "products", "product_codes", "elements", "ele_codes"))
colnames(mer.df)[colnames(mer.df) == "variable"] = "Year"
colnames(mer.df)[colnames(mer.df) == "value"] = "conversion_rate"
mer.df$extraction_rate = ifelse(100000/mer.df$conversion_rate == Inf, 0,
    100000/mer.df$conversion_rate)
mer.df$Year = as.numeric(gsub("[^0-9]", "", mer.df$Year))
colnames(mer.df) = c("Country", "FAOST_CODE", "itemName", "itemCode",
        "elementName", "elementCode", "Year", "conversion_rate", "extraction_rate")



itemTree.df = read.csv(file = "item_tree_standard.csv", header = TRUE,
    stringsAsFactors = FALSE)

colnames(itemTree.df)[1] = "product_codes"

itLong.df = merge(itemTree.df, mer.df, all = TRUE)

## Need to check the difference between the global and specific
## extraction rate

itFinal.df = subset(itLong.df, select = c("product_codes", "FirstNam",
                               "Tar_Cod", "TargNAm",
                               "countries", "country_codes", "Year", "value"))
colnames(itFinal.df) = c("deriv_code", "deriv_name", "target_code", "target_name",
        "country_name", "FAOST_CODE", "Year", "specER")


## Bran of wheat has extraction rate but is not in the item tree. This
## is because it has zero weight. This is true for all the products,
## if it has "w=0" in the weights, then it does not appear in the
## item_tree_standard.csv


## Some products has extraction rate greater than one, this is because
## other ingredients such as sugar and water also goes into the
## productin of the derivatives. It is important to check the calorie
## conversion factor of these products.
itemTree.df[grep("TOMATO", itemTree.df[, "FirstNam"]), ]

## NOTE(Michael): There are items in the production domain which
## requries aggregation.
##
## Crops_Palm kernels_Production(tonnes)
## Crops_Palm oil_Production(tonnes)
## Crops_Kapokseed in Shell_Production(tonnes)
## Crops_Seed cotton_Production(tonnes)
## Crops_Cottonseed_Production(tonnes)
## Crops_Cotton lint_Production(tonnes)
## Crops_Kapok Fibre_Production(tonnes)

load("allTrade")
trade.df = trade.lst$entity
trade.df$deriv_code = trade.df$itemCode

test = merge(itFinal.df, trade.df, all.x = TRUE)



## NOTE (Michael): We can aggregate everything into the FBS by
##                 multiplying the statistics downloaded from SUA
##                 multiply by the incTot and weight and conversion
##                 factor then sum by FBS group.

## NOTE (Michael): FAOST_CODE = 298 is Test area in extraction rate
##                 table.

itf.df = read.csv(file = "item_tree_final.csv", header = TRUE,
    stringsAsFactors = FALSE)

finalTree.df = merge(mer.df, itf.df, all = TRUE)

## NOTE(Michael): These are entries in the extraction tables but are
##                not in the tree table. They appears to be aggregated
##                item but will eed to be further clearified.
aggItemCode = c(1701, 1710, 1716, 1719, 1725, 1734, 1743, 1761, 1772, 1782,
                1785, 1786, 1787, 1788, 1789, 1790, 1791, 1793, 1815, 1740, 1794)

unique(mer.df[mer.df$itemCode %in% aggItemCode, "itemName"])

## NOTE (Michael):There are many items in the tree which does not have
##                an entry in the extraction table.

## Take the tree to be included, 43 have incTot == 0 and are totally
## discarded from the exercise.
included.df = itf.df[itf.df$incTot == 1, ]

## Codes which are found to have no extraction rate
tmp = unique(included.df$itemCode)[!unique(included.df$itemCode) %in%
                                   unique(mer.df$itemCode)]

## NOTE(Michael): 451 products does not have extraction rate
noExtractionRate.df = itf.df[itf.df$itemCode %in% tmp, ]

## NOTE (Michael): 63 were no targets/primary products.
notTarget.df = noExtractionRate.df[noExtractionRate.df$itemCode !=
                                   noExtractionRate.df$targetCode, ]

## NOTE (Michael): 451 - 63 = 388 - 2(Cotton duplicates) = 386 are the
##                 auto targets in Rafik's document.

## NOTE(Michael): The tree seems to be ready, but I don't think the
##                tree is complete. Breakfast cereal (itemCode = 41)
##                is not aggregated in France but this information is
##                not incorporated in the tree.


erTree.df = merge(mer.df, itf.df[, -grep("itemName", colnames(itf.df))],
    all = TRUE)


## Finish this function for presentation when you know where the data are.
SUAskeleton = function(){



## Use code == 2511
## Find the derive codes
wd.df = subset(itf.df, fbsCode == 2511)

wder.df = subset(mer.df, itemCode %in% wd.df$itemCode)

subTree.df = subset(erTree.df, fbsCode == 2511)



derivItem = unique(wder.df$itemCode)
n.deriveItem = length(derivItem)
prod.df = getFAOtoSYB(domainCode = rep("QC", n.deriveItem),
                      itemCode = derivItem,
                      elementCode = rep(5510, n.deriveItem))$entity


tp.df = getFAOtoSYB(domainCode = rep("TP", 2 * n.deriveItem),
                    itemCode = rep(derivItem, 2),
                    elementCode = rep(c(5610, 5910), each = n.deriveItem,
                    format = "long"))$entity
}



u.itemCode = unique(mer.df$itemCode)
u.itemName = unique(mer.df$itemName)
n.item = length(u.itemCode)

pdf(file = "extraction_rate_hist.pdf")
for(i in 1:n.item){
print(
    ggplot(data = mer.df[mer.df$Year %in% c(1975, 1985, 1995, 2005) &
                         mer.df$itemCode == u.itemCode[i], ],
           aes(x = extraction_rate)) + geom_histogram() +
        facet_wrap(~Year, ncol = 2) +
        xlab(paste("extraction rate of", u.itemName[i], sep = ": "))
    )
}
graphics.off()
system("open extraction_rate_hist.pdf")

library(network)
tmp.net = network(na.omit(unique(itFinal.df[, c("deriv_name", "target_name")])))
plot(tmp.net, displaylabels = TRUE, label.cex = 0.5)

pdf(file = "item_tree_network.pdf", width = 50, height = 50)
plot(tmp.net, displaylabels = TRUE, label.cex = 2, mode = "kamadakawai")
graphics.off()
system("open item_tree_network.pdf")


## Try read in the SUA data
test = read.csv(file = "sua_2008.csv", header = TRUE, stringsAsFactors = FALSE)
test = data.table(test)
var_names = grep("[^0-9]{4}", colnames(test), value = TRUE)
year_names = grep("[0-9]{4}", colnames(test), value = TRUE)

test2 = rbind(test[, c(var_names, year_names[1]), with = FALSE],
              test[, c(var_names, year_names[1]), with = FALSE])

test2 = rbind(test2, test[, c(var_names, year_names[1]), with = FALSE])
test2 = data.table(test2)

test2 = split(test, test$country_codes)
foo = function(x, id.vars){
    x = data.table(melt(x, id.vars = id.vars))
    setnames(x, "variable", "Year")
    x$Year = as.numeric(gsub("[^0-9]", "", x$Year))
    x
}
test3 = lapply(test2, FUN = foo, id.vars = var_names)

test4 = test3[[1]]

setnames(test4, old = colnames(test4),
         new = c("FAOST_CODE", "Country", "itemCode", "itemName", "elementCode",
        "elementName", "unit", "Year", "Value"))
test5 = subset(test4, subset = elementCode %in% c(51, 61, 91))
mer.df = data.table(mer.df)
test6 = merge(test5[, list(FAOST_CODE, Country, itemCode, itemName, elementCode,
                           elementName, Year, Value)],
              mer.df[, list(FAOST_CODE, itemCode, Year, extraction_rate)],
    by = c("FAOST_CODE", "itemCode", "Year"))

test7 = test6[, sum(Value * extraction_rate, na.rm = TRUE),
    by = list(itemCode, Year, elementCode)]




## NOTE (Michael): The following solution does not seem to work well,
##                 maybe use reduce to do the work.
test = data.table(test)
year_name = grep("[0-9]{4}", colnames(test), value = TRUE)
test2 = test[, c(grep("[^0-9]{4}", colnames(test), value = TRUE), year_name[1]),
    with = FALSE]
setnames(test2, year_name[1], "Value")
test2$Year = as.numeric(gsub("[^0-9]", "", year_name[1]))

for(i in 2:length(year_name)){
    tmp = test[, c(grep("[^0-9]{4}", colnames(test), value = TRUE), year_name[i]),
        with = FALSE]
    setnames(tmp, year_name[i], "Value")
    tmp$Year = as.numeric(gsub("[^0-9]", "", year_name[i]))
    test2 = rbind(test2, tmp)
}



tmp = test[, c(grep("[^0-9]{4}", colnames(test), value = TRUE), "X")]





test = data.table(matrix(rnorm(1000000 * 60), nc = 60))
test$index = rep(1:10000, each = 100)
test$Year = rep(1901:2000, each = 1000)
setkeyv(test, c("index", "Year"))

system.time(
    test[, myMean := mean(V1), by = list(index, Year)]
    )


mer.dt = data.table(mer.df)
setkeyv(mer.dt, c("Country", "Year"))

system.time(
    mer.dt[, mySum := sum(conversion_rate + extraction_rate),
           by = list(Country, Year)]
    )


########################################################################
## Merge the 2 old trees to form one comprehensive tree
########################################################################

itemTree1.df = read.csv(file = "item_tree.csv", header = TRUE,
    stringsAsFactors = FALSE)

itemTree2.df = read.csv(file = "item_tree_standard.csv", header = TRUE,
    stringsAsFactors = FALSE)


colnames(itemTree1.df) = c("itemCode", "itemName", "incTot", "aggCom", "weight",
        "target", "fbsCode", "fbsName", "convType")
colnames(itemTree2.df) = c("itemCode", "itemName", "targetCode", "targetName",
        "baseExtraction", "fbsCode", "fbsName")

test = merge(itemTree1.df[, -c(7:8)], itemTree2.df, all = TRUE)


test = read.csv(file = "item_tree_final.csv", header = TRUE,
    stringsAsFactors = FALSE)


test[test$incTot == 1 & test$target == "Target", "targetCode"] =
    test[test$incTot == 1 & test$target == "Target", "itemCode"]
test[test$incTot == 1 & test$target == "Target", "targetName"] =
    test[test$incTot == 1 & test$target == "Target", "itemName"]


tmp = unique(test[, c("targetCode", "fbsCode", "fbsName")])
tmp = na.omit(tmp)

colnames(tmp) = c("targetCode", "newFbsCode", "newFbsName")


test2 = merge(tmp, test, all = TRUE)
test3 = test2[, c("itemCode", "itemName", "incTot", "aggCom", "weight", "target",
                  "convType", "targetCode", "targetName", "baseExtraction",
                  "newFbsCode", "newFbsName")]
colnames(test3)[11:12] = c("fbsCode", "fbsName")

test3 = arrange(test3, itemCode)
test3$weight = ifelse(test3$weight == "w=0", 0, 1)

