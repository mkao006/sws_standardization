library(data.table)
library(igraph)
library(faoswsExtra)
library(FAOSTAT)
source("algorithm.R")

currentYear = 2005
## countryCode = 231
## inputData =
##     readInputFile(treeData = "usa_demo_network.csv",
##                   extractionRateData = "usa_demo_extract_data_full.csv",
##                   inputData = "usa_demo_input_data.csv",
##                   suaData = "usa_demo_sua_data_full.csv",
##                   year = currentYear)

countryCode = 106
inputData =
    readInputFile(treeData = "italy_demo_network.csv",
                  extractionRateData = "italy_demo_extract_data_full.csv",
                  inputData = "italy_demo_input_data.csv",
                  suaData = "italy_demo_sua_data_full.csv",
                  year = currentYear)

standardize = with(inputData,
    computeStandardization(suaData = sua,
                           directWeights = directWeights,
                           element = "141")
    )
         
setnames(standardize, "root", "itemCode")
postProcessConversion =
    data.table(
        read.csv(file = "postProcessWeight.csv",
                 header = FALSE)
        )
setnames(postProcessConversion, old = c("V1", "V2", "V3"),
         new = c("fbsCode", "itemCode", "conversion"))
finalConversion = merge(standardize, postProcessConversion,
    by = "itemCode", all = TRUE, allow.cartesian = TRUE)
standardized =
    finalConversion[, sum(standardizedValue * conversion, na.rm = TRUE),
                    by = "fbsCode"]
setnames(standardized, old = c("fbsCode", "V1"),
         new = c("itemCode", "standardizedValue"))


## load("FBSfoodQuery.RData")
## FBSdisseminate = getFAOtoSYB(query = FBSfoodQuery,
##     outputFormat = "long", yearRange = 2000:2011)

check =
    checkStandardization(disseminatedData =
                         data.table(FBSdisseminate$entity),
                         standardizedData = standardized,
                         currentYear = currentYear,
                         countryCode = countryCode)
check[pctDifference >= 5, list(itemName, disseminatedValue,
          standardizedValue = round(standardizedValue), pctDifference)]

check[pctDifference >= 5, list(itemCode, itemName, disseminatedValue,
          standardizedValue = round(standardizedValue), pctDifference)]

## This is for check
postProcessConversion =
    data.table(
        read.csv(file = "postProcessWeight.csv",
                 header = FALSE)
        )
setnames(postProcessConversion, old = c("V1", "V2", "V3"),
         new = c("fbsCode", "itemCode", "conversion"))
final = merge(standardize, postProcessConversion,
    by = "itemCode", all = TRUE, allow.cartesian = TRUE)
final[, standardizedValue := round(standardizedValue)]
final[fbsCode == 2513, ]
final[is.na(fbsCode) & !is.na(standardizedValue), ]


## USA
## 2005
## There is still difference in the disseminated Barley (2513),
## however, since item (48) is non-zero the difference became smaller.
## 
## 2009
## The disseminated Barley (2513) had weights calculated wrongly.
## 
## 2010
## The disseminated Barley (2513) had weights calculated wrongly.
## For fruits (2625), FRUIT PR NES (623) seems to be excluded
## For citurs other (2614), citrus juice conc (514) seems to be excluded
## 2011
## The disseminated Barley (2513) had weights calculated wrongly.
## For citurs other (2614), citrus juice conc (514) seems to be excluded



## China41
## 2007
## Cereal Prep Nes (113) should be excluded from cereals, other (2520)
## FLOUR RT TUB (151) should be excluded from roots, other (2534)
## 2009
## Cereal Prep Nes (113) should be excluded from cereals, other (2520)
## FLOUR RT TUB (151) should be excluded from roots, other (2534)
## 2011
## Cereal Prep Nes (113) should be excluded from cereals, other (2520)
## FLOUR RT TUB (151) should be excluded from roots, other (2534)
