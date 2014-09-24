library(RJDBC)
library(reshape2)
library(data.table)
library(igraph)
library(faoswsExtra)
library(FAOSTAT)
source("algorithm.R")

drv = JDBC(driverClass = "oracle.jdbc.driver.OracleDriver",
    classPath = "ojdbc14.jar")
conn = dbConnect(drv, "jdbc:oracle:thin:@lprdbwo1:3310:fstp",
    user = "demo", password = "demo")

currentYear = 2005
countryCode = 106
inputData =
    getInputFile(conn = conn, countryCode = countryCode,
                 year = currentYear, treeData = "italy_demo_network.csv",
                 postWeightData = "postProcessWeight.csv")

standardized = with(inputData,
    computeStandardization(suaData = sua,
                           directWeights = directWeights,
                           postConversion = postConversion,
                           element = "141")
    )
         

check =
    checkStandardization(standardizedData = standardized,
                         currentYear = currentYear,
                         countryCode = countryCode,
                         element = "5142")

check[pctDifference >= 5, list(itemCode, itemName, disseminatedValue,
          standardizedValue = round(standardizedValue), pctDifference)]

## This is for individual fbs item check
final = merge(standardized, inputData$postConversion,
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
