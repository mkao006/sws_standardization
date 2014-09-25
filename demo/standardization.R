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

fbsElementName = c("production", "import", "stock_variation", "export",
    "food", "feed", "seed", "waste", "other")
fbsElementSuaCode = c("51", "61" , "71", "91", "141", "101", "111",
    "121", "151")
fbsElementFbsCode = c("5511", "5611", "5072", "5911", "5142", "5521",
    "5527", "5123", "5154")

fbsElements =
    constructFbsElement(fbsElementSuaCode = fbsElementSuaCode,
                        fbsElementFbsCode = fbsElementFbsCode,
                        fbsElementName = fbsElementName)
    

currentYear = 2005
countryCode = 100
inputData =
    getInputFile(conn = conn, countryCode = countryCode,
                 year = currentYear, treeData = "italy_demo_network.csv",
                 postWeightData = "postProcessWeight.csv")

standardized = with(inputData,
    computeStandardization(suaData = sua,
                           directWeights = directWeights,
                           postConversion = postConversion,
                           fbsElementSuaCode = "141",
                           fbsElementFbsCode = "5142")
    )


standardizedFbs =
    computeStandardizedFbs(inputData = inputData,
                           fbsElements = fbsElements)


disseminatedFbs =
    getDisseminatedFbs(countryCode = countryCode,
                       fbsElements = fbsElements)


check = checkStandardization(standardizedFbs, disseminatedFbs,
    currentYear, fbsElementFbsCode)
write.csv(check$fullCheck, file = "check.csv", row.names = FALSE,
          na = "0")

## It's more efficient to just check item by item

check =
    checkStandardization(standardizedData = standardized,
                         currentYear = currentYear,
                         countryCode = countryCode,
                         fbsElementFbsCode = "5142",
                         fbsElementSuaCode = "141")

check[pctDifference >= 5, list(itemCode, itemName, disseminatedValue,
          standardizedValue = round(standardizedValue),
          absDifference)]

## Check item by item
checkItem = 2612
check[pctDifference >= 5, list(itemCode, itemName, -absDifference,
          disseminatedValue)]
inputData$preStandard[fbsCode ==  checkItem & elementCode == 141,
                      list(itemCode, leaves, Value, directWeight,
                           conversion, targetValue)]

## Need to identify the items that were not included in the
## standardization.
## inputData$preStandard[is.na(fbsCode) & !is.na(Value) & Value != 0 &
##                       elementCode == 141]


fbs = computeStandardizedFbs(inputData, suaElements)
