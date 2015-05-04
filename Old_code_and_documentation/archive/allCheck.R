########################################################################
## Title: Script to check the pre-standardization results
## Date: 2013-06-17
########################################################################

source("preStandard.R")
source("checkStandard.R")

## Checks
## ---------------------------------------------------------------------

## Download the FBS data
allFBS.lst = getFAOtoSYB(name = c("wheatProductImp", "wheatProductExp", 
                           "riceProductImp", "riceProductExp"),
  domainCode = rep("FB", 4),
  elementCode = rep(c(5611, 5911), 2),
  itemCode = c(rep(2511, 2), rep(2805, 2)), outputFormat = "long")

allFBS.dt = data.table(subset(allFBS.lst$entity, Year %in% allStandard.dt$Year))
allFBS.dt[, domainCode := NULL]
allFBS.dt[, name := NULL]
setnames(allFBS.dt, old = c("FAOST_CODE", "itemCode", "elementCode", "Value"),
         new = c("Area.Code", "Item.Code", "Element.Code", "FBSvalue"))



allCheck.dt = data.table(merge(allStandard.dt, allFBS.dt,
  by = c("Area.Code", "Item.Code", "Element.Code", "Year")))


## NOTE (Michael): It appears that the difference simply come from
##                 breakfast cereals (41). This is the main counter
##                 case, since the breakfast cereal is greater than
##                 the FBS result already. Tomasz said that breakfast
##                 cereal is not includedin wheat for India.
checkStandard(countryCode = 100, year = 2002, finalData = allFinal.dt,
          checkData = allCheck.dt, elementCode = 5611)


## NOTE (Michael): This entry (Congo) shows that the FBS is much
##                 greater than the pre-standardization and the data
##                 does not seem to support it.
checkStandard(countryCode = 46, year = 2005, finalData = allFinal.dt,
          checkData = allCheck.dt, elementCode = 5611)


## NOTE (Michael): Seems the shortage is from Malt barley (49) and
##                 beer barley (51), but these two shold not be
##                 standardized into wheat and product (2511) but
##                 barley and products (2513)
checkStandard(countryCode = 147, year = 2003, finalData = allFinal.dt,
          checkData = allCheck.dt, elementCode = 5611)

## NOTE (Michael) : Probably malt and beer of barley as well but only
##                  for 2002, 2003 seems fine.
checkStandard(countryCode = 185, year = 2002, finalData = allFinal.dt,
          checkData = allCheck.dt, elementCode = 5611)
checkStandard(countryCode = 185, year = 2003, finalData = allFinal.dt,
          checkData = allCheck.dt, elementCode = 5611)
