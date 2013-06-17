########################################################################
## Title: Script to check the pre-standardization results
## Date: 2013-06-17
########################################################################

source("wheatPreStandard.R")
source("checkStandard.R")

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

wheatCheck.dt = data.table(arrange(merge(wheatAll.df, wheatFBS.df,
  by = c("Area.Code", "Year"), all = TRUE), Area.Code, Year))
wheatCheck.dt[, impDiff := 100 * (round(wheatImport/1000) - wheatProductImp)/
         round(wheatImport/1000)]
wheatCheck.dt[, exDiff := 100 * (round(wheatExport/1000) - wheatProductExp)/
         round(wheatExport/1000)]
  
par(mfrow = c(2, 1))
## hist(wheatCheck.dt$impDiff[abs(wheatCheck.dt$impDiff) < 300], breaks = 300)
hist(wheatCheck.dt$impDiff, breaks = 300)
abline(v = c(-5, 5), lty = 2, col = "red")
hist(wheatCheck.dt$exDiff, breaks = 300)
abline(v = c(-5, 5), lty = 2, col = "red")

table(abs(wheatCheck.dt$impDiff) < 5, useNA = "always")
table(abs(wheatCheck.dt$exDiff) < 5, useNA = "always")

## Find deviation which are greater than 10% the value is not small
arrange(wheatCheck.dt[abs(impDiff) >= 10,
                 list(Area.Code, Year, wheatImport, wheatProductImp, impDiff)],
        abs(impDiff))


arrange(wheatCheck.dt[abs(exDiff) >= 10,
                 list(Area.Code, Year, wheatExport, wheatProductExp, exDiff)],
        abs(exDiff))

## This entry for Congo seems difficult to debug, the difference in
## the pre-standardization result and the FBS appears larger than the
## data.



## NOTE (Michael): It appears that the difference simply come from
##                 breakfast cereals (41). This is the main counter
##                 case, since the breakfast cereal is greater than
##                 the FBS result already.
checkStandard(countryCode = 100, year = 2002, finalData = wheatFinal.dt,
          checkData = wheatCheck.dt, elementCode = 61)


## NOTE (Michael): This entry (Congo) shows that the FBS is much
##                 greater than the pre-standardization and the data
##                 does not seem to support it.
checkStandard(countryCode = 46, year = 2005, finalData = wheatFinal.dt,
          checkData = wheatCheck.dt, elementCode = 61)


## NOTE (Michael): Seems the shortage is from Malt barley (49) and
##                 beer barley (51), but these two shold not be
##                 standardized into wheat and product (2511) but
##                 barley and products (2513)
checkStandard(countryCode = 147, year = 2003, finalData = wheatFinal.dt,
          checkData = wheatCheck.dt, elementCode = 61)

## NOTE (Michael) : Probably malt and beer of barley as well.
checkStandard(countryCode = 185, year = 2002, finalData = wheatFinal.dt,
          checkData = wheatCheck.dt, elementCode = 61)

checkStandard(countryCode = 185, year = 2003, finalData = wheatFinal.dt,
          checkData = wheatCheck.dt, elementCode = 61)
