########################################################################
## Title: Function for checking pre-standardization
## Date: 2013-06-17
########################################################################


checkStandard = function(countryCode, countryCodeCol = "Area.Code",
  year, yearCol = "Year", itemCodeCol = "Item.Code", elementCode = c(5611, 5911),
  elementCodeCol = "Element.Code", finalData, checkData){
  finalData.dt = data.table(finalData)
  setnames(finalData.dt, old = c(countryCodeCol, yearCol, itemCodeCol,
                           elementCodeCol),
           new = c("countryCodeCol", "yearCol", "itemCodeCol", "elementCodeCol"))
  print(finalData.dt[countryCodeCol == countryCode &
                     yearCol == year &
                     elementCodeCol == elementCode,
                     list(itemCodeCol, Trade, Primary.Extraction.Rate, Weight)])
  checkData.dt = data.table(checkData)
  setnames(checkData.dt, old = c(countryCodeCol, yearCol, elementCodeCol),
           new = c("countryCodeCol", "yearCol", "elementCodeCol"))
  print(subset(checkData.dt, countryCodeCol == countryCode &
               yearCol == year & elementCodeCol == elementCode))
}


checkStandardIm = function(countryCode, primaryCode, year, finalData,
    checkData){
    tmp = checkData[Area.Code == countryCode & Year == year,
        list(Area.Code, Year, wheatImport, wheatProductImp)]
    tmp2 = data.table(melt(tmp, id.vars = c("Area.Code", "Year")))
    ## tmp2[, variable :=
    ##      as.numeric(ifelse(variable == "wheatImport", 5611, 5911))]
    ## print(tmp2)
    tmp3 = finalData[Area.Code == countryCode & Year == year &
                    Primary == primaryCode & Element.Code == 5611, ]
    ## print(tmp3)
    merge(tmp2, tmp3, allow.cartesian = TRUE, 
          by = c("Area.Code", "Year"))
}


checkStandardEx = function(countryCode, primaryCode, year, finalData,
    checkData){
    tmp = checkData[Area.Code == countryCode & Year == year,
        list(Area.Code, Year, wheatExport, wheatProductExp)]
    tmp2 = data.table(melt(tmp, id.vars = c("Area.Code", "Year")))
    ## tmp2[, variable :=
    ##      as.numeric(ifelse(variable == "wheatImport", 5611, 5911))]
    ## print(tmp2)
    tmp3 = finalData[Area.Code == countryCode & Year == year &
                    Primary == primaryCode & Element.Code == 5911, ]
    ## print(tmp3)
    merge(tmp2, tmp3, allow.cartesian = TRUE, 
          by = c("Area.Code", "Year"))
}
