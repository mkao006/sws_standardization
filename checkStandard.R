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
