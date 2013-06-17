########################################################################
## Title: A function to fill in missing area, year and extraction rate
##        as a result of cartesian join.
## Date: 2013-05-23
########################################################################

fillMissingCartesian = function(Data, areaCol, yearCol, extractSpecificCol,
  extractDefaultCol){
  setnames(Data, old = c(extractSpecificCol, extractDefaultCol, areaCol, yearCol),
           new = c("specific", "default", "areaCol", "yearCol"))
  Data[is.na(specific), specific := as.numeric(default)]
  ## NOTE (Michael): This is associated with github issue #17 where
  ##                 zero extraction rate is causing a problem.
  Data[specific == 0, specific := as.numeric(default)]  
  Data[is.na(areaCol), areaCol := unique(na.omit(Data[, areaCol]))]
  Data[is.na(yearCol), yearCol := unique(na.omit(Data[, yearCol]))]
  setnames(Data, new = c(extractSpecificCol, extractDefaultCol, areaCol, yearCol),
           old = c("specific", "default", "areaCol", "yearCol"))
  Data
}
