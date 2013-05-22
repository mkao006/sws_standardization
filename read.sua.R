########################################################################
## Title: Function to read and transform sua data format.
## Date: 2013-05-22
########################################################################


## Function to read and convert SUA data format to long format
read.sua = function(file, keys, ...){
  tmp = read.csv(file = file, ...)
  mtmp = melt(tmp, id.var = keys)
  mtmp$Year = as.integer(gsub("[^0-9]", "", mtmp$variable))
  mtmp$Type = ifelse(grepl("Num", mtmp$variable), "Value", "Symb")
  mtmp$variable = NULL
  mtmp
}
