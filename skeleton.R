########################################################################
## Title: The skeleton code for FBS pre-standardization
## Date: 2013-03-06
########################################################################

## TODO (Michael): Finish checking the data and structure.
## TODO (Michael):

library(data.table)
library(reshape2)

## Load the tree
## ---------------------------------------------------------------------
## TODO (Michael): What is 'convType', and is the extraction rate the
##                 base Extraction rate?

fullTree.dt = data.table(read.csv(file = "item_tree_final.csv", header = TRUE,
    stringsAsFactors = FALSE))

## Load and manipulate the extraction rate data
## ---------------------------------------------------------------------
er.df = read.csv(file = "extraction_rate.csv", header = TRUE,
    stringsAsFactors = FALSE)
mer.df = melt(er.df, id.vars = grep("[^0-9]{4}", colnames(er.df), value = TRUE))
mer.dt = data.table(mer.df)
rm(mer.df)
setnames(mer.dt, old = c("variable", "value"), new = c("year", "extractionRate"))
mer.dt$year = as.numeric(gsub("[^0-9]", "", mer.dt$year))

## Compute the conversion as 1/extraction
mer.dt[, conversionFactor := ifelse(10000/extractionRate == Inf, 0,
                          10000/extractionRate)]
setnames(mer.dt, old = colnames(mer.dt),
         new  = c("countryName", "FAOST_CODE", "itemName", "itemCode",
                  "elementName", "elementCode", "year", "extractionRate",
                  "conversionFactor"))

## Select the subset of the tree that is required
subER.dt = mer.dt[, list(FAOST_CODE, countryName, itemCode, itemName,
                         year, conversionFactor)]

## Merge the extraction rate with the tree.
finalTree.dt = merge(subER.dt,
                     fullTree.dt[, list(itemCode, incTot, weight, target,
                                        convType, targetCode, fbsCode)],
                     by = "itemCode", all = TRUE)

## TODO (Michael): Why do we need two set of aggregation? First from
##                 wheat equivalent (15) then to wheat and products
##                 (2511) while AQ M PREP NS has a 'targetCode' but
##                 not 'fbsCode'.
subset(finalTree.dt, itemCode == targetCode)


## TODO (Michael): Need to understand why some country, and part of
##                 the tree does not have relationship. Some does not
##                 have fbsCode because they either have 'incTot = 0'
##                 or 'weight = 0' which implies they are not mapped
##                 to the FBS. While some are simply aggregates such
##                 as 'Grand Total'.
##
## missTree.dt = subset(finalTree.dt, subset = is.na(FAOST_CODE))
## hackTree.dt = subset(finalTree.dt, subset = !is.na(FAOST_CODE) & !is.na(fbsCode))

## TODO (Michael): Need to include the target commodity such as
##                 'Wheat'.

## NOTE(Michael): I can not find anything on Pizza!




## Read in and manipuate the full SUA data
fullSUA.dt = data.table(read.csv(file = "sua_2008.csv", header = TRUE,
    stringsAsFactors = FALSE))
var_names = grep("[^0-9]{4}", colnames(fullSUA.dt), value = TRUE)
new_var_names = c("FAOST_CODE", "countryName", "itemCode", "itemName",
                  "elementCode", "elementName", "unit")
year_names = grep("[0-9]{4}", colnames(fullSUA.dt), value = TRUE)
setnames(fullSUA.dt, old = var_names, new = new_var_names)
subSUA.dt = subset(fullSUA.dt, subset = elementCode %in% c(51, 61, 91))
names(subSUA.lst) = sapply(subSUA.lst, function(x) unique(x$countryName))

## Split the tree to make the list for mapreduce.
## NOTE (Michael): What country standard should be used?
subSUA.lst = split(subSUA.dt, subSUA.dt$FAOST_CODE)
foo = function(x, id.vars){
    x = data.table(melt(x, id.vars = id.vars))
    setnames(x, "variable", "year")
    x$year = as.numeric(gsub("[^0-9]", "", x$year))
    x
}
subSUA.lst = lapply(subSUA.lst, FUN = foo, id.vars = new_var_names)
## hist(sapply(subSUA.lst, function(x) length(unique(x$itemCode))))


## Function to perform the standardization
preStand = function(raw_data, extraction_data, na.rm = FALSE){
    cat(paste0("Computing standardization for: ", unique(raw_data$countryName),
               "\n"))
    tmp = merge(raw_data, extraction_data,
        by = c("FAOST_CODE", "itemCode", "year"))
    if(NROW(tmp) == 0){
        tmp = data.table()
    } else {
        tmp[, equivValue := sum(value * conversionFactor * incTot * weight,
                         na.rm = na.rm),
            by = list(FAOST_CODE, fbsCode, elementCode, year)]
    }
    tmp
}


## Test on the first country
testSD = preStand(subSUA.lst[[1]], hackTree.dt)


## Compute the whole standardization
foo2 = function(x, y){
    tmp = preStand(raw_data = y, extraction_data = hackTree.dt)
    rbind(x, tmp)
}

system.time(
    {sdFull.dt =
        Reduce(f = foo2, x = subSUA.lst[2:length(subSUA.lst)],
               init = preStand(raw_data = subSUA.lst[[1]],
                               extraction_data = hackTree.dt))
 }
    )


## Test parallel computing
cl = makeCluster(10)
parLapply(cl, 1:15, get("+"), 2)
parSapply(cl, 1:15, get("+"), 2)


parSapply(cl, subSUA.lst[1:2], FUN = preStand, extraction_data = hackTree.dt)
