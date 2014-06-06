########################################################################
## Title: A function to split the extraction rate data into a list and
##        merge with the commodity tree.
## Date: 2013-05-23
########################################################################

toExtractTree = function(extractData, treeData, splitKey,
    itemKey = "Item.Code", areaCol, yearCol, extractSpecificCol,
    extractDefaultCol){
    extract.lst = split(x = extractData, f = splitKey)

    ## Merge country specific rate data to the default rate and
    ## item to FBS relationship network.
    extractTree.lst = lapply(X = extract.lst,
        FUN = function(x) merge(treeData, x, all.x = TRUE,
            allow.cartesian = TRUE, by = itemKey))

    ## Fill in missing values as a result of cartesian join.
    extractTreeFull.lst = lapply(X = extractTree.lst,
        FUN = fillMissingCartesian, areaCol = areaCol,
        yearCol = yearCol, extractSpecificCol = extractSpecificCol,
        extractDefaultCol = extractDefaultCol)
    extractTreeFull.lst
}
