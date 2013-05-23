########################################################################
## Title: A function to split the extraction rate data into a list and
##        merge with the commodity tree.
## Date: 2013-05-23
########################################################################

toExtractTree = function(extractData, treeData, splitKey, itemKey = "Item.Code",
  areaCol, yearCol, extractSpecificCol, extractDefaultCol){
  extract.lst = split(x = extractData, f = splitKey)

  extractTree.lst = lapply(X = extract.lst,
    FUN = function(x) merge(treeData, x, all.x = TRUE, allow.cartesian = TRUE,
    by = itemKey))

  extractTreeFull.lst = lapply(X = extractTree.lst, FUN = fillMissingCartesian,
    areaCol = areaCol, yearCol = yearCol, extractSpecificCol = extractSpecificCol,
    extractDefaultCol = extractDefaultCol)
  extractTreeFull.lst
}
