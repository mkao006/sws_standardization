##' Map Commodity Tree
##' 
##' This function maps the oldCommodityTree (under one commodity code system)
##' into a new commodity tree.  It does this by first translating all parents
##' and children in the tree to their new elements.  This may create duplicated
##' edges, and in those cases the extraction rates and shares are averaged.
##' 
##' @param commodityTree A data.table containing the edge structure of the
##' commodity tree (specified by parent/child columns) and the extraction
##' rates and shares.  Typically, this is produced from getOldCommodityTree.
##' @param commodityMap A data.table containing the mapping from the old
##' commodity codes to the new commodity codes.  Currently, no other
##' information from this table will be utilized.
##' @param oldColname The column name of commodityMap which contains the IDs
##' for the old commodity codes.
##' @param newColname The column name of commodityMap which contains the IDs
##' for the new commodity codes.
##' @param parentColname The column name of commodityTree which contains the
##' ID for the parent commodity.
##' @param childColname The column name of commodityTree which contains the
##' ID for the child commodity.
##' @param extractionColname The column name of commodityTree which contains the
##' extraction rates.  Extraction rates represent the quantity of the child that
##' can be produced from 1 unit of the parent.  Typically, though, they are
##' expressed in 10,000's: i.e. if the extraction rate is 8000, 1 parent unit
##' can be processed into 1 * 8000/10000 = 0.8 child units.
##' @param shareColname The column name of commodityTree which contains the
##' shares variable.  This value represents the percent of the parent commodity
##' that is processed into the child.  For example, a share value of 50 means
##' that 300 units of the parent would be processed into 300*50/100 = 150 units
##' of the child.
##' @param byKey Character vector of column names of oldCommodityTree which
##' should be used in the by argument when averaging the final extraction
##' rates.  The new parent and child item codes will always be included, but
##' the user may want to include year, country, etc. if the passed commodity
##' tree has those dimensions.
##' 
##' @return
##' 

mapCommodityTree = function(oldCommodityTree, commodityMap,
                            oldColname, newColname,
                            parentColname = "measuredItemParentFS",
                            childColname = "measuredItemChildFS",
                            extractionColname = "extractionRate",
                            shareColname = "share", byKey = NULL){
    
    ## Data Quality Checks
    stopifnot(is(oldCommodityTree, "data.table"))
    stopifnot(is(commodityMap, "data.table"))
    stopifnot(c(parentColname, childColname, extractionColname,
                shareColname) %in% colnames(oldCommodityTree))
    stopifnot(c(oldColname, newColname) %in% colnames(commodityMap))
    
    ## Convert codes by merging
    ## Make a copy of commodityMap so as to not overwrite colnames
    temporaryMap = copy(commodityMap)

    ## Update the parent to the new code
    setnames(temporaryMap, old = oldColname, new = parentColname)
    temporaryMap[, c(parentColname) := as.character(get(parentColname))]
    oldCommodityTree[, c(parentColname) := as.character(get(parentColname))]
    oldCommodityTree = merge(oldCommodityTree, temporaryMap,
                             by = parentColname, allow.cartesian = TRUE)
    ## Capitalize first letter of newColname to be consistent with camelcase
    newName = gsub("^([[:alpha:]])", "\\U\\1", newColname, perl = TRUE)
    setnames(oldCommodityTree, newColname, paste0("parent", newName))
    ## Update the child to the new code
    setnames(temporaryMap, old = parentColname, new = childColname)
    temporaryMap[, c(childColname) := as.character(get(childColname))]
    oldCommodityTree[, c(childColname) := as.character(get(childColname))]
    oldCommodityTree = merge(oldCommodityTree, temporaryMap,
                             by = childColname, allow.cartesian = TRUE)
    setnames(oldCommodityTree, newColname, paste0("child", newName))
    parentColname = paste0("parent", newName)
    childColname = paste0("child", newName)

    ## Average extraction rates and shares on duplicated edges
    newCommodityTree = oldCommodityTree[, list(mean(get(extractionColname)),
                                               mean(get(shareColname))),
                                    by = c(parentColname, childColname, byKey)]
    setnames(newCommodityTree, old = c("V1", "V2"),
             new = c(extractionColname, shareColname))
    newCommodityTree
}