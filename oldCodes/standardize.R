##' Standardize
##' 
##' This function takes a data.table containing calorie and quantity
##' information and standardizes that data based on a provided commodity
##' tree.
##' 
##' @param nodeData A data.table object containing information about the
##' quantities and calories at each node.
##' @param idColname The column name of nodeData which contains the ID for
##' the node of interest.
##' @param quantityColname The column name of nodeData which contains the
##' quantity of the particular node/commodity.
##' @param calorieColname The column name of nodeData containing the calorie
##' information.  NOTE: this should be a multiplier, so you take the quantity
##' column multiplied by this calorie column to get the final calories for that
##' commodity.
##' @param commodityTree A data.tree object as created by constructDataTree
##' which contains the commodity tree structure.  This object should also
##' have an extractionRate and share column (and it will if it is constructed
##' with constructDataTree).
##' 
##' @return A data.tree object containing the quantity and calorie information
##' standardized to all levels.
##' 

standardize = function(nodeData, idColname, quantityColname, calorieColname,
                       commodityTree){

    ## Data Quality Checks
    stopifnot(c(idColname, quantityColname, calorieColname) %in%
                  colnames(nodeData))
    stopifnot(is(nodeData, "data.table"))
    stopifnot(is(commodityTree, "Node"))
    
    ## Place node attributes on commodity tree
    commodityTree$Get
}