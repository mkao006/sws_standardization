##' Standardize FAO data
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
##' @param calorieRateColname The column name of nodeData containing the
##' calorie information.  NOTE: this should be a multiplier, so you take the
##' quantity column multiplied by this calorie column to get the final calories
##' for that commodity.
##' @param commodityTree A data.table with parent and child node IDs
##' (corresponding to the IDs in nodes) which specify the commodity tree
##' structure.  Additionally, there should be a column with extraction rate
##' data and a column with shares data.
##' @param parentColname The column name of commodityTree which contains the ID
##' of the parent node.
##' @param childColname The column name of commodityTree which contains the ID
##' of the child node.
##' @param extractionColname The column name of commodityTree which contains
##' the extraction rate data.
##' @param shareColname The column name of commodityTree which contains the
##' share data.
##' @param byProductColname The column name of the "by-product" flag column in
##' commodityTree.  This variable should be a logical vector indicating which
##' is always TRUE unless an item's quantity should not be standardized (but
##' it's calories should).  For example, wheat is processed to flour, bran, and
##' germ with extraction rates of 0.85, 0.12, and 0.03 (say).  If we have
##' 850 kg of flour, 120 kg of bran and 30 kg of germ and try to standardize
##' all the quantites, we'll overcount by a factor of 3: (850/0.85 + 120/0.12 +
##' 30/0.03 = 3000, not 1000).  Thus, this flag allows us to specify which
##' commodities should be ignored in the quantity standardization.  NOTE: all
##' commodities will be standardized in the calorie standardization, as that is
##' a simple adding of calories.
##' @param targetNodes A vector containing the ID's which should be
##' standardized to.
##' 
##' @return A data.table object with quantities and calories aggregated (or
##' disaggregated) to the targetNodes provided.
##' 

standardize = function(nodeData, idColname, quantityColname,
                       calorieRateColname, commodityTree, parentColname,
                       childColname, extractionColname, shareColname,
                       byProductColname = "byProductFlag", targetNodes){

    ## Data Quality Checks
    stopifnot(c(idColname, quantityColname, calorieRateColname) %in%
                  colnames(nodeData))
    stopifnot(c(parentColname, childColname, extractionColname,
                shareColname, byProductColname) %in% colnames(commodityTree))
    stopifnot(is(nodeData, "data.table"))
    stopifnot(is(commodityTree, "data.table"))
    if("totalCalories" %in% colnames(nodeData))
        stop("'totalCalories' cannot be a column name of nodeData, as this ",
             "column will be created by this standardization function.")

    ## Prepare for aggregation/disaggregation
    commodityTree = splitEdges(edges = commodityTree,
                               targetElements = targetNodes,
                               parent = parentColname, child = childColname)
    
    ## Roll up calories
    calorie = copy(nodeData)
    calorie[, totalCalories := get(quantityColname) * get(calorieRateColname)]
    calorie = rollUpNodes(nodes = calorie,
                          edgesUp = commodityTree[direction == "up", ],
                          nodesID = idColname, parentID = parentColname,
                          childID = childColname,
                          calculationFormula = totalCalories ~ totalCalories,
                          aggregationFormula = totalCalories ~ sum(totalCalories))
    
    ## Roll down calories
    aggFormula = as.formula(paste0(
        "totalCalories ~ sum(totalCalories * ifelse(is.na(", shareColname, "),
        100, ", shareColname, ")/ 100)"))
    calorie = rollDownNodes(nodes = calorie,
                            edgesDown = commodityTree[direction == "down", ],
                            nodesID = idColname, parentID = parentColname,
                            childID = childColname,
                            calculationFormula = list(),
                            disaggregationFormula = aggFormula)
    
    ## Roll up quantities
    calcFormula = as.formula(paste0(quantityColname, "~",
                                    quantityColname, "/", extractionColname))
    aggFormula = as.formula(paste0(quantityColname,
                                   "~ sum(", quantityColname, ")"))
    quantity = copy(nodeData)
    quantity = rollUpNodes(nodes = quantity,
                           edgesUp = commodityTree[direction == "up", ],
                           nodesID = idColname, parentID = parentColname,
                           childID = childColname,
                           calculationFormula = calcFormula,
                           aggregationFormula = aggFormula)
    
    ## Roll down quantities
    aggFormula = as.formula(paste0(
        quantityColname, " ~ sum(", quantityColname, " * !", byProductColname,
        " * ifelse(is.na(", shareColname, "), 100, ", shareColname, ")/ 100)"))
    quantity = rollDownNodes(nodes = quantity,
                            edgesDown = commodityTree[direction == "down", ],
                            nodesID = idColname, parentID = parentColname,
                            childID = childColname,
                            calculationFormula = list(),
                            disaggregationFormula = aggFormula)
    
    output = merge(quantity, calorie, by = idColname)
    setnames(output, quantityColname, "totalQuantity")
    return(output)
}
