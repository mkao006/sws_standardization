##' Roll Up Nodes
##' 
##' This function takes a nodes data.table and an edges data.table
##' and aggregates the nodes data.table according to the edges
##' structure.
##' 
##' @param nodes A data.table with columns containing the node ID and node
##' attributes.
##' @param edgesUp A data.table with parent and child node IDs (corresponding
##' to the IDs in nodes) which specify the commodity tree structure.
##' Additionally, there may be edge attributes present which control how
##' aggregation occurs via the aggregation function.
##' @param nodesID The column name of nodes which contains the ID variable.
##' @param parentID The column name of edges which contains the ID of the
##' parent node.
##' @param childID The column name of edges which contains the ID of the
##' child node.
##' @param calculationFormula A formula, or a list of R formula objects, that
##' will be evaluated on a data.table object.  This data.table
##' object results from the merge of the nodes data.table with
##' the edges data.table, joining nodeID with childID.
##' Thus, this data.table has nodeID, parentID, and any other columns of nodes
##' or edges available for this formula.  Each individual expression can
##' therefore redefine an element of this data.table based on a node or edge
##' attribute, or could create a new element.  If no calculation is required,
##' this can be assigned an empty list (i.e. list())..
##' @param aggregationFormula A formula, or a list of R formula objects,
##' specifying how multiple values should be combined.
##' 
##' @example
##' nodes <- data.table(id = 1:10, value = 1)
##' edgesUp <- data.table(parent = 1:9, child = 2:10, multiplier = 2:10)
##' rollUpNodes(nodes, edgesUp, nodesID = "id", parentID = "parent",
##'             childID = "child",
##'             calculationFormula = value ~ value * multiplier,
##'             aggregationFormula = value ~ sum(value))
##' sum(factorial(1:10))
##' 
##' # Weird behavior, because aggregation formula doesn't aggregate
##' rollUpNodes(nodes, edgesUp, nodesID = "id", parentID = "parent",
##'             childID = "child",
##'             calculationFormula = value ~ value * multiplier,
##'             aggregationFormula = value ~ value)
##' 
##' edgesUp$adder <- 9:1
##' nodes$value <- 0
##' rollUpNodes(nodes, edgesUp, nodesID = "id", parentID = "parent",
##'             childID = "child",
##'             calculationFormula = value ~ value + adder,
##'             aggregationFormula = value ~ sum(value))
##' sum((1:9)^2)
##' 
##' nodes <- data.table(id = 1:4, value1 = rnorm(4), value2 = rnorm(4))
##' edgesUp <- data.table(parent = c(1, 1, 3), child = c(2, 3, 4))
##' nodes
##' rollUpNodes(nodes, edgesUp, nodesID = "id", parentID = "parent",
##'             childID = "child",
##'             calculationFormula = list(value1 ~ value1),
##'             aggregationFormula = list(value1 ~ sum(value1),
##'                                       value2 ~ prod(value2)))
##' sum(nodes$value1)
##' prod(nodes$value2)
##' 
##' @return The aggregated nodes table.
##' 

rollUpNodes <- function(nodes, edgesUp, nodesID, parentID, childID,
                       calculationFormula, aggregationFormula){
    
    ## Data Quality Checks
    stopifnot(is(nodes, "data.table"))
    stopifnot(is(edgesUp, "data.table"))
    stopifnot(nodesID %in% colnames(nodes))
    stopifnot(c(parentID, childID) %in% colnames(edgesUp))
    if(!is(calculationFormula, "list"))
        calculationFormula <- list(calculationFormula)
    if(length(calculationFormula) != 0)
        stopifnot(sapply(calculationFormula, is, "formula"))
    if(!is(aggregationFormula, "list"))
        aggregationFormula <- list(aggregationFormula)
    stopifnot(sapply(aggregationFormula, is, "formula"))
    
    ## Convert the passed formulas to data.table expressions
    calculationExpression <- lapply(calculationFormula, function(form){
        paste(form[2], ":=", form[3])
    })
    ## Note: no colon below because all formulas must be evaluated at once
    ## and thus require slightly different data.table syntax
    aggregationExpression <- lapply(aggregationFormula, function(form){
        paste(form[2], "=", form[3])
    })
    aggregationExpression <- paste0(".(", paste(aggregationExpression,
                                              collapse = ","), ")")

    edges <- copy(edgesUp) # Don't change edges here
    ## It's ok to modify names since edges is a temp copy
    setnames(edges, childID, nodesID)
    setkeyv(edges, nodesID)
    setkeyv(nodes, nodesID)
    continue <- TRUE
    while(continue){
        nodes <- merge(nodes, edges, all.x = TRUE, allow.cartesian = TRUE)
        ## Evaluate each of the passed expressions in turn, but only evaluate
        ## it for nodes that are getting aggregated up.
        for(expr in calculationExpression)
            nodes[!is.na(get(parentID)), eval(parse(text = expr))]
        ## Any node which is aggregated should get reassigned the parent node
        ## id for the aggregation step.
        nodes[!is.na(get(parentID)), c(nodesID) := get(parentID)]
        continue <- any(!is.na(nodes[[parentID]]))
        ## Aggregate nodes using the aggregationExpression.  Usually, this will
        ## reduce the size of the nodes data.table (as we're collapsing rows).
        nodes <- nodes[, eval(parse(text = aggregationExpression)), by = nodesID]
        setkeyv(nodes, nodesID)
    }
    if(length(unique(nodes[[nodesID]])) != length(nodes[[nodesID]]))
        warning("Aggregation functions don't return a single value and so the ",
                "nodes don't actually get aggregated up (although the id's of ",
                "children do change to their parents).")
    return(nodes)
}
