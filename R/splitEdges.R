##' Split Edges
##' 
##' This function takes the edges object and appends a new column, direction,
##' which describes if the current node corresponds to the "rolling up" or
##' "rolling down" part of standardization.  In other words, does the current
##' edge contain information about commodities that need to be aggregated up
##' or disaggregated down?
##' 
##' @param edges A data.table object containing the columns "parent" and
##' "children" which specify the edges of the commodity tree.  Additionally,
##' other columns can be present, such as extraction rates or split rates.
##' @param targetElements A character vector with the ids for the target nodes.
##' Multiple target nodes can (and should, in most cases) be specified.
##' @param parentID The column name of edges which contains the ID of the
##' parent node.
##' @param childID The column name of edges which contains the ID of the
##' child node.
##' 
##' @return No object is returned, but the passed edges data.table is given
##' an additional column, direction, which describes if that edge corresponds
##' to an aggregation ("up") or disaggregation ("down").
##' 

splitEdges <- function(edges, targetElements, parentID, childID){
    
    ## Data Quality Checks
    stopifnot(is(edges, "data.table"))
    stopifnot(c(parentID, childID) %in% colnames(edges))
    if("direction" %in% colnames(edges))
        stop("'direction' cannot be a colname of edges as that value is ",
             "created and assigned in this script.")
    
    edges[, direction := ifelse(get(parentID) %in% targetElements, "up", NA)]
    edges[, direction := ifelse(get(childID) %in% targetElements,
                                "down", direction)]
    oldNACnt <- nrow(edges) # Stop when no more elements have been updated
    NACnt <- sum(is.na(edges$direction))
    while(oldNACnt != NACnt){
        elementsRollingUp <- edges[!is.na(direction) & direction == "up",
                                   get(childID)]
        edges[get(parentID) %in% elementsRollingUp, direction := "up"]
        elementsRollingDown <- edges[!is.na(direction) & direction == "down",
                                     get(parentID)]
        edges[get(childID) %in% elementsRollingDown, direction := "down"]
        oldNACnt <- NACnt
        NACnt <- sum(is.na(edges$direction))
    }
    if(any(is.na(edges$direction)))
        warning("Some edges have not been assigned to an aggregation or ",
                "disaggregation process.  This suggests that targetElements ",
                "may not be comprehensive.")

    return(edges)
}
