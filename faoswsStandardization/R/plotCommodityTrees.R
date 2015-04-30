##' Plot Commodity Trees
##' 
##' This function generates plots for each of the commodity trees defined
##' in a commodity tree object.  A separate plot is given for each node that
##' has no parent, and this should allow for the producing of a wheat tree,
##' a milk tree, etc.
##' 
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
##' 
##' @return
##' 

plotCommodityTrees = function(commodityTree, parentColname, childColname,
                              extractionColname){
    
    ## Data Quality Checks
    stopifnot(is(commodityTree, "data.table"))
    stopifnot(c(parentColname, childColname, extractionColname) %in%
                  colnames(commodityTree))
    if(any(commodityTree[, .N, by = c(parentColname, childColname)][, N] > 1))
        stop("This function is not designed to work for multiple countries, ",
             "years, etc.  Please subset the data and loop to generate ",
             "multiple such trees.")
    
    ## Find the top nodes.
    topNodes = setdiff(commodityTree[[parentColname]],
                       commodityTree[[childColname]])
    
    ## Now, we'll assume that each topNode corresponds ot a unique commodity
    ## tree (at least initially).  However, we may find that two topNodes feed
    ## into the same child, and thus we may need to combine 
    ## combine some of these "topNodes" into one tree, and so this table can
    ## be modified later in the code if topNodes are grouped into one tree.
    ##
    ## Assign the edges to the approriate tree.  Any child node receives the
    ## treeID of it's parent, if available.  Some children may be children
    ## of multiple parents and hence (possibly) multiple treeID's.  In those
    ## cases, we'll need to group treeID's into the same group.
    commodityTree[, oldTreeID := NA_character_]
    commodityTree[get(parentColname) %in% topNodes,
                  oldTreeID := get(parentColname)]
    while(any(is.na(commodityTree[, oldTreeID]))){
        ids = commodityTree[!is.na(oldTreeID), .N,
                            by = c(childColname, "oldTreeID")]
        ## Update ids with the first appearing id.  We'll group the duplicate
        ## treeID's together later.
        aggregatedIds = ids[, list(treeID = oldTreeID[1]), by = childColname]
        setnames(aggregatedIds, childColname, parentColname)
        commodityTree = merge(commodityTree, aggregatedIds,
                              by = parentColname, all.x = TRUE)
        commodityTree[is.na(oldTreeID), oldTreeID := treeID]
        commodityTree[, treeID := NULL]
    }
    
    ## Group together oldTreeID's into relevant groups
    commodityTree[, finalTreeID := oldTreeID[1], by = childColname]
    commodityTree[, oldTreeID := NULL]
    
    edges2Plot = function(edges){
        allNodes = unique(c(edges[[parentColname]], edges[[childColname]]))
        edges[, c(parentColname) := factor(get(parentColname),
                                           levels = allNodes)]
        edges[, c(childColname) := factor(get(childColname),
                                          levels = allNodes)]
        A = matrix(0, length(allNodes), length(allNodes))
        indices = as.matrix(edges[, list(as.numeric(get(childColname)),
                                         as.numeric(get(parentColname)))])
        A[indices] = 1
        rownames(A) = allNodes
        colnames(A) = allNodes
        plotmat(A, curve = 0)
    }
    for(id in commodityTree[, unique(finalTreeID)]){
    }
}