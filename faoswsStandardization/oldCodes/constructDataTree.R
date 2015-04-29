##' Construct Commodity Tree
##' 
##' This function creates an object of type data.tree that contains the
##' commodity tree.  This can then be used to easily aggregate calories and
##' quantites to standardized values.
##' 
##' @param edges A data.table containing the edges for the tree.
##' @param parentColname The column name of edges which contains the
##' parent commodity's ID.
##' @param childColname The column name of edges which contains the
##' child commodity's ID.
##' @param extractionColname The column name of edges which contains the
##' extraction rate.
##' @param shareColname The column name of edges which contains the
##' share data.
##' 
##' @return A data.tree object containing the commodity tree.
##' 

constructCommodityTree <- function(edges, parentColname, childColname,
                                   extractionColname, shareColname){
    
    ## Data Quality Checks
    stopifnot(c(parentColname, childColname,
                extractionColname, shareColname) %in% colnames(edges))
    
    ## Start with a fresh commodityTree object
    suppressWarnings(rm(commodityTree))
    commodityTree <- Node$new("Total")
    ## Any nodes which don't have children should be created as descendants of
    ## the top, "Total" node.
    topNodes <- unique(edges[[parentColname]][
        !edges[[parentColname]] %in% edges[[childColname]]])
    for(node in topNodes){
        node = paste0("fcl", node)
        ## Assign the child as a child of the parent in the data.tree
        eval(parse(text = paste0(node, "<- commodityTree$AddChild('",
                                 node, "')")))
        ## Save the extraction rate and share to the child
        eval(parse(text = paste0(node, "$extractionRate <- ", 0)))
        eval(parse(text = paste0(node, "$share <- ", 100)))
    }
    ## Add all other nodes, which are descendants of parent nodes, to the
    ## data.tree.  Note: we should sort edges to make sure 
    for(i in 1:nrow(edges)){
        ## Assign the child as a child of the parent in the data.tree
        childName = paste0("fcl", edges[[childColname]][i])
        parentName = paste0("fcl", edges[[parentColname]][i])
        eval(parse(text = paste0(childName, "<- ", parentName, "$AddChild('",
                                 childName, "')")))
        ## Save the extraction rate and share to the child
        eval(parse(text = paste0(childName, "$extractionRate <- ",
                                 edges[[extractionColname]][i])))
        eval(parse(text = paste0(childName, "$share <- ",
                                 edges[[shareColname]][i])))
    }
    return(commodityTree)
}