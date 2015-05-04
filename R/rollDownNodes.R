##' Roll Down Nodes
##' 
##' This function takes a nodes data.table and an edges data.table
##' and disaggregates the nodes data.table according to the edges
##' structure.
##' 
##' @param nodes A data.table with columns containing the node ID and node
##' attributes.
##' @param edgesDown A data.table with parent and child node IDs (corresponding
##' to the IDs in nodes) which specify the commodity tree structure.
##' Additionally, there may be edge attributes present which control how
##' disaggregation occurs via the disaggregation function(s).
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
##' attribute, or could create a new element.
##' @param disaggregationFormula A formula, or a list of R formula objects,
##' specifying how multiple values should be combined.
##' 
##' @example
##' nodes = data.table(id = 1:9, value = 1)
##' edgesDown = data.table(parent = 1:4, child = 2:5, multiplier = 1:4)
##' nodes = rollDownNodes(nodes, edgesDown, nodesID = "id", parentID = "parent",
##'                       childID = "child",
##'                       calculationFormula = value ~ value * multiplier,
##'                       disaggregationFormula = value ~ sum(value))
##' nodes
##' edgesUp = data.table(parent = 5:8, child = 6:9, multiplier = 4:1)
##' rollUpNodes(nodes, edgesUp, nodesID = "id", parentID = "parent",
##'             childID = "child",
##'             calculationFormula = value ~ value * multiplier,
##'             aggregationFormula = value ~ sum(value))
##' 
##' @return The disaggregated nodes table.
##' 

rollDownNodes <- function(nodes, edgesDown, nodesID, parentID, childID,
                       calculationFormula, disaggregationFormula){
    ## Rather than repeat the logic of the rollUpNodes function, simply
    ## roll down by using the roll up formula but reversing parents and
    ## children.
    rollUpNodes(nodes = nodes, edgesUp = edgesDown, nodesID = nodesID,
                parentID = childID, childID = parentID,
                calculationFormula = calculationFormula,
                aggregationFormula = disaggregationFormula)
}
