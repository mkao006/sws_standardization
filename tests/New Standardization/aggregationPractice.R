library(faosws)
library(data.table)
for(file in dir("~/Documents/Github/sws_standardization/faoswsStandardization/",
            full.names = TRUE, pattern = "*.R$"))
    source(file)

## Now, let's run this on some FAO examples!

GetTestEnvironment(
    # baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
    # token = "835b930a-c37c-42a8-9a56-d8f9026be230"
    baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
    token = "2810fca0-b171-4ffb-ba25-f6603990d06d"
)

test = GetCodeTree(domain = "suafbs", dataset = "fbs_prebalance",
                   dimension = "measuredItemSuaFbs")
test = GetCodeTree(domain = "suafbs", dataset = "fbs_balanced",
                   dimension = "measuredItemSuaFbs")
test = GetCodeTree(domain = "faostat_one", dataset = "aupus_share_fs",
                   dimension = "measuredItemParentFS")

## Inputs:
test = data.table(parent = c("0015", "0016", "0017"), 
                  child  = c("0016, 0017, 0019", "0113, 0018, 0020, 0022", "0041"))
edges = faoswsUtil::adjacent2edge(test)
edges$extractionRate = runif(nrow(edges))
edges$shares = runif(nrow(edges))
nodes = data.table(commodity =
                       unique(do.call("c", edges[, c("parent", "children"),
                                                  with = FALSE])),
                   value = 1000)
targetElements = c("0016", "0017") # which elements should be rolled up/down to

editedEdges = copy(edges)
editedNodes = copy(nodes)
splitEdges(edges = editedEdges, targetElements = targetElements)
editedNodes = rollUpNodes(nodes = editedNodes,
                          edgesUp = editedEdges[direction == "up", ],
                          nodesID = "commodity", parentID = "parent",
                          childID = "children",
                          calculationFormula = value ~ value / extractionRate,
                          aggregationFormula = value ~ sum(value))
editedNodes = rollDownNodes(nodes = editedNodes,
                          edgesDown = editedEdges[direction == "down", ],
                          nodesID = "commodity", parentID = "parent",
                          childID = "children",
                          calculationFormula = value ~ value * extractionRate,
                          disaggregationFormula = value ~ sum(value))
