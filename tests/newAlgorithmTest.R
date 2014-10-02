########################################################################
## Title: Tests of new algorithm framework
## Date: 2014-07-22
########################################################################

library(data.table)
library(igraph)
library(reshape2)
currentYear = 2009

## Pseudo codes
## (1) Read the default graph
defaultGraph.dt =
    data.table(
        read.csv(file = "usa_demo_network.csv",
                 stringsAsFactors = FALSE
                 )
        )

notInFBS = c(67, 851, 853, 864, 865, 918, 956, 974, 975, 986, 1014,
    1015, 1024, 1050, 1067, 1092, 1115, 1131, 1169, 1174, 1180, 1223,
    1512, 1513, 1525, 1526, 1538, 1539, 1551, 1552, 1560, 1561, 1568,
    1569, 1577, 1578, 1585, 1586, 1592, 1593, 1597, 1598, 1599)

## (2) Read the extraction rate data
extractRate.dt =
    data.table(
        read.csv(file = "usa_demo_extract_data_full.csv",
                 stringsAsFactors = FALSE
                 )
        )
extractRate.dt = extractRate.dt[Year == currentYear, ]

## Where extraction rate is zero and is calculated then assign zero,
## if missing with M and zero then assign NA.
extractRate.dt[Num == 0 & Symb == "C", Num := NA]
extractRate.dt[Symb == "M", Num := NA]


## (3) Compute the hybrid extraction rate
edgeInit.dt =
    merge(
        defaultGraph.dt[, list(Item.Code, Item.Name,
                               FBS.Parent.Code,
                               Aggregate.Method,
                               Default.Extraction.Rates,
                               Use.Calorie,
                               Weight)],
        extractRate.dt[, list(Item.Code, Num)],
        all = TRUE, by = "Item.Code"
        )

edgeInit.dt[,
            finalExtractionRate :=
            ifelse(Aggregate.Method == "T", 10000,
                   ifelse(Aggregate.Method == "X", 0,
                          ifelse(!is.na(Num), Num,
                                 ifelse(!is.na(Default.Extraction.Rates),
                                        Default.Extraction.Rates, NA
                                        )
                                 )
                          )
                   )
            ]
edgeInit.dt[, Default.Extraction.Rates := NULL]
edgeInit.dt[, Num := NULL]

## (4) Build initial graph with default graph
graphInit.dt = edgeInit.dt[, list(Item.Code, FBS.Parent.Code)]
setnames(graphInit.dt,
         old = c("Item.Code", "FBS.Parent.Code"),
         new = c("child", "parent")
         )


init.graph = graph.data.frame(graphInit.dt)

## (5) Read input for processing file
input.dt =
    data.table(
        read.csv(file = "usa_demo_input_data.csv",
                 stringsAsFactors = FALSE
                 )
        )
input.dt = input.dt[Year == currentYear, ]

## Remove input edge which are targets
input.dt =
    input.dt[!Child.Item.Code %in%
             defaultGraph.dt[Target == "Target", Item.Code],
             ]

## (6) Compute recipricol shares
input.dt = input.dt[Num != 0, ]
input.dt[, reciprocalShare := Num/sum(Num, na.rm = TRUE),
         by = "Child.Item.Code"]
input.dt[!is.finite(reciprocalShare), reciprocalShare := 0]


## (7) create input graph with shares
inputInit.dt =
    input.dt[, list(Child.Item.Code, Item.Code, reciprocalShare)]
setnames(inputInit.dt,
         old = c("Child.Item.Code", "Item.Code"),
         new = c("child", "parent")
         )

input.graph = graph.data.frame(inputInit.dt)


## Remove edge from init.graph which are present in the input.graph
outDegree = rowSums(input.graph[, sparse = FALSE])
haveOutDegree = names(outDegree[outDegree != 0])
init.graph = delete.edges(init.graph,
    edges = E(init.graph)[from(haveOutDegree)]
    )


## (8) Merge two graphs
standardization.graph = graph.union(init.graph, input.graph)

## (9) Assign reciprocal share of 1 to edge which has missing share
## then remove edge which has zero reciprocal share.
E(standardization.graph)[is.na(reciprocalShare)]$reciprocalShare = 1
E(standardization.graph)[reciprocalShare == 0]$reciprocalShare = 1

## (10) Assign attributes to edge
for(vertex in V(standardization.graph)$name){
    E(standardization.graph)[from(vertex)]$extractRate =
        edgeInit.dt[Item.Code == as.numeric(vertex), finalExtractionRate]
    tmp = edgeInit.dt[Item.Code == as.numeric(vertex), Item.Name]
    V(standardization.graph)[vertex]$label =
        ifelse(length(tmp) == 0, paste0("Target", vertex), tmp)
}


## (11) Removing the target equal zero
standardization.graph = delete.vertices(standardization.graph, v = "0")

## (12) Removing redudant nodes
redudantNodes = which(degree(standardization.graph, mode = "all") == 0)
standardization.graph =
    delete.vertices(standardization.graph, redudantNodes
                    )

hist(E(standardization.graph)$reciprocalShare *
     E(standardization.graph)$extractRate, breaks = 100)
range(E(standardization.graph)$reciprocalShare *
     E(standardization.graph)$extractRate, na.rm = TRUE)


## NOTE (Michael): Need to check the allocation of shares.
for(vertex in V(standardization.graph)$name){
    tmp = sum(E(standardization.graph)[from(vertex)]$reciprocalShare,
        na.rm = TRUE)
    if(tmp != 1){
        print(vertex)
        print(tmp)
    }
}

## (13) Compute direct distance
computeDirectWeight = function(graph, target, weights, calorieOnly){
    ## hack zero weights
    weights[weights == 0] = NA
    ## Need to think about how to deal with zero weights
    minWeight = min(weights, na.rm = TRUE)
    ## print(minWeight)
    weightedLogDistance =
        shortest.paths(graph = graph,
                       to = as.character(target),
                       weights = log(weights/minWeight) * calorieOnly,
                       algorithm = "johnson",
                       mode = "out")
    ## print(weightedLogDistance)
    
    physicalDistance =
        shortest.paths(graph = graph,
                       to = as.character(target),
                       weight = calorieOnly,
                       algorithm = "johnson",
                       mode = "out")
    ## This is somewhat a hack to turn the calorie path off
    physicalDistance[physicalDistance == 0] = Inf
    physicalDistance[rownames(physicalDistance) %in% target] = 0
    ## print(physicalDistance)
    
    exp(weightedLogDistance + physicalDistance * log(minWeight))
}


directWeights = computeDirectWeight(standardization.graph,
    target = V(standardization.graph)[grepl("Target", label)]$name,
    weights = (E(standardization.graph)$reciprocalShare)/
    (E(standardization.graph)$extractRate/10000),
    calorieOnly = rep(1, length(E(standardization.graph)$extractRate)))
image(directWeights)

## Read the sua data
sua.dt =
    data.table(
        read.csv(file = "usa_demo_sua_data_full.csv",
                 stringsAsFactors = FALSE
                 )
        )
sua.dt = sua.dt[Year == currentYear, ]

## tmp = dcast(sua.dt[, list(Item.Code, Element.Code, Num)],
##     Element.Code ~ Item.Code, value.var = "Num")
## rownames(tmp) = tmp[, 1]
## tmp = data.matrix(tmp[, -1])

tmp[, colnames(tmp)[colnames(tmp) %in% rownames(directWeights)]] %in% directWeights



########################################################################
## graphics for check
########################################################################

getSubgraph = function(terminalNode, graph){
    dist = shortest.paths(graph, to = terminalNode, weights = NA,
        mode = "out")
    subsetNodes = names(dist[which(is.finite(dist)), ])
    induced.subgraph(graph, v = subsetNodes)
}


pdf(file = "check_network.pdf")
for(vertex in V(standardization.graph)[grepl("Target", label)]$name){
    sub.graph = getSubgraph(terminalNode = vertex, standardization.graph)
    for(items in V(sub.graph)$name){
        realName = strsplit(items, "_")[[1]][1]
        tmp = sua.dt[Item.Code == realName & Element.Code == 141, Num]
        if(length(tmp) != 0)
            V(sub.graph)[items]$food = tmp
        }
    plot(sub.graph,
         vertex.label = paste0(V(sub.graph)$label, "\n",
             V(sub.graph)$food),
         vertex.color = ifelse(grepl("Target", V(sub.graph)$label),
             "steelblue", "white"),
         vertex.frame.color =
         ifelse(grepl("Target", V(sub.graph)$label),
                "darkblue", "steelblue"),
         vertex.size = 15,
         vertex.label.cex = 0.5,
         vertex.label.color = 
         ifelse(grepl("Target", V(sub.graph)$label),
                "white", "steelblue"),
         edge.arrow.size = 1,
         edge.label.cex = 0.5,
         edge.label =
         paste0("Ext: ", round(E(sub.graph)$extractRate, 4), "\n",
                "Share: ", round(E(sub.graph)$reciprocalShare, 4)),
         ## edge.color = ifelse(E(sub.graph)$reciprocalShare == 0,
         ##     "red", "grey"),
         edge.curved = 0.5
         )
}
graphics.off()

## There is still something wrong, the oil cotton seed should also
## include margarine.

## Hack function

mod = data.table(data.frame(get.edgelist(standardization.graph),
    stringsAsFactors = FALSE))
setnames(mod, c("X1", "X2"), c("V1", "V2"))
mod$extractRate = E(standardization.graph)$extractRate
mod$reciprocalShare = E(standardization.graph)$reciprocalShare
mod$finalWeight = mod$reciprocalShare/(mod$extractRate/10000)
mod[, `:=`(c("reciprocalShare", "extractRate"), NULL)]
tmp = unique(mod$V2)
target = tmp[nchar(tmp) == 4]
target = gsub("[^0-9]", "",
    V(standardization.graph)[grepl("Target", label)]$label)
mod = rbind(mod, data.table(V1 = target, V2 = target, finalWeight = 1))
mod = mod[is.finite(finalWeight), ]

base = copy(mod)
setnames(base, "finalWeight", "compoundRate")
setnames(mod, c("V1", "V2"), c("V2", "V3"))


iter = 0
while(!all(base$V2 %in% target)){
    iter = iter + 1
    print(iter)
    base = merge(base, mod, by = "V2", all.x = TRUE,
        allow.cartesian = TRUE)
    base[, compoundRate := compoundRate * finalWeight]
    base = base[!is.na(V3) & !is.na(compoundRate), ]
    base[, `:=`(c("V2", "finalWeight"), NULL)]
    setnames(base, "V3", "V2")
}
setkeyv(base, c("V2", "V1"))

tmp = sua.dt[Element.Code == 141, list(Item.Code, Num)]
setnames(tmp, "Item.Code", "V1")
tmp[, V1 := as.character(V1)]
tmp2 = merge(tmp, base, by = "V1")

tmp3 = tmp2[, sum(Num * compoundRate, na.rm = TRUE)/1000, by = "V2"]








## maizeGraph = getSubgraph("2514", standardization.graph)
## multiPathNode = which(rowSums(maizeGraph[, sparse = FALSE]) >=2 )
## unfold = unfold.tree(maizeWheat.graph, mode = "in",
##     roots = c("2514", "2511"))
## newtree = unfold$tree
## V(newtree)$label = V(maizeWheat.graph)[unfold$vertex_index]$label
## plot(newtree, vertex.label = V(newtree)$label)

findMultiplePathNodes = function(graph){
    multiPathNode = which(degree(graph, mode = "out") > 1)
    names(multiPathNode)
}
multiplePathNodes = findMultiplePathNodes(standardization.graph)

## Need to consider secondary break such as hydrogen oil

## Split multiple path nodes
for(i in multiplePathNodes){
    multiplePathEdgeFrom = E(standardization.graph)[from(i)]
    itemLabel = V(standardization.graph)[i]$label
    ## Need to check this
    tmp = data.frame(matrix(c(V(standardization.graph)$name[get.edges(standardization.graph, multiplePathEdgeFrom)], multiplePathEdgeFrom$reciprocalShare, multiplePathEdgeFrom$extractRate), ncol = 4))
    tmp[, 1] = paste0(tmp[, 1] , "_", tmp[, 2])
    tmp[, 3] = as.numeric(tmp[, 3])
    tmp[, 4] = as.numeric(tmp[, 4])
    colnames(tmp) = c("child", "parent", "reciprocalShare", "extractRate")    
    multiplePathEdgeTo = E(standardization.graph)[to(i)]
    ## Need to check this
    tmp2 = data.frame(matrix(c(V(standardization.graph)$name[get.edges(standardization.graph, multiplePathEdgeTo)], multiplePathEdgeTo$reciprocalShare, multiplePathEdgeTo$extractRate), ncol = 4))
    tmp2[, 2] = paste0(tmp2[, 1] , "_", tmp2[, 2])
    tmp2[, 3] = as.numeric(tmp2[, 3])
    tmp2[, 4] = as.numeric(tmp2[, 4])
    colnames(tmp2) = c("child", "parent", "reciprocalShare", "extractRate")    
    tmp3 = rbind(tmp, tmp2)
    

    ## tmp3.graph = graph.data.frame(tmp3)
    standardization.graph = standardization.graph + vertices(tmp3[, 1], label = itemLabel)
    standardization.graph = standardization.graph + edges(c(t(tmp3[, 1:2])), reciprocalShare = tmp3[, 3], extractRate = tmp3[, 4])
    
    ## standardization.graph =
    ##     graph.union(standardization.graph, tmp.graph)
    standardization.graph =
        delete.vertices(standardization.graph, v = i)
}

table(degree(standardization.graph, mode = "out"))

## Then compute the direct weights
computeDirectWeight = function(graph, target, weights, calorieOnly){
    ## hack zero weights
    weights[weights == 0] = NA
    ## Need to think about how to deal with zero weights
    minWeight = min(weights, na.rm = TRUE)
    ## print(minWeight)
    weightedLogDistance =
        shortest.paths(graph = graph,
                       to = as.character(target),
                       weights = log(weights/minWeight) * calorieOnly,
                       algorithm = "johnson",
                       mode = "out")
    ## print(weightedLogDistance)
    
    physicalDistance =
        shortest.paths(graph = graph,
                       to = as.character(target),
                       weight = calorieOnly,
                       algorithm = "johnson",
                       mode = "out")
    ## This is somewhat a hack to turn the calorie path off
    physicalDistance[physicalDistance == 0] = Inf
    physicalDistance[rownames(physicalDistance) %in% target] = 0
    ## print(physicalDistance)
    
    exp(weightedLogDistance + physicalDistance * log(minWeight))
}

directWeights = computeDirectWeight(standardization.graph,
    target = V(standardization.graph)[grepl("Target", label)]$name,
    weights = (E(standardization.graph)$reciprocalShare)/
    (E(standardization.graph)$extractRate/10000),
    calorieOnly = rep(1, length(E(standardization.graph)$extractRate)))
image(directWeights)

ind = which(is.finite(directWeights), arr.ind = TRUE)
weightsTable = data.table(child = rownames(directWeights)[ind[, 1]], parent = colnames(directWeights)[ind[, 2]], weights = directWeights[ind])
weightsTable$child = as.numeric(sapply(strsplit(weightsTable$child, "_"), function(x) x[1]))

weightsTable[, value := sum(weights, na.rm = TRUE), by = c("child", "parent")]
weightsTable[, weights := NULL]

finalTable = dcast(unique(weightsTable), child ~ parent, value.var = "value")

distanceMatrix = data.matrix(finalTable[, -1])
rownames(distanceMatrix) = finalTable$child
image(distanceMatrix)

tmp = dcast(sua.dt[, list(Element.Code, Item.Code, Num)], Element.Code ~ Item.Code, value.var = "Num")
suaMatrix = as.matrix(tmp[, -1])
rownames(suaMatrix) = tmp[, 1]

suaMatrix[is.na(suaMatrix)] = 0
distanceMatrix[is.na(distanceMatrix)] = 0

FBSQuantity = suaMatrix[,intersect(colnames(suaMatrix), rownames(distanceMatrix))] %*%
    distanceMatrix[intersect(colnames(suaMatrix), rownames(distanceMatrix)), ]

write.csv(t(FBSQuantity), file = "newAlgorithmFBS.csv")

## ## Need to substitute shortest.paths with average path.
## allPath = get.all.shortest.paths(maizeGraph,
##     from = multiPathNode, to = "2514")$res
## computeAveragePath = function(graph, v, to, path){
##     sapply(path, function(x){tmp =  induced.subgraph(graph, x);
##                                  shortest.paths(tmp, v = v, to = to,
##                                                 weights = E(tmp)$reciprocalShare/(E(tmp)$extractRate/10000))})
## }

## computeAveragePath(maizeGraph, v = "846", to = "2514", path = allPath)
## 0.317/0.079
## 0.6525/0.12


##     E(maizeGraph)[from("846")]$reciprocalShare
## E(maizeGraph)[from("846")]$extractRate


## plot(maizeGraph)
## is.simple(maizeGraph)

## E(maizeGraph)





pdf(file = "usa_full_graph.pdf", width = 30, height = 20)
plot(standardization.graph,
     vertex.label = V(standardization.graph)$label,
     vertex.color = ifelse(V(standardization.graph)$label %in% "Target",
         "steelblue", "white"),
     vertex.frame.color =
         ifelse(V(standardization.graph)$label %in% "Target",
                "darkblue", "steelblue"),
     vertex.size = 3,
     vertex.label.cex = 0.5,
     vertex.label.color = 
         ifelse(V(standardization.graph)$label %in% "Target",
                "white", "steelblue"),
     edge.arrow.size = 0.5,
     edge.label.cex = 0.8,
     edge.label = round(E(standardization.graph)$extractRate, 4),
     edge.color = ifelse(E(standardization.graph)$reciprocalShare == 0,
         "red", "grey"),
     edge.curved = 0.5
     )
graphics.off()

## (10) check share sum to one and extraction rates are all the same
## (11) Check the uniqueness of the path, if not weight combine it.
## (12) Collapese the graph for the computation
