########################################################################
## Title: Demo for the standardization framework
## Date: 2014-06-17
########################################################################



## load libraries
library(data.table)
library(igraph)
library(reshape2)
currentYear = 2009
target = "2511"

## Read the graph for usa
usaNetwork.dt = data.table(read.csv(file = "usa_demo_network.csv",
  stringsAsFactors = FALSE))

## Read specific extraction rate
extract.dt = data.table(read.csv(file = "usa_demo_extract_data.csv",
    stringsAsFactors = FALSE))

## Merge and obtain the final extraction rates
usaNetwork.dt = merge(usaNetwork.dt,
    extract.dt[Year == currentYear, list(Item.Code, Num)],
    by = "Item.Code", all.x = TRUE)
setnames(usaNetwork.dt, old = "Num", new = "Specific.Rates")
usaNetwork.dt[, finalExtractRate :=
                  ifelse(!is.na(Specific.Rates), Specific.Rates,
                         ifelse(!is.na(Default.Extraction.Rates),
                                Default.Extraction.Rates, NA))]

## This is a hack, need to accommodate this
usaNetwork.dt[Item.Code == 15, finalExtractRate := 10000]

## This create the vertex table, need to think about how to handle the
## fcl and fbs classification together.
itemTable.dt =
    unique(usaNetwork.dt[, list(Item.Code, Item.Name)])
itemTable.dt[, FBS := FALSE]
fbsTable.dt = 
    unique(usaNetwork.dt[!(FBS.Parent.Code %in% Item.Code),
                                 list(FBS.Parent.Code, FBS.Parent.Name)])
setnames(fbsTable.dt, old = c("FBS.Parent.Code", "FBS.Parent.Name"),
         new = c("Item.Code", "Item.Name"))
fbsTable.dt[, FBS := TRUE]
nodesTable.dt = rbind(itemTable.dt, fbsTable.dt)


## Devide the extraction rate by 10000 so it becomes a standard
## expression
usaNetwork.dt[, finalExtractRate := finalExtractRate/10000]

## create the graph
usaNetwork.graph =
    graph.data.frame(
        d = usaNetwork.dt[, list(Item.Code, FBS.Parent.Code)],
        vertices = nodesTable.dt
        )

## Assign extraction rates to the graph
E(usaNetwork.graph)$weight =
    usaNetwork.dt[, finalExtractRate]

## Assign calorie only to graph, should it be vertex or edge?
E(usaNetwork.graph)$calorieOnly =
    usaNetwork.dt[, Use.Calorie]

## Function for adding the reverse edge with weights
add.reverse.edges = function(graph, ..., attr = list()){
    current.edges = get.edgelist(graph)
    add.edges(graph, edges = c(rev(t(current.edges))),
              attr = list(weight = rev(1/E(graph)$weight),
                  calorieOnly = rev(E(graph)$calorieOnly)), ...)
}

## The final full graph
## usaFullNetwork.graph = add.reverse.edges(usaNetwork.graph)
standardization.graph = add.reverse.edges(usaNetwork.graph)

## For this demo, we standardize to the FBS code 2511
## standardization.graph = induced.subgraph(usaFullNetwork.graph,
##     V(usaFullNetwork.graph)
##     [which(is.finite(shortest.paths(usaFullNetwork.graph,
##                                     to = "2511")))]$name)


## Plot of the network assuming the FBS item 2511 is the root.
##
plot.igraph(standardization.graph,
            vertex.color =
            ifelse(V(standardization.graph)$name == target,
                   "steelblue", "white"),
            vertex.frame.color =
            ifelse(V(standardization.graph)$name == target,
                   "darkblue", "steelblue"),
                   vertex.size = 20,
            vertex.label = gsub(" |,", "\n",
                V(standardization.graph)$Item.Name),
            vertex.label.family = "sans", vertex.label.cex = 0.5,
            vertex.label.color = 
            ifelse(V(standardization.graph)$name == target,
                   "white", "steelblue"),            
            edge.arrow.size = 0.5,
            edge.label =
            round(E(standardization.graph)$weight, 4),
            edge.label.cex = 0.8,
            edge.label.family = "sans",
            edge.lty = ifelse(E(standardization.graph)$calorieOnly,
                2, 1),
            edge.arrow.mode = 1,
            edge.curved = 0.5,
            layout = layout.auto
            )


## Read the SUA data
sua.dt = data.table(read.csv(file = "usa_demo_sua_data.csv",
    stringsAsFactors = FALSE))
sua.dt = sua.dt[Year == currentYear, ]

## Read nutrient data, check why there is food in FBS selection
nutrient.dt = data.table(read.csv(file = "usa_demo_nutrient_data.csv",
    stringsAsFactors = FALSE))
nutrient.dt = nutrient.dt[Year == currentYear & Element.Code != 141, ]

fbs.dt = rbind(sua.dt, nutrient.dt)

## attributes of vertexes
standardizeElementCode = c(61, 71, 91, 101, 111, 141, 261, 271, 281)

## Assign attributes to the vertex
for(i in standardizeElementCode){
    standardization.graph =
        with(fbs.dt[Element.Code == i, ],
             set.vertex.attribute(standardization.graph,
                                  name = as.character(i),
                                  index = V(standardization.graph)[as.character(Item.Code)],
                                  value = Num))
}



## Function for computing the direct distance to a certain commodity
computeDirectWeight = function(graph, target, weights, calorieOnly){
    ## Need to think about how to deal with zero weights
    minWeight = min(weights, na.rm = TRUE)
    ## print(minWeight)
    weightedLogDistance =
        shortest.paths(graph = graph,
                     to = as.character(target),
                     weights = log(weights/minWeight) * calorieOnly,
                     algorithm = "johnson")
    ## print(weightedLogDistance)
    
    physicalDistance =
        shortest.paths(graph = graph,
                       to = as.character(target),
                       weight = calorieOnly,
                       algorithm = "johnson")
    ## This is somewhat a hack to turn the calorie path off
    physicalDistance[physicalDistance == 0] = Inf
    physicalDistance[rownames(physicalDistance) == target] = 0
    ## print(physicalDistance)
    
    exp(weightedLogDistance + physicalDistance * log(minWeight))
}

## Compute the direct weights
directWeights = computeDirectWeight(standardization.graph,
    target = target, weights = 1/(E(standardization.graph)$weight),
    calorieOnly = as.numeric(!E(standardization.graph)$calorieOnly))
V(standardization.graph)[rownames(directWeights)]$directWeight =
    ifelse(is.finite(directWeights), directWeights, 0)

## Standardization, trades are slightly off need to investigate why.
sum(V(standardization.graph)$`61` * 
    V(standardization.graph)$directWeight, na.rm = TRUE)/1000
sum(V(standardization.graph)$`71` *
    V(standardization.graph)$directWeight, na.rm = TRUE)/1000
sum(V(standardization.graph)$`91` *
    V(standardization.graph)$directWeight, na.rm = TRUE)/1000
sum(V(standardization.graph)$`141` *
    V(standardization.graph)$directWeight, na.rm = TRUE)/1000
sum(V(standardization.graph)$`101` *
    V(standardization.graph)$directWeight, na.rm = TRUE)/1000
sum(V(standardization.graph)$`111` *
    V(standardization.graph)$directWeight, na.rm = TRUE)/1000

## NOTE (Michael): Need to obtain the data for population


## Kg/Yr/caput
sum(V(standardization.graph)$`141` *
    V(standardization.graph)$directWeight, na.rm = TRUE)/309492

## g/day/caput
sum(V(standardization.graph)$`141` *
    V(standardization.graph)$directWeight, na.rm = TRUE)/309492/365 *
    1000

## Kcal/day/caput
## This is wrong
sum(V(standardization.graph)$`141`/100 * 
    V(standardization.graph)$`261` *
    V(standardization.graph)$directWeight/309492/365 * 1000,
    na.rm = TRUE)

## This should be correct
sum(V(standardization.graph)$`141` * 1000 * 10 * 
    V(standardization.graph)$`261` *
    as.numeric(V(standardization.graph)$directWeight != 0),
    na.rm = TRUE)/309492000/365


