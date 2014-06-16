########################################################################
## Title: Demo for the standardization framework
## Date: 2014-06-17
########################################################################



## load libraries
library(data.table)
library(igraph)
library(reshape2)

## Read the graph for usa
usa2009network.dt = data.table(read.csv(file = "usa_2009_network.csv",
  stringsAsFactors = FALSE))
usa2009network.dt[, finalExtractRate :=
                  ifelse(!is.na(Specific.Rates), Specific.Rates,
                         ifelse(!is.na(Default.Extraction.Rates),
                                Default.Extraction.Rates, NA))]

## This create the vertex table, need to think about how to handle the
## fcl and fbs classification together.
itemTable.dt =
    unique(usa2009network.dt[, list(Item.Code, Item.Name)])
itemTable.dt[, FBS := FALSE]
fbsTable.dt = 
    unique(usa2009network.dt[!(FBS.Parent.Code %in% Item.Code),
                                 list(FBS.Parent.Code, FBS.Parent.Name)])
setnames(fbsTable.dt, old = c("FBS.Parent.Code", "FBS.Parent.Name"),
         new = c("Item.Code", "Item.Name"))
fbsTable.dt[, FBS := TRUE]
nodesTable.dt = rbind(itemTable.dt, fbsTable.dt)


## Devide the extraction rate by 10000 so it becomes a standard
## expression
usa2009network.dt[, finalExtractRate := finalExtractRate/10000]

## create the graph
usa2009network.graph =
    graph.data.frame(
        d = usa2009network.dt[, list(Item.Code, FBS.Parent.Code)],
        vertices = nodesTable.dt
        )

## Assign extraction rates to the graph
E(usa2009network.graph)$weight =
    usa2009network.dt[, finalExtractRate]

## Assign calorie only to graph, should it be vertex or edge?
E(usa2009network.graph)$calorieOnly =
    usa2009network.dt[, Use.Calorie]

## Function for adding the reverse edge with weights
add.reverse.edges = function(graph, ..., attr = list()){
    current.edges = get.edgelist(graph)
    add.edges(graph, edges = c(rev(t(current.edges))),
              attr = list(weight = rev(1/E(graph)$weight),
                  calorieOnly = rev(E(graph)$calorieOnly)), ...)
}

## The final full graph
## usa2009FullNetwork.graph = add.reverse.edges(usa2009network.graph)
standardization.graph = add.reverse.edges(usa2009network.graph)

## For this demo, we standardize to the FBS code 2511
## standardization.graph = induced.subgraph(usa2009FullNetwork.graph,
##     V(usa2009FullNetwork.graph)
##     [which(is.finite(shortest.paths(usa2009FullNetwork.graph,
##                                     to = "2511")))]$name)


## Plot of the network assuming the FBS item 2511 is the root.
##
plot.igraph(standardization.graph,
            vertex.color =
            ifelse(V(standardization.graph)$name == "2511",
                   "steelblue", "white"),
            vertex.frame.color =
            ifelse(V(standardization.graph)$name == "2511",
                   "darkblue", "steelblue"),
                   vertex.size = 20,
            vertex.label = gsub(" |,", "\n",
                V(standardization.graph)$Item.Name),
            vertex.label.family = "sans", vertex.label.cex = 0.5,
            vertex.label.color = 
            ifelse(V(standardization.graph)$name == "2511",
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
demoData.dt = data.table(read.csv(file = "usa_2009_demo_data.csv",
    stringsAsFactors = FALSE))


## This is a problem that needs to be solved, the names and meaning of
## codes changes with primary and processed commodity.
## demoData.dt[Item.Code == 15 & Element.Code == 131, Element.Code := 141]

## Elements to be standardized.
standardizeElementCode = c(61, 71, 91, 101, 111, 141)

## Assign attributes to the vertex
for(i in standardizeElementCode){
    standardization.graph =
        with(demoData.dt[Element.Code == i, ],
             set.vertex.attribute(standardization.graph,
                                  name = as.character(i),
                                  index = V(standardization.graph)[as.character(Item.Code)],
                                  value = Num_2009))
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
    physicalDistance =
        shortest.paths(graph = graph,
                       to = as.character(target),
                       weight = calorieOnly,
                       algorithm = "johnson")
    ## This is somewhat a hack to turn the calorie path off
    physicalDistance[physicalDistance == 0] = Inf
    exp(weightedLogDistance + physicalDistance * log(minWeight))
}

## Compute the direct weights
directWeights = computeDirectWeight(standardization.graph,
    target = "2511", weights = 1/(E(standardization.graph)$weight),
    calorieOnly = as.numeric(!E(standardization.graph)$calorieOnly))
V(standardization.graph)[rownames(directWeights)]$directWeight =
    ifelse(is.finite(directWeights), directWeights, 0)

## Standardization, trade and feed are slightly off.
sum(V(standardization.graph)$`61` * 
    V(standardization.graph)$directWeight, na.rm = TRUE)/1000
sum(V(standardization.graph)$`71` *
    V(standardization.graph)$directWeight, na.rm = TRUE)/1000
sum(V(standardization.graph)$`91` *
    V(standardization.graph)$directWeight, na.rm = TRUE)/1000
sum(V(standardization.graph)$`101` *
    V(standardization.graph)$directWeight, na.rm = TRUE)/1000
sum(V(standardization.graph)$`111` *
    V(standardization.graph)$directWeight, na.rm = TRUE)/1000
sum(V(standardization.graph)$`141` *
    V(standardization.graph)$directWeight, na.rm = TRUE)/1000

