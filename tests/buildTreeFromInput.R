########################################################################
## Title: This is a new script which rather than taking the tree from
## the default file, we build in from input.
## Date: 2014-07-22
########################################################################

library(data.table)
library(igraph)
library(reshape2)
currentYear = 2009
target = as.character(sort(read.csv(file = "fbs_table.csv")$FBS.Code))
plotGraph = TRUE
standardizeElementCode =
    c(51, 61, 71, 91, 141, 101, 111, 121, 151)
standardizeElementName =
    c("Production", "Import", "DStock", "Export",
      "Food", "Feed", "Seed", "Waste", "Other Util"
      )

## Load the input file
input.dt =
    data.table(
        read.csv(file = "usa_demo_input_data.csv",
                 stringsAsFactors = FALSE
                 )
        )
input.dt = input.dt[Year == currentYear, ]

## calculate the reverse proportion
input.dt[, share := Num/sum(Num, na.rm = TRUE),
         by = "Child.Item.Code"]
input.dt[!is.finite(share), share := 0]


## Load the extraction rate data
extract.dt =
    data.table(
        read.csv(file = "usa_demo_extract_data_full.csv",
                 stringsAsFactors = FALSE
                 )
        )
extract.dt = extract.dt[Year == currentYear, ]

## Set extraction rate to missing if it is zero by calculation
extract.dt[Num == 0 & Symb == "C", Num := NA]

## NOTE (Michael): Still need the default network here to classify the
##                 commodities.
usaNetwork.dt =
    data.table(
        read.csv(file = "usa_demo_network.csv",
                 stringsAsFactors = FALSE
                 )
        )

## Create the data table which contains all information about the nodes.
nodeInfo.dt =
    merge(
        usaNetwork.dt[, list(Item.Code, FBS.Parent.Code,
                             Aggregate.Method, Default.Extraction.Rates,
                             Use.Calorie
                             )
                      ],
        extract.dt[, list(Item.Code, Item.Name, Num)
                   ]
        , by = "Item.Code", all = TRUE
        )

## Compute the final extraction rate based on different layer of info
nodeInfo.dt[,
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
                                      

## build the tree from the input file
input.graph =
    graph.data.frame(
        input.dt[, list(Child.Item.Code, Item.Code, share)]
        )

## Assign extraction rate to all edge
for(vertex in V(input.graph)$name){
    print(vertex)
    E(input.graph)[from(vertex)]$extractionRate =
        nodeInfo.dt[Item.Code == as.numeric(vertex), finalExtractionRate]
}


getSubgraph = function(terminalNode, graph){
    dist = shortest.paths(graph, to = terminalNode, weights = NA,
        mode = "out")
    subsetNodes = names(dist[which(is.finite(dist)), ])
    induced.subgraph(graph, v = subsetNodes)
}

check.graph = getSubgraph("15", input.graph)
## plot(check.graph)
plot(check.graph,
     ## vertex.size = 0.3,
     vertex.label = V(check.graph)$name,
     ## vertex.label.cex = 1,
     ## edge.arrow.size = 1,
     edge.label = round(E(check.graph)$extractionRate, 4),
     edge.color = ifelse(E(check.graph)$share == 0,
                         "red", "grey"),
     ## edge.arrow.mode = 1,
     edge.curved = 0.5
     )


## NOTE (Michael): It looks like I can't build the tree from the input
##                 file. Since it only includes inputs which were
##                 calculated by AUPUS.
