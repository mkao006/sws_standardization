########################################################################
## Title: New framework fully based on network
## Date: 2014-06-11
########################################################################


## load libraries
library(data.table)
library(igraph)
library(reshape2)
source("computeDirectExtractRate.R")
source("toExtractTree.R")
source("fillMissingCartesian.R")


## (1) Read Extraction rate
extractionRate.dt = data.table(read.csv("wheatExtractRate.csv"))

## (2) Read tree/network control file
##
## ---------------------------------------------------------------------
completeFBSnetwork.dt = data.table(read.csv(file = "tree_test.csv",
  stringsAsFactors = FALSE))
subNetwork.dt =
    completeFBSnetwork.dt[, list(Item.Name, FBS.Parent.Name,
                                 Default.Extraction.Rates)]

itemTable.dt =
    unique(completeFBSnetwork.dt[, list(Item.Code, Item.Name)])
itemTable.dt[, FBS := FALSE]
fbsTable.dt = 
    unique(completeFBSnetwork.dt[!(FBS.Parent.Code %in% Item.Code),
                                 list(FBS.Parent.Code, FBS.Parent.Name)])
setnames(fbsTable.dt, old = c("FBS.Parent.Code", "FBS.Parent.Name"),
         new = c("Item.Code", "Item.Name"))
fbsTable.dt[, FBS := TRUE]
nodesTable.dt = rbind(itemTable.dt, fbsTable.dt)

completeFBSnetwork.dt[, Default.Extraction.Rates :=
                     Default.Extraction.Rates/10000]
tmp.dt = copy(completeFBSnetwork.dt)
setnames(tmp.dt, c("Item.Code", "FBS.Parent.Code"),
         c("FBS.Parent.Code", "Item.Code"))
tmp.dt = tmp.dt[, colnames(completeFBSnetwork.dt), with = FALSE]
tmp.dt[, Default.Extraction.Rates := 1/Default.Extraction.Rates]
fullFBSnetwork.dt = rbind(completeFBSnetwork.dt, tmp.dt)

completeFBSnetwork.graph =
    graph.data.frame(
        d = fullFBSnetwork.dt[, list(Item.Code, FBS.Parent.Code)],
        vertices = nodesTable.dt
        )

E(completeFBSnetwork.graph)$extractRate =
    fullFBSnetwork.dt[, Default.Extraction.Rates]
E(completeFBSnetwork.graph)$conversionFactor =
    1/(fullFBSnetwork.dt[, Default.Extraction.Rates])
E(completeFBSnetwork.graph)$calorieOnly =
    as.logical(fullFBSnetwork.dt[, Use.Calorie])



standardization.graph = induced.subgraph(completeFBSnetwork.graph,
    V(fullFBSnetwork.graph)
    [which(is.finite(shortest.paths(fullFBSnetwork.graph,
                                    to = "2511")))]$name)


plot.igraph(standardization.graph,  vertex.color = "white",
            vertex.frame.color = "steelblue", vertex.size = 20,
            vertex.label = gsub(" |,", "\n",
                V(standardization.graph)$Item.Name),
            vertex.label.family = "sans", vertex.label.cex = 0.5,
            vertex.label.color = "steelblue",
            edge.arrow.size = 0.5,
            edge.label =
            round(E(standardization.graph)$conversionFactor, 4),
            edge.label.family = "sans",
            ## edge.color = ifelse(E(standardization.graph)$calorieOnly,
            ##     "red", "grey")
            edge.lty = ifelse(E(standardization.graph)$calorieOnly,
                2, 1),
            edge.arrow.mode = 1,
            edge.curved = 0.5,
            layout = layout.auto
            )


## This shows we can standardize to whatever equivalent we want
standardizeCode = 19
standardizeCodeName = "GERM WHEAT"
minConversionFactor = min(E(standardization.graph)$conversionFactor)
weightedLogDistance =
    shortest.paths(graph = standardization.graph,
                   to = as.character(standardizeCode),
                   weights =
                   log(E(standardization.graph)$conversionFactor/
                       minConversionFactor),
                   algorithm = "johnson")
physicalDistance = 
    shortest.paths(graph = standardization.graph,
                   to = as.character(standardizeCode),
                   algorithm = "johnson")
directDistance =
    exp(weightedLogDistance + physicalDistance *
        log(minConversionFactor))
standardized.dt =
    data.table(Item.Code = V(standardization.graph)$name,
               FBS.Parent.Code = standardizeCode,
               Item.Name = V(standardization.graph)$Item.Name,
               FBS.Parent.Name = standardizeCodeName,
               directDistance = c(directDistance))
## standardized.dt = standardized.dt[Item.Code != FBS.Parent.Code, ]

standardized.graph =
    graph.data.frame(
        d = standardized.dt, directed = TRUE,
        vertices = standardized.dt[, list(Item.Code, Item.Name)]
        )
standardized.graph[from = as.character(standardizeCode),
                   to = as.character(standardizeCode)] = 0
## V(standardized.graph)$Item.Name = standardized.dt[, Item.Name]
## This graph is a hack
plot.igraph(standardized.graph,  vertex.color = "white",
            vertex.frame.color = "steelblue", vertex.size = 20,
            vertex.label = gsub(" |,", "\n",
                V(standardized.graph)$Item.Name),
            vertex.label.family = "sans", vertex.label.cex = 0.5,
            vertex.label.color = "steelblue",
            edge.arrow.size = 0.5,
            edge.label =
            round(E(standardized.graph)$directDistance, 4),
            edge.label.family = "sans"
            )


## Tests for contract
V(standardized.graph)$size = rnorm(length(V(standardized.graph)$size))
comm.graph =
    contract.vertices(standardized.graph,
                      mapping = rep(1, length(V(standardized.graph)),
                          vertex.attr.comb=list(size="sum", "ignore"))
                      )
plot(comm.graph)
