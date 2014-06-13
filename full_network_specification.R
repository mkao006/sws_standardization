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
    V(completeFBSnetwork.graph)
    [which(is.finite(shortest.paths(completeFBSnetwork.graph,
                                    to = "2511")))]$name)

## Plot of the network assuming the FBS item 2511 is the root.
##
pdf(file = "wheat_network.pdf", width = 12, height = 9)
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
            round(E(standardization.graph)$conversionFactor, 4),
            edge.label.cex = 0.8,
            edge.label.family = "sans",
            edge.lty = ifelse(E(standardization.graph)$calorieOnly,
                2, 1),
            edge.arrow.mode = 1,
            edge.curved = 0.5,
            layout = layout.auto
            )
graphics.off()


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
## NOTE (Michael): If you are going to standardize to a non-FBS item,
##                 then you need to make sure that you treat the FBS
##                 item has all the attributes of non-FBS item such as
##                 "Use.Calorie"
standardized.dt =
    data.table(Item.Code = V(standardization.graph)$name,
               FBS.Parent.Code = standardizeCode,
               Item.Name = V(standardization.graph)$Item.Name,
               FBS.Parent.Name = standardizeCodeName,
               directDistance = c(directDistance),
               calorieOnly = c(completeFBSnetwork.dt[Item.Code %in%
                   as.numeric(V(standardization.graph)$name),
                   as.logical(Use.Calorie)], FALSE))

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
            edge.label.family = "sans",
            layout = layout.auto,
            edge.lty = ifelse(E(standardized.graph)$calorieOnly,
                2, 1),
            edge.curved = 1
            )




## Illustration of the aggregation process
## ---------------------------------------------------------------------


## (2) Read tree/network control file
##
## ---------------------------------------------------------------------
usa2009network.dt = data.table(read.csv(file = "usa_2009_network.csv",
  stringsAsFactors = FALSE))
usa2009network.dt[, finalExtractRate :=
                  ifelse(!is.na(Specific.Rates), Specific.Rates,
                         ifelse(!is.na(Default.Extraction.Rates),
                                Default.Extraction.Rates, NA))]
## usa2009network.dt[, finalExtractRate :=
##                   ifelse(!is.na(Default.Extraction.Rates),
##                          Default.Extraction.Rates, NA)]

subNetwork.dt =
    usa2009network.dt[, list(Item.Name, FBS.Parent.Name,
                             Default.Extraction.Rates, Specific.Rates)]

                     
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

usa2009network.dt[, finalExtractRate := finalExtractRate/10000]

tmp.dt = copy(usa2009network.dt)
setnames(tmp.dt, c("Item.Code", "FBS.Parent.Code"),
         c("FBS.Parent.Code", "Item.Code"))
tmp.dt = tmp.dt[, colnames(usa2009network.dt), with = FALSE]
tmp.dt[, finalExtractRate := 1/finalExtractRate]
fullUsa2009network.dt = rbind(usa2009network.dt, tmp.dt)

usa2009network.graph =
    graph.data.frame(
        d = fullUsa2009network.dt[, list(Item.Code, FBS.Parent.Code)],
        vertices = nodesTable.dt
        )

E(usa2009network.graph)$extractRate =
    fullUsa2009network.dt[, finalExtractRate]
E(usa2009network.graph)$conversionFactor =
    1/(fullUsa2009network.dt[, finalExtractRate])
E(usa2009network.graph)$calorieOnly =
    as.logical(fullUsa2009network.dt[, Use.Calorie])



standardization.graph = induced.subgraph(usa2009network.graph,
    V(usa2009network.graph)
    [which(is.finite(shortest.paths(usa2009network.graph,
                                    to = "2511")))]$name)

## Trade
trade.dt = data.table(read.csv(file = "usa_2009_trade.csv",
    stringsAsFactors = FALSE))
standardization.graph = 
    set.vertex.attribute(g = standardization.graph,
                         name = "export",
                         value = c(trade.dt[Item.Code %in%
                             as.numeric(V(standardization.graph)$name) &
                             Element.Code == 91,
                             Num_2009], 0))

standardization.graph = 
    set.vertex.attribute(g = standardization.graph,
                         name = "import",
                         value = c(trade.dt[Item.Code %in%
                             as.numeric(V(standardization.graph)$name) &
                             Element.Code == 61,
                             Num_2009], 0))

standardizeCode = 2511
standardizeCodeName = "WHEAT & PRODUCTS"
minConversionFactor = min(E(standardization.graph)$conversionFactor)
weightedLogDistance =
    shortest.paths(graph = standardization.graph,
                   to = as.character(standardizeCode),
                   weights =
                   log(E(standardization.graph)$conversionFactor/
                       minConversionFactor) *
                   as.numeric(!E(standardization.graph)$calorieOnly),
                   algorithm = "johnson")
physicalDistance = 
    shortest.paths(graph = standardization.graph,
                   to = as.character(standardizeCode),
                   algorithm = "johnson")
V(standardization.graph)$directDistance = 
    exp(weightedLogDistance + physicalDistance *
        log(minConversionFactor))
V(standardization.graph)$weightedExport =
    V(standardization.graph)$export *
    V(standardization.graph)$directDistance
V(standardization.graph)$weightedImport =
    V(standardization.graph)$import *
    V(standardization.graph)$directDistance

## Standardized export
standardization.graph =
    set.vertex.attribute(standardization.graph, "weightedExport",
                         index = V(standardization.graph)
                         [V(standardization.graph)$name ==
                          as.character(standardizeCode)],
                         value = 
                     sum(V(standardization.graph)$weightedExport))

standardization.graph =
    set.vertex.attribute(standardization.graph, "weightedImport",
                         index = V(standardization.graph)
                         [V(standardization.graph)$name ==
                          as.character(standardizeCode)],
                         value = 
                     sum(V(standardization.graph)$weightedImport))




## FBS: 23303, 4313
## default rate: 23499, 4632.58
## specific rate: 23612, 4627

## Plot of the network assuming the FBS item 2511 is the root.
##
pdf(file = "wheat_usa_2009_standardized_network.pdf",
    width = 12, height = 9)
plot.igraph(standardization.graph,
            vertex.color =
            ifelse(V(standardization.graph)$name == "2511",
                   "steelblue", "white"),
            vertex.frame.color =
            ifelse(V(standardization.graph)$name == "2511",
                   "darkblue", "steelblue"),
                   vertex.size = 20,
            vertex.label = paste0(gsub(" |,", "\n",
                V(standardization.graph)$Item.Name), "\n(Equivalence)",
                "\n Export: ",
                round(V(standardization.graph)$weightedExport/1000, 2),
                "\n Import: ",
                round(V(standardization.graph)$weightedImport/1000, 2)
                ),
            vertex.label.family = "sans", vertex.label.cex = 0.5,
            vertex.label.color = 
            ifelse(V(standardization.graph)$name == "2511",
                   "black", "steelblue"),            
            edge.arrow.size = 0.5,
            edge.label =
            round(E(standardization.graph)$conversionFactor, 4),
            edge.label.cex = 0.8,
            edge.label.family = "sans",
            edge.lty = ifelse(E(standardization.graph)$calorieOnly,
                2, 1),
            edge.arrow.mode = 1,
            edge.curved = 0.5,
            layout = layout.auto
            )
graphics.off()


## Now assume a distribution
standardization.graph =
    set.vertex.attribute(standardization.graph, "exportSd",
                         value = 0)
standardization.graph =
    set.vertex.attribute(standardization.graph, "importSd",
                         value = 0)

## Set the standard deviation for bread
standardization.graph =
    set.vertex.attribute(standardization.graph, "exportSd",
                         index = V(standardization.graph)[c("15", "20")],
                         value =
                         V(standardization.graph)[c("15", "20")]$export/10)

standardization.graph =
    set.vertex.attribute(standardization.graph, "importSd",
                         index = V(standardization.graph)[c("15", "20")],
                         value =
                         V(standardization.graph)[c("15", "20")]$import/10)





## Standardized standard deviation
standardization.graph =
    set.vertex.attribute(standardization.graph, "exportSd",
                         index = V(standardization.graph)
                         [V(standardization.graph)$name ==
                          as.character(standardizeCode)],
                         value = 
                     sqrt(sum((V(standardization.graph)$exportSd *
                         V(standardization.graph)$directDistance)^2)))
standardization.graph =
    set.vertex.attribute(standardization.graph, "importSd",
                         index = V(standardization.graph)
                         [V(standardization.graph)$name ==
                          as.character(standardizeCode)],
                         value = 
                     sqrt(sum((V(standardization.graph)$importSd *
                         V(standardization.graph)$directDistance)^2)))


## Plot of the network assuming the FBS item 2511 is the root.
##
pdf(file = "wheat_usa_2009_distribution_standardized_network.pdf",
    width = 12, height = 9)
plot.igraph(standardization.graph,
            vertex.color =
            ifelse(V(standardization.graph)$name == "2511",
                   "steelblue", "white"),
            vertex.frame.color =
            ifelse(V(standardization.graph)$name == "2511",
                   "darkblue", "steelblue"),
                   vertex.size = 20,
            vertex.label = paste0(gsub(" |,", "\n",
                V(standardization.graph)$Item.Name), "\n(Equivalence)",
                "\n Export: ",
                round(V(standardization.graph)$weightedExport/1000, 2),
                "\n Export SD: ",
                round(V(standardization.graph)$exportSd/1000, 2),
                "\n Import: ",
                round(V(standardization.graph)$weightedImport/1000, 2),
                "\n import SD: ",
                round(V(standardization.graph)$importSd/1000, 2)
                ),
            vertex.label.family = "sans", vertex.label.cex = 0.5,
            vertex.label.color = 
            ifelse(V(standardization.graph)$name == "2511",
                   "black", "steelblue"),            
            edge.arrow.size = 0.5,
            edge.label =
            round(E(standardization.graph)$conversionFactor, 4),
            edge.label.cex = 0.8,
            edge.label.family = "sans",
            edge.lty = ifelse(E(standardization.graph)$calorieOnly,
                2, 1),
            edge.arrow.mode = 1,
            edge.curved = 0.5,
            layout = layout.auto
            )
graphics.off()


## MCMC simulation for distribution
V(standardization.graph)$directDistance
V(standardization.graph)$name

rlogcauchy = function(n, mu, sigma){
    u = runif(n)
    x = mu + sigma*tan(pi*(u-.5))
    exp(x)
}

test = rgamma(n.sim, shape = 9, scale = 50000)
hist(test, breaks = 100)


library(MASS)
n.sim = 1000000
flourBulgur =
    mvrnorm(n = n.sim,
            mu = c(V(standardization.graph)["16"]$weightedExport,
                V(standardization.graph)["21"]$weightedExport),
            Sigma = matrix(c(50000, -4500, -4500, 1000), ncol = 2))
sim.matrix = matrix(c(
    c(runif(n.sim, 21000000,
            V(standardization.graph)["15"]$weightedExport)),
    flourBulgur[, 1],
    rep(V(standardization.graph)["17"]$weightedExport, n.sim),
    rep(V(standardization.graph)["18"]$weightedExport, n.sim),
    runif(n.sim, V(standardization.graph)["20"]$weightedExport, 300000),
    flourBulgur[, 2],
    rgamma(n.sim, shape = 9, scale = 50000), # 22
    rep(V(standardization.graph)["23"]$weightedExport, n.sim),
    rep(V(standardization.graph)["24"]$weightedExport, n.sim),
    rep(V(standardization.graph)["41"]$weightedExport, n.sim),
    rep(V(standardization.graph)["110"]$weightedExport, n.sim),
    rep(0, n.sim)), byrow = FALSE, nrow = n.sim)
    
posterior = sim.matrix %*% V(standardization.graph)$directDistance/1000
pdf(file = "wheat_mcmc_standardization.pdf", width = 12, height = 9)
hist(posterior, breaks = 100, freq = FALSE, xlim = c(21000, 27000),
     main = "")
abline(v = V(standardization.graph)["2511"]$weightedExport/1000,
       col = "tan4", lwd = 3, lty = 2)
curve(dnorm(x,
            mean = V(standardization.graph)["2511"]$weightedExport/1000,
            sd = V(standardization.graph)["2511"]$exportSd/1000),
      add = TRUE, col = "tan4", lwd = 2)
graphics.off()
mean(posterior)




test = simplify(contract.vertices(standardization.graph,
    mapping = rep(1, length(V(standardization.graph))),
    vertex.attr.comb = list(weightedExport = "sum"))


get.edgelist(standardization.graph)

standardized.graph =
    contract.vertices(standardization.graph,
                      mapping = rep(1, length(V(standardization.graph))),
                          vertex.attr.comb=list(export="sum", "ignore")
                      )


standardized.graph = simplify(standardized.graph, remove.loops=FALSE,
                       edge.attr.comb=list(export="sum", name ="ignore"))


plot(standardized.graph)



## Plot of the network assuming the FBS item 2511 is the root.
##
## pdf(file = "wheat_usa_2009_network.pdf", width = 12, height = 9)
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
            round(E(standardization.graph)$conversionFactor, 4),
            edge.label.cex = 0.8,
            edge.label.family = "sans",
            edge.lty = ifelse(E(standardization.graph)$calorieOnly,
                2, 1),
            edge.arrow.mode = 1,
            edge.curved = 0.5,
            layout = layout.auto
            )
## graphics.off()



            
## Tests for contract
V(standardized.graph)$size = rnorm(length(V(standardized.graph)))
comm.graph =
    contract.vertices(standardized.graph,
                      mapping = rep(1, length(V(standardized.graph))),
                          vertex.attr.comb=list(size="sum", "ignore")
                      )

comm.graph = simplify(comm.graph, remove.loops=FALSE,
                       edge.attr.comb=list(size="sum", name ="ignore"))


plot(comm.graph)



