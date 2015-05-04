########################################################################
## Title: Demo for the standardization framework
## Date: 2014-06-17
########################################################################

## NOTE (Michael): Need to get population data for per capita supply.
##
## NOTE (Michael): The potential differences in the computation and
##                 FBS may be that of the share.

## NOTE (Michael): Item worth investigation.
##
##     Sweetner2543 (small) - the difference is maple sugar which is
##                            marked "X"
##
##     groundnut2556 (medium) - I think the tree from Rafik and Marten
##                              is wrong, the ground nut shelled (243)
##                              should be the target, not groung nut
##                              (242).
##
##     whole oil group (medium) - I think this is country specific
##                                standardization. Margarine +
##                                shortening is aggregated into oil
##                                crops oil, other (2586) but the FBS
##                                on faostat doesn't appear to do so.
##
##     vegetablesother2605 (large)
##
##     apples and product2617 (large) - This is fixed, the reason was
##                                      due to the fact that the item
##                                      apple juice concentrated had
##                                      extraction rate of zero from
##                                      computation. Removing this and
##                                      use the default rate gives the
##                                      right number.
##
##     pineaple and prod 2618 (medium) - Extraction rates are default,
##                                       does not appear to be
##                                       difference in item. No clue.
##
##     fruits other 2625 (medium) - Looks like USA excludes watermelon
##                                  and melon in this group.
##
##     tea 2635 (small) - All of tea were marked "X", same as sweetner.
##
##     butter ghee 2740 (large) - If we don't apply extraction rate,
##                                then the number will be correct.
##
##     Rice 2805 (medium) - Milled rice (31) should be a target, since
##                          the FBS item (2805) is milled
##                          equivalent. Thus no extraction should be
##                          applied. However, the amount of milled
##                          rice (31) is still higher than the FBS.

## load libraries
library(data.table)
library(igraph)
library(reshape2)
currentYear = 2009
target = as.character(sort(read.csv(file = "fbs_table.csv")$FBS.Code))
plotGraph = TRUE
standardizeElementCode = c(51, 61, 71, 91, 141, 101, 111, 121, 151)
standardizeElementName = c("Production", "Import", "DStock", "Export",
            "Food", "Feed", "Seed", "Waste", "Other Util")

standardizedElements.df =
    data.frame(standardizeElementCode =
               c(51, 61, 71, 91, 141, 101, 111, 121, 151),
               standardizeElementName = c("Production", "Import",
                   "DStock", "Export", "Food", "Feed", "Seed", "Waste",
                   "Other Util"))


## Read the graph for usa
usaNetwork.dt = data.table(read.csv(file = "usa_demo_network.csv",
  stringsAsFactors = FALSE))

## Read specific extraction rate
extract.dt = data.table(read.csv(file = "usa_demo_extract_data_full.csv",
    stringsAsFactors = FALSE))

## Change the extraction rates to 10000 if the item is a target
## extract.dt[grepl("Yield", Element.Name), Num := 10000]
## targets = usaNetwork.dt[Aggregate.Method == "T", Item.Code]
## extract.dt[Item.Code %in% targets, Num := 10000]

## This is a hack to fix the zero extraction rate when calculated
extract.dt[Num == 0 & Symb == "C", Num := NA]

## Merge and obtain the final extraction rates
usaNetwork.dt = merge(usaNetwork.dt,
    extract.dt[Year == currentYear, list(Item.Code, Num)],
    by = "Item.Code", all.x = TRUE)
setnames(usaNetwork.dt, old = "Num", new = "Specific.Rates")
usaNetwork.dt[, finalExtractRate :=
              ifelse(Aggregate.Method == "T", 10000,
                     ifelse(Aggregate.Method == "X", 0,
                            ifelse(!is.na(Specific.Rates),
                                   Specific.Rates,
                                   ifelse(!is.na(Default.Extraction.Rates),
                                          Default.Extraction.Rates, NA))))]


## Read the input file
input.dt = data.table(read.csv(file = "usa_demo_input_data.csv",
    stringsAsFactors = FALSE))
input.dt = input.dt[Year == currentYear, ]

input.dt[, reverse_share := Num/sum(Num, na.rm = TRUE),
         by = "Child.Item.Code"]
input.dt[!is.finite(reverse_share), reverse_share := 0]

## This create the vertex table, need to think about how to handle the
## fcl and fbs classification together.
tmp0 = usaNetwork.dt[, list(Item.Code, Item.Name)]
tmp1 = input.dt[, list(Item.Code, Item.Name)]
tmp2 = input.dt[, list(Child.Item.Code, Child.Item.Name)]
setnames(tmp2, old = c("Child.Item.Code", "Child.Item.Name"),
         new = c("Item.Code", "Item.Name"))
itemTable.dt = unique(rbind(tmp0, tmp1, tmp2))
itemTable.dt[, unique_name := Item.Name[1], by = "Item.Code"]
itemTable.dt[, Item.Name := unique_name]
itemTable.dt[, unique_name := NULL]
itemTable.dt = unique(itemTable.dt)
itemTable.dt[, FBS := FALSE]
fbsTable.dt = 
    unique(usaNetwork.dt[!(FBS.Parent.Code %in% Item.Code),
                                 list(FBS.Parent.Code, FBS.Parent.Name)])
setnames(fbsTable.dt, old = c("FBS.Parent.Code", "FBS.Parent.Name"),
         new = c("Item.Code", "Item.Name"))
fbsTable.dt[, FBS := TRUE]
nodesTable.dt = unique(rbind(itemTable.dt, fbsTable.dt))


## Devide the extraction rate by 10000 so it becomes a standard
## expression
usaNetwork.dt[, finalExtractRate := finalExtractRate/10000]

defaultNetwork.dt = usaNetwork.dt[, list(Item.Code, FBS.Parent.Code)]
setnames(defaultNetwork.dt, old = c("Item.Code", "FBS.Parent.Code"),
         new = c("child", "parent"))
inputNetwork.dt = input.dt[reverse_share != 0,
    list(Child.Item.Code, Item.Code)]
setnames(inputNetwork.dt, old = c("Child.Item.Code", "Item.Code"),
         new = c("child", "parent"))
finalNetwork.dt = unique(rbind(defaultNetwork.dt, inputNetwork.dt))

rates.dt = usaNetwork.dt[, list(Item.Code, finalExtractRate,
    Use.Calorie)]
setnames(rates.dt,
         old = c("Item.Code", "finalExtractRate", "Use.Calorie"),
         new = c("child", "weight", "calorieOnly"))
finalNetwork.dt = merge(finalNetwork.dt, rates.dt, by = "child")
setnames(finalNetwork.dt, old = c("child", "parent"),
         new = c("parent", "child"))


## create the graph
usaNetwork.graph =
    graph.data.frame(
        d = finalNetwork.dt,
        vertices = nodesTable.dt
        )

getSubgraph = function(terminalNode, graph){
    dist = shortest.paths(graph, to = terminalNode, weights = NA,
        mode = "out")
    subsetNodes = names(dist[which(is.finite(dist)), ])
    induced.subgraph(graph, v = subsetNodes)
}

## Function for adding the reverse edge with weights
add.reverse.edges = function(graph, ..., attr = list()){
    current.edges = get.edgelist(graph)
    add.edges(graph, edges = c(rev(t(current.edges))),
              attr = list(weight = rev(1/E(graph)$weight),
                  calorieOnly = rev(E(graph)$calorieOnly)), ...)
}

## The final full graph
standardization.graph = usaNetwork.graph
## standardization.graph = add.reverse.edges(usaNetwork.graph)
## standardization.graph = getSubgraph("2805", standardization.graph)





## Read the SUA data
sua.dt = data.table(read.csv(file = "usa_demo_sua_data_full.csv",
    stringsAsFactors = FALSE))
sua.dt = sua.dt[Year == currentYear, ]

## Read nutrient data, inspect why there is food in FBS selection
nutrient.dt =
    data.table(read.csv(file = "usa_demo_nutrient_data_full.csv",
    stringsAsFactors = FALSE))
nutrient.dt = nutrient.dt[Year == currentYear & Element.Code != 141, ]

fbs.dt = rbind(sua.dt, nutrient.dt)


## Assign attributes to the vertex
for(i in standardizeElementCode){
    standardization.graph =
        with(fbs.dt[Element.Code == i &
                    as.character(Item.Code) %in%
                    V(standardization.graph)$name, ],
             set.vertex.attribute(standardization.graph,
                                  name = as.character(i),
                                  index = V(standardization.graph)[as.character(Item.Code)],
                                  value = Num))
}



## Function for computing the direct distance to a certain commodity
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

## Compute the direct weights
directWeights = computeDirectWeight(standardization.graph,
    target = target,
    weights = 1/(E(standardization.graph)$weight),
    calorieOnly = as.numeric(!E(standardization.graph)$calorieOnly))


## Compute the whole FBS
directWeights = ifelse(is.finite(directWeights), directWeights, 0)

## Compute the quantity of the FBS
suaCast.mat =
    data.matrix(dcast(sua.dt[, list(Item.Code, Element.Code, Num)],
                      Item.Code ~ Element.Code, value.var = "Num"))
rownames(suaCast.mat) = suaCast.mat[, 1]
suaCast.mat = suaCast.mat[, -1]
suaCast.mat[is.na(suaCast.mat)] = 0
suaWeightsIntersect =
    intersect(rownames(suaCast.mat), rownames(directWeights))
FBSQuantity =
    t(t(suaCast.mat[suaWeightsIntersect,
                    as.character(standardizeElementCode)]) %*%
    directWeights[suaWeightsIntersect, ]/1000)
colnames(FBSQuantity) = standardizeElementName

## Compute the per capita supply of nutrients and calorie
nutrientCast.mat =
    data.matrix(dcast(nutrient.dt[, list(Item.Code, Element.Code, Num)],
                      Item.Code ~ Element.Code, value.var = "Num"))
rownames(nutrientCast.mat) = nutrientCast.mat[, 1]
nutrientCast.mat = nutrientCast.mat[, -1]
nutrientCast.mat[is.na(nutrientCast.mat)] = 0
nutrientWeightsIntersect =
    intersect(rownames(suaCast.mat), rownames(nutrientCast.mat))
FBSCalorie = t(t(suaCast.mat[nutrientWeightsIntersect, ]) %*%
    ((directWeights[nutrientWeightsIntersect, ] != 0) *
     nutrientCast.mat[nutrientWeightsIntersect, "261"]))[, "141"]/
    30949.2/365
FBSProtein = t(t(suaCast.mat[nutrientWeightsIntersect, ]) %*%
    ((directWeights[nutrientWeightsIntersect, ] != 0) *
     nutrientCast.mat[nutrientWeightsIntersect, "271"]))[, "141"]/
    30949.2/365/10
FBSFat = t(t(suaCast.mat[nutrientWeightsIntersect, ]) %*%
    ((directWeights[nutrientWeightsIntersect, ] != 0) *
     nutrientCast.mat[nutrientWeightsIntersect, "281"]))[, "141"]/
    30949.2/365/10
FBS = cbind(FBSQuantity, FBSCalorie, FBSProtein, FBSFat)

## Plot of the network assuming the FBS item 2511 is the root.
if(plotGraph){
    pdf(file = "usa_demo_standardization_graph.pdf",
        width = 12, height = 9)
    for(fbsItem in sort(na.omit(unique(usaNetwork.dt$FBS.Code)))){
        sub.graph = getSubgraph(as.character(fbsItem),
            standardization.graph)
        plot.igraph(sub.graph,
                    vertex.color =
                    ifelse(V(sub.graph)$name %in% target,
                           "steelblue", "white"),
                    vertex.frame.color =
                    ifelse(V(sub.graph)$name %in% target,
                           "darkblue", "steelblue"),
                    vertex.size = 20,
                    vertex.label = paste0(gsub(" |,", "\n",
                        V(sub.graph)$Item.Name), "\n",
                        ifelse(is.na(V(sub.graph)$`141`), 0,
                               V(sub.graph)$`141`)),
                    vertex.label.family = "sans",
                    vertex.label.cex = 0.5,
                    vertex.label.color = 
                    ifelse(V(sub.graph)$name %in% target,
                           "white", "steelblue"),            
                    edge.arrow.size = 0.5,
                    edge.label =
                    round(E(sub.graph)$weight, 4),
                    edge.label.cex = 0.8,
                    edge.label.family = "sans",
                    edge.lty =
                    ifelse(E(sub.graph)$calorieOnly, 2, 1),
                    edge.arrow.mode = 1,
                    edge.curved = 0.5,
                    layout = layout.auto
                    )
    }
    graphics.off()
}

check.graph = getSubgraph("2571", standardization.graph)
pdf(file = "check_network.pdf", width = 20, height = 15)
plot.igraph(check.graph,
            vertex.color =
            ifelse(V(check.graph)$name %in% target,
                   "steelblue", "white"),
            vertex.frame.color =
            ifelse(V(check.graph)$name %in% target,
                   "darkblue", "steelblue"),
            vertex.size = 10,
            vertex.label = paste0(gsub(" |,", "\n",
                V(check.graph)$Item.Name), "\n",
                ifelse(is.na(V(check.graph)$`111`), 0,
                       V(check.graph)$`111`)),
            vertex.label.family = "sans",
            vertex.label.cex = 0.5,
            vertex.label.color = 
            ifelse(V(check.graph)$name %in% target,
                   "white", "steelblue"),            
            edge.arrow.size = 0.5,
            edge.label =
            round(E(check.graph)$weight, 4),
            edge.label.cex = 0.8,
            edge.label.family = "sans",
            edge.lty =
            ifelse(E(check.graph)$calorieOnly, 2, 1),
            edge.arrow.mode = 1,
            edge.curved = 0.5,
            layout = layout.auto
            )
graphics.off()




## Output the file for examination
write.csv(FBS[as.character(sort(as.numeric(rownames(FBS)))), ],
          file = "usa_demo_fbs.csv", na = "")



## This was for individual groups
## ---------------------------------------------------------------------

## V(standardization.graph)[rownames(directWeights)]$directWeight =
##     ifelse(is.finite(directWeights), directWeights, 0)

## ## Standardization, trades are slightly off need to investigate why.
## sum(V(standardization.graph)$`61` * 
##     V(standardization.graph)$directWeight, na.rm = TRUE)/1000
## sum(V(standardization.graph)$`71` *
##     V(standardization.graph)$directWeight, na.rm = TRUE)/1000
## sum(V(standardization.graph)$`91` *
##     V(standardization.graph)$directWeight, na.rm = TRUE)/1000
## sum(V(standardization.graph)$`141` *
##     V(standardization.graph)$directWeight, na.rm = TRUE)/1000
## sum(V(standardization.graph)$`101` *
##     V(standardization.graph)$directWeight, na.rm = TRUE)/1000
## sum(V(standardization.graph)$`111` *
##     V(standardization.graph)$directWeight, na.rm = TRUE)/1000

## ## NOTE (Michael): Need to obtain the data for population


## ## Kg/Yr/caput
## sum(V(standardization.graph)$`141` *
##     V(standardization.graph)$directWeight, na.rm = TRUE)/309492

## ## g/day/caput
## sum(V(standardization.graph)$`141` *
##     V(standardization.graph)$directWeight, na.rm = TRUE)/309492/365 *
##     1000

## ## Kcal/day/caput
## ## This is wrong
## sum(V(standardization.graph)$`141`/100 * 
##     V(standardization.graph)$`261` *
##     V(standardization.graph)$directWeight/309492/365 * 1000,
##     na.rm = TRUE)

## ## This should be correct
## sum(V(standardization.graph)$`141` * 1000 * 10 * 
##     V(standardization.graph)$`261` *
##     as.numeric(V(standardization.graph)$directWeight != 0),
##     na.rm = TRUE)/309492000/365
