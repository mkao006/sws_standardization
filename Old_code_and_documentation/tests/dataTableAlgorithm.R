########################################################################
## Title: This is an implementation fully based on data.table
## Date: 2014-07-30
########################################################################

library(data.table)
library(igraph)
library(faoswsUtil)
library(FAOSTAT)

currentYear = 2010

defaults.dt =
    data.table(
        read.csv(file = "usa_demo_network.csv",
                 stringsAsFactors = FALSE
                 )
        )

## NODE TABLE
## ----------
##
## Read the nodes table from the default network which take only the
## item code, default extraction rate and whether it is a target or
## not. The default rate is modified by the new country specific
## extraction rate from SUA.
##
## Variables:
## * Item.Code
## * Item.Name (Maybe this should be taken from the SUA.)
## * The final FBS codes.
## * Default.Extraction.Rate
## * Aggregate.Method
## * Weight
## * Target (T, B, F etc...)
## * Use.Calorie
## * Specfic rates from SUA.

initNodes.dt =
    defaults.dt[, list(Item.Code, Item.Name, Default.Extraction.Rates,
                       Aggregate.Method, Weight, Target, Use.Calorie
                       )
                ]

extractRate.dt =
    data.table(
        read.csv(file = "usa_demo_extract_data_full.csv",
                 stringsAsFactors = FALSE
                 )
        )

extractRate.dt = extractRate.dt[Year == currentYear, ]

## Where extraction rate is zero and is calculated then assign zero,
## if missing with M and zero then assign NA.
extractRate.dt[(Num == 0 & Symb == "C") | Symb == "M", Num := NA]

nodes.dt =
    merge(
        initNodes.dt, extractRate.dt[, list(Item.Code, Num)],
        by = "Item.Code",
        all = TRUE
        )


nodes.dt[,
         finalExtractionRate :=
         ifelse(Aggregate.Method %in% c("T", "X") |
                (Aggregate.Method == "B" & Weight == 0), 10000,
                ifelse(!is.na(Num), Num,
                       ifelse(!is.na(Default.Extraction.Rates),
                              Default.Extraction.Rates, NA
                              )
                       )
                )
         ]

nodes.dt[, Default.Extraction.Rates := NULL]
nodes.dt[, Num := NULL]

targets =
    nodes.dt[Target == "Target" |
             (Aggregate.Method == "B" & Weight == 0),
             Item.Code]



## EDGE TABLE
## ----------
##
## This will consist of two components, the first will be the default
## edges from the default network. Then the input file will modify the
## edges. The share will also contain information such as shares.
##
## Variables:
## * Child
## * Parent
## * Shares (reciprocal calculated from input)

initEdge.dt = defaults.dt[, list(Item.Code, FBS.Parent.Code)]
setnames(initEdge.dt, c("Item.Code", "FBS.Parent.Code"),
         c("child", "parent"))
initEdge.dt[, reciprocalShare := as.numeric(NA)]

inputEdge.dt =
    data.table(
        read.csv(file = "usa_demo_input_data.csv",
                 stringsAsFactors = FALSE
                 )
        )

## Removing inputs which are zero, since there is no need to
## standardize and also because the reciprocal share will be infinity.
inputEdge.dt =
    inputEdge.dt[Year == currentYear & !(Num == 0), ]

inputEdge.dt =
    inputEdge.dt[Year == currentYear,
                 list(Child.Item.Code, Item.Code, Num)
                 ]

## Remove input edge which are targets
inputEdge.dt =
    inputEdge.dt[!Child.Item.Code %in% targets, ]

## Calculate reciprocal share
inputEdge.dt[, reciprocalShare := Num/sum(Num, na.rm = TRUE),
             by = "Child.Item.Code"]
inputEdge.dt[, Num := NULL]
setnames(inputEdge.dt, c("Child.Item.Code", "Item.Code"),
         c("child", "parent"))

## union the two sets of edges
edgeUnion.dt = rbind(initEdge.dt[!child %in% inputEdge.dt$child, ],
    inputEdge.dt)


edges.dt =
    edgeUnion.dt[, list(reciprocalShare = sumWithNA(reciprocalShare)),
                 by = c("child", "parent")
                 ]

## If we can not obtain the share from the inputs table, then we
## assign it as 1.
edges.dt[is.na(reciprocalShare), reciprocalShare := as.numeric(1)]




## STANDARDIZATION TABLE
## ---------------------

tmp = nodes.dt[, list(Item.Code, finalExtractionRate)]
setnames(tmp, "Item.Code", new = "child")

graph.dt = merge(edges.dt, tmp, by = "child")

## Remove if the parent code is zero, it is a placeholder for no
## standardization.
graph.dt = graph.dt[parent != 0, ]

## Remove entry with extraction is zero.
graph.dt = graph.dt[finalExtractionRate != 0, ]

## Compute the final rate to apply
graph.dt[, finalWeight :=
                   reciprocalShare/(finalExtractionRate/10000)
                   ]

graph.dt[, `:=`(c("reciprocalShare", "finalExtractionRate"),
                          NULL)]

## Extract final FBS targets
fbsTargets = na.omit(unique(defaults.dt$FBS.Code))

graph.dt =
    rbind(
        graph.dt,
        data.table(child = fbsTargets, parent = fbsTargets,
                   finalWeight = 1
                   )
        )

## Start standardization
leave.dt = copy(graph.dt)
setnames(leave.dt, c("child", "parent", "finalWeight"),
         c("leaves", "inter", "directWeight"))



root.dt = copy(graph.dt)
setnames(root.dt, c("parent", "child"),
         c("root", "inter"))




## Initiate
leave.dt = merge(leave.dt, root.dt, by = "inter", all.x = TRUE,
    allow.cartesian = TRUE)
leave.dt[, directWeight := directWeight * finalWeight]
leave.dt[, `:=`(c("inter", "finalWeight"), NULL)]
setnames(leave.dt, "root", "inter")
iter = 1

while(!all(leave.dt$inter %in% fbsTargets)){
    if(any(is.na(leave.dt$inter))){
        print("Some leaves are terminated and removed")
        leave.dt = leave.dt[!is.na(inter), ]
    }
    iter = iter + 1
    leave.dt = merge(leave.dt, root.dt, by = "inter", all.x = TRUE,
        allow.cartesian = TRUE)
    leave.dt[, directWeight := directWeight * finalWeight]
    leave.dt[, `:=`(c("inter", "finalWeight"), NULL)]
    if(all(leave.dt$root %in% fbsTargets))
        break
    setnames(leave.dt, "root", "inter")
}

## Check which one has multiple path.

directWeight.dt =
    leave.dt[, list(directWeight = sumWithNA(directWeight)),
             by = c("leaves", "root")
             ]
setkeyv(directWeight.dt, c("leaves", "root"))
directWeight.dt = directWeight.dt[leaves != root, ]

## This is the post processing rate which I don't know which step it
## is applied and how it is applied
## -----------------------------------------------------------------
##
## postProcessWeight.dt =
##     data.table(
##         read.csv(file = "postProcessWeight.csv",
##                  stringsAsFactors = FALSE,
##                  header = FALSE
##                  )
##         )
## setnames(postProcessWeight.dt, c("V1", "V2", "V3"),
##          c("root", "leaves", "postProcessWeight"))
## finalWeights.dt =
##     merge(directWeight.dt,
##           postProcessWeight.dt,
##           by = c("leaves", "root"),
##           all.x = TRUE
##           )
## finalWeights.dt[is.na(postProcessWeight), postProcessWeight := 1]
## ## finalWeights.dt[, directWeight := directWeight * postProcessWeight]
## ## finalWeights.dt[, postProcessWeight := NULL]

         

## Perform the standardization
sua.dt =
    data.table(
        read.csv(file = "usa_demo_sua_data_full.csv",
                 stringsAsFactors = FALSE
                 )
        )
sua.dt = sua.dt[Year == currentYear, ]

food.dt = sua.dt[Element.Code == 141, list(Item.Code, Num)]
setnames(food.dt, c("Item.Code", "Num"), c("leaves", "food"))

standardization.dt =
    merge(food.dt, directWeight.dt, by = "leaves", all = TRUE)

## postStandardization.dt =
##     merge(standardization.dt, postProcessWeight.dt,
##           by = c("root"),
##           all = TRUE)
## postStandardization.dt[is.na(postProcessWeight),
##                        postProcessWeight := 1]


foodStandardized.dt =
    standardization.dt[!is.na(root),
                       sum(food * directWeight,
                           na.rm = TRUE)/1000,
                       by = "root"]
setnames(foodStandardized.dt, c("root", "V1"),
         c("itemCode", "standardizedValue"))




## Check
load("FBSfoodQuery.RData")
FBSdisseminate = getFAOtoSYB(query = FBSfoodQuery,
    outputFormat = "long", yearRange = 2000:2011)


foodCheck.dt =
    data.table(
       FBSdisseminate$entity[FBSdisseminate$entity$Year == currentYear & 
                              FBSdisseminate$entity$FAOST_CODE == 231,
                              c("itemCode", "name", "Value")]
        )
foodCheck.dt[, itemCode := as.numeric(itemCode)]
foodCheck.dt[, itemName :=
             gsub("Food Balance Sheets_|_Food\\(1000 tonnes\\)", "",
                  name)]
foodCheck.dt[, name := NULL]

compare.dt = merge(foodStandardized.dt, foodCheck.dt, by = "itemCode",
    all = TRUE)
compare.dt = compare.dt[!is.na(Value), ]
compare.dt[, pctDiff :=
           abs(Value - round(standardizedValue))/Value * 100]
compare.dt[standardizedValue == 0 & Value == 0, pctDiff := 0]
## Percet difference distribution
compare.dt[, range(pctDiff, na.rm = TRUE, finite = TRUE)]

## Percentage of difference unacceptable, defined as more than 5%
table(compare.dt[is.finite(pctDiff) & !is.na(pctDiff) &
                 !itemCode %in% c(2761:2775), pctDiff < 5])

## There are the records which has a percentage difference greater
## than 5%.
##
## ** Need to check Maize
## ** The problem with Nuts is due to missing conversion to shelled
## ** Oil is as normal, still checking
## ** Vegetable and fruits are still under investigation
## ** Fats and ghee is also missing conversion
## ** Fish is not required.
## ** Rice is not milled equivalent
## ** Barley is calculated incorrectly in the old system

compare.dt[is.finite(pctDiff) &
           !itemCode %in% c(2761:2775) &
           pctDiff >= 5, ]




## Need to change the post-processing rates, which are given in the
## CBDstep1.list.csv.

## The difference in Barley comes from the fact that peral barley is
## using the rate of pot barley.

## No idea about pineapple juice


## The difference in barley (44)is due to the fact that there is
## weights calculated for barley Pearled (46) and barley flour and
## grit (48) into pot barley (45). However, this is wrong since there
## are no item in barley flour and grit (48), so the weight is
## essentially under-counting barley pearl. Equation 3.15 on page 12
## is not satisfied if one of the Beta in equation 3.14 is missing.
