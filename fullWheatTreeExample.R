########################################################################
## Title: Using the full data for wheat as an example
## Date: 2013-05-08
########################################################################

## Load the libraries
library(data.table)
library(reshape2)

## Read the SUA data
fw.df = read.csv("fullWheatExample.csv", header = TRUE,
  stringsAsFactors = FALSE)
mfw.df = melt(fw.df, id.var = c("Area.Code", "Item.Code", "Item.Name",
                       "Element.Code"))
mfw.df = mfw.df[grepl("Num", mfw.df$variable), ]
mfw.df$year = as.integer(gsub("[^0-9]{4}", "", mfw.df$variable))
mfw.df$variable = NULL
cmfw.df = dcast(mfw.df, Area.Code + Item.Code + Item.Name + year ~ Element.Code,
  value.var = "value")
final.dt = data.table(cmfw.df)
setnames(final.dt, colnames(cmfw.df),
         new = c("areaCode", "itemCode", "itemName", "year", "extRate",
           "production", "import", "export"))
final.dt[, extRate := as.numeric(extRate)]
final.dt[, production := as.numeric(production)]
final.dt[, import := as.numeric(import)]
final.dt[, export := as.numeric(export)]


## Load the tree
## ---------------------------------------------------------------------

fullTree.dt = data.table(read.csv(file = "item_tree_raw.csv", header = TRUE,
    stringsAsFactors = FALSE))
fullTree.dt[, FirstNam := NULL]
fullTree.dt[, TargNAm := NULL]
setnames(fullTree.dt, old = colnames(fullTree.dt),
         new = c("itemCode", "targetItemCode", "baseExt", "fbsCode", "fbsName"))

test = merge(final.dt, fullTree.dt, by = "itemCode", all.x = TRUE)


names <- c("a", "b", "c")

library(igraph)


test.dt = data.matrix(fullTree.dt[1:6, list(itemCode, targetItemCode)])
test.dt[2, 2] = 16
test.graph = graph(t(test.dt), n = 6)
plot(test.graph)


## Pseudo codes
## (1) Feed in the sub-tree for each commodity
## (2) compute the nearest distance to the primary commodity
## (3) For each level, compute the cumprod extraction rate until the
##     first level.
## (4) apply the extraction rate to each individual item, than
##     aggregate to complete the pre-standardization


