suppressMessages({
    library(faosws)
    library(faoswsUtil)
    library(data.table)
    library(magrittr)
    library(reshape2)
    library(igraph)
})

R_SWS_SHARE_PATH = Sys.getenv("R_SWS_SHARE_PATH")
DEBUG_MODE = Sys.getenv("R_DEBUG_MODE")

## Set up testing environments
if(!exists("DEBUG_MODE") || DEBUG_MODE == ""){
    GetTestEnvironment(
        ## baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
        token = "d91934e8-6bf9-4f44-b051-504a18c885fc"
        )
}

fclcpcMapping = GetTableData(schemaName = "ess", tableName = "fcl_2_cpc")


## This default standardization tree was taken from the old system
## provided by Rafik.
dataUrl = RCurl::getURL("https://raw.githubusercontent.com/mkao006/sws_standardization/master/item_tree_final.csv")
oldCommodityTree = fread(dataUrl)

## Remove multiple parents of SEED COTTON (328)
oldCommodityTree =
    oldCommodityTree[!(itemCode == 328 & targetCode %in% c(767, 769)), ]

## Remove White Maize (67) and also pop corn (68) which are obsolete
## items and also not mapped uniquely in cpc.
oldCommodityTree =
    oldCommodityTree[!itemCode %in% c(67, 68), ]

## Append CPC code to the old commodity tree
setnames(oldCommodityTree, old = c("itemCode", "targetCode"),
         new = c("fcl_children", "fcl_parent"))
fclcpcMapping[, fcl_numeric := as.numeric(fcl)]

## Fill in cpc codes
oldCommodityTree[, cpc_children_code:=
                     fclcpcMapping[match(oldCommodityTree$fcl_children,
                                         fclcpcMapping$fcl_numeric), cpc]]
oldCommodityTree[, cpc_parent_code:=
                     fclcpcMapping[match(oldCommodityTree$fcl_parent,
                                         fclcpcMapping$fcl_numeric), cpc]]

## Read in fbs default composition file
# dataUrl = RCurl::getURL("https://raw.githubusercontent.com/mkao006/sws_standardization/master/item_tree_final.csv")
# oldCommodityTree = fread(dataUrl)
fbsComposition =
    data.table(read.csv(file = "default_fbs_composition.csv"))
## setnames(fbsComposition, old = "fcl_code", new = "fcl_children")

## Fill in default fbs composition
oldCommodityTree[, fbs_code :=
                   fbsComposition[match(oldCommodityTree$fcl_children,
                                        fbsComposition$fcl_code), fbs_code]]

oldCommodityTree[, fbs_name :=
                   fbsComposition[match(oldCommodityTree$fcl_children,
                                        fbsComposition$fcl_code), fbs_name]]



## Subset only the CPC component
cpcCommodityTree =
    oldCommodityTree[!is.na(cpc_children_code) &
                         !is.na(cpc_parent_code),
                         list(cpc_children_code, cpc_parent_code,
                              fbs_code, fbs_name)]


## Read in the item table for the item description
itemTable =
    GetCodeList(domain = "agriculture",
                dataset = "agriculture",
                dimension = "measuredItemCPC")


## Fill in the description based on the item table.
cpcCommodityTree[, cpc_children_name :=
                     itemTable[match(cpcCommodityTree$cpc_children,
                                     itemTable$code), description]]
cpcCommodityTree[, cpc_parent_name :=
                     itemTable[match(cpcCommodityTree$cpc_parent,
                                     itemTable$code), description]]


## Read the new fbs code to standardize group mapping
dataUrl = RCurl::getURL("https://raw.githubusercontent.com/mkao006/sws_standardization/master/reconstructed_tree_cpc/fbs_stnd_item_map.csv")
fbsStndGroupMapping = fread(dataUrl)
setnames(fbsStndGroupMapping,
         old = c("FCL", "CPC_ST"),
         new = c("fbs_code", "cpc_standardized_code"))

fbsOneToOneMap = 
    fbsStndGroupMapping[!(fbs_code == "2701" & cpc_standardized_code == "S02111") &
                        !(fbs_code == "2855" & cpc_standardized_code == "S21233") &
                        !(fbs_code == "2512" & cpc_standardized_code == "S0113") &
                        !(fbs_code == "2912" & cpc_standardized_code == "S0137") &
                        !(fbs_code == "2829" & cpc_standardized_code == "S0142") &
                        !(fbs_code == "2827" & cpc_standardized_code == "S2351") &
                        !(cpc_standardized_code == "S2152"), ]

cpcCommodityTreeFinal =
    merge(cpcCommodityTree, fbsOneToOneMap, by = "fbs_code", all.x = TRUE)
setcolorder(cpcCommodityTreeFinal,
            neworder = c("cpc_children_code", "cpc_children_name",
                "cpc_parent_code", "cpc_parent_name", "fbs_code", "fbs_name",
                "cpc_standardized_code"))

write.csv(cpcCommodityTreeFinal[order(cpc_children_code), ],
          file = "cpcCommodityTreeReconstructed.csv", na = "", row.names = FALSE)
