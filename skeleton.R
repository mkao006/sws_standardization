########################################################################
## Title: Imputation and validation methodologies for the FAOSTAT
##        production domain.
## Date:  2013-03-12
########################################################################

library(FAOSTAT)
library(reshape2)
library(zoo)


## Commodity group data take from the SAS script
##---------------------------------------------------------------------

## TODO (Michael): Check which commodity group does 461 belong to
##                 (currently in fruits and other_crops), otherwise it
##                 creates duplicates. We will put it in fruits for now.
tmp = c(cereals = c(15, 27, 44, 75, 56, 71, 79, 83, 89, 101, 92, 94,
                    97, 103, 108),
        pulses = c(176, 187, 191, 195, 201, 181, 197, 210, 205,
                   203, 211),
        roots_tub = c(125, 116, 122, 137, 135, 136, 149),
        treenuts = c(221, 223, 217, 225, 222, 220, 224, 216, 226, 234),
        oilcrops = c(236, 242, 260, 249, 277, 305, 310, 256, 263, 265,
                     267, 270, 275, 280, 289, 292, 296, 299, 328, 329,
                     333, 336, 339),
        veg = c(388, 402, 403, 406, 426, 407, 393, 358, 372, 397, 417,
                414, 423, 420, 366, 367, 399, 449, 401, 373, 394, 446,
                430, 378, 567, 568, 463),
        fruit = c(486, 489, 577, 569, 574, 572, 571, 603, 490, 495, 507,
                  497, 512, 560, 600, 515, 521, 523, 526, 530, 531, 534,
                  536, 544, 547, 552, 554, 558, 550, 549, 592, 587, 542,
                  541, 591, 619, 461),
        stim_beve = c(656, 661, 667, 671, 674),
        spices = c(687, 689, 692, 693, 698, 702, 711, 720, 723),
        tobacco = c(826),
        othr_crops = c(459, 677),
        rubber = c(837, 836),
        leaves = c(748, 754),
        fibres = c(767, 780, 788, 800, 809, 821, 782, 987, 773, 1185, 789),
        othr_meat = c(867, 947, 1035, 977, 1017, 1127, 1097, 1108, 1111,
                      1163, 1166),
        poultry_meat = c(1058, 1069, 1073, 1080, 1141),
        milk = c(882, 951, 982, 1020, 1130), eggs = c(1062, 1091))

comGrp.df = melt(tmp)
comGrp.df$Group = gsub("[0-9]", "", rownames(comGrp.df))
row.names(comGrp.df) = NULL
comGrp.dt = data.table(comGrp.df)
rm(comGrp.df)
setnames(comGrp.dt, old = colnames(comGrp.dt), new = c("itemCode", "comGroup"))

## These commodities were commented out.
lvstk_prds = c(886, 901)
sugar = c(162, 163, 1182)
veg_oils = c(36, 60, 237, 244, 257, 258, 252, 261, 264, 266, 268, 271, 276, 281,
             290, 293, 297, 331, 334, 337, 340, 1242, 278, 307, 313)
alch_bev = c(564, 51, 66, 82, 86)



## Read and process the test data
## ---------------------------------------------------------------------
test.dt = data.table(read.csv("nicola_ws_request_12yrsR.csv", header = TRUE,
    stringsAsFactors = FALSE))
setnames(test.dt, old = c("ItemGroup", "AREA", "ITEM", "ELE"),
         new = c("itemGroup", "FAOST_CODE", "itemCode", "elementCode"))

## These countries were excluded (from SAS code)
FAOcountryProfile[FAOcountryProfile$FAOST_CODE %in%
                  c(351, 296, 357, 76, 245, 246, 246, 43), ]

## Subset only area harvested (31) and production (51)
process.dt = subset(test.dt, subset = elementCode == 31)
process.dt$itemGroup = NULL

## Process the data into the long format
mprocess.df = melt(process.dt, id.var = c("FAOST_CODE", "itemCode", "elementCode"))
mprocess.df$Year = gsub("[^0-9]", "", mprocess.df$variable)
mprocess.df$variable = gsub("[0-9|_]", "", mprocess.df$variable)
cprocess.df = dcast(mprocess.df, FAOST_CODE + itemCode + elementCode + Year ~
    variable)
cprocess.dt = data.table(cprocess.df)
rm(cprocess.df)

## Merge with the regional profile
## TODO (Michael): Need to check for those that doesn't have a region
## TODO (Michael): Need to check the region used for imputation.
cprocess.dt = merge(cprocess.dt,
    data.table(FAOregionProfile[, c("FAOST_CODE", "FAO_MACRO_REG",
                                    "FAO_SUB_REG")]),
    by = "FAOST_CODE", all.x = TRUE)

## Merge with the commodity group
cprocess.dt = merge(cprocess.dt, comGrp.dt, by = "itemCode", all.x = TRUE)

## Hack the character
## TODO (Michael): Fix this, check why it is converted to character
cprocess.dt$num = as.numeric(cprocess.dt$num)

## Create a column for original value
cprocess.dt$origVal = cprocess.dt$num


## Remove observation marked as "T"
cprocess.dt[cprocess.dt$symb == "T", num := NA]

## Replace duplicated "F" entry with NA
cprocess.dt[duplicated(cprocess.dt[, list(itemCode, FAOST_CODE, elementCode, num,
                                          symb)]) & symb == "F", num := NA]


## ## Find the repeated F
## test = rle(cprocess.dt$symb)
## test2 = with(test, data.frame(lengths, values))
## subset(test2, values == "F") ## What to do if the run was greater than 2?
## test$ind = cumsum(test$lengths)
## ind = test$ind[test$lengths >= 2 & test$values == "F"]




## Imputation
## ---------------------------------------------------------------------

## Function to compute the ratio between successive years

## NOTE (Michael): The problem with the current formulae is that it
##                 does not account for changes in item in the basket
##                 from year to year. Even if we take into account of
##                 this by computing pair-wise growth, the basket will
##                 still change from year to year because the
##                 pair-wise basket may not be the same.
diffv = function(x){
    T = length(x)
    x[x == 0] = 1e-5
    ## Use linear interpolation for weight (this might not be suitable)
    if(sum(!is.na(x)) >= 2)
        ## TODO (Michael): test whether approx or na.approx is faster.
        x = na.approx(x)
    tmp = c(x[2:T]/x[1:(T - 1)])
    tmp
}

## Compute country and commodity specific growth rate
cprocess.dt[, data_gr := c(1, diffv(num)),
     by = c("FAOST_CODE", "itemCode", "elementCode")]

## Function to compute the basket aggregate and the imputation
## NOTE (Michael): Might be better to implement the growth rate
##                 directly
nnimp = function(data, index, type_name, dataCol = "num", grCol = "data_gr",
                 includeInter = TRUE){
    setnames(data, old = c(dataCol, grCol), new = c("dataCol", "grCol"))
    ## Calculate the grouped sum
    ## data[, eval(parse(text = paste0("tmp1 := sum(", dataCol, ", na.rm = TRUE)"))),
    ##      by = c(unique(c("Year", "elementCode", index)))]
    data[, tmp1 := sum(dataCol, na.rm = !all(is.na(dataCol))),
         by = c(unique(c("Year", "elementCode", index)))]
    ## Calculate the group growth factor
    data[, tmp2 := c(1, diffv(tmp1)),
         by = c(unique(c("FAOST_CODE", "itemCode", "elementCode", index)))]
    ## Calculate the mixed growth based on commodity specific and grouped factor
    data[which(is.na(data[, grCol])),
         tmp3 := data[which(is.na(data[, grCol])), tmp2]]
    data[which(!is.na(data[, grCol])),
         tmp3 := data[which(!is.na(data[, grCol])), grCol]]

    ## data[, tmp3:= ifelse(c(is.na(data[, grCol, with = FALSE])),
    ##                      c(data[, tmp2]), c(data[, grCol, with = FALSE]))]
    ## print(is.na(data[, grCol, with = FALSE]))
    ## print(data[, tmp2])
    ## print(data[, grCol, with = FALSE])
    ## data[, tmp3] = ifelse(c(is.na(data[, grCol, with = FALSE])),
    ##                   c(data[, tmp2]), c(data[, grCol, with = FALSE]))

    ## Compute the imputation, no imputation is done all observation
    ## are either zero or NA (i.e. no useful information available).
    foo = function(val, gr){
        print(val)
        if(all(is.na(val) | val == 0)){
            tmp = val
        } else {
            firstObs = which(!is.na(val) & val != 0)[1]
            tmp = c(rep(NA, firstObs - 1), val[firstObs] *
                cumprod(gr[firstObs:length(gr)]))
        }

        ## if(any(!is.na(val)) | any(val != 0)){
        ##     firstObs = which(!is.na(val) & val != 0)[1]
        ##     tmp = c(rep(NA, firstObs - 1), val[firstObs] *
        ##         cumprod(gr[firstObs:length(gr)]))
        ## } else {
        ##     tmp = val
        ## }
        tmp
    }

    data[, tmp4 := foo(val = dataCol, gr = tmp3),
                by = c(unique(c("FAOST_CODE", "itemCode", "elementCode", index)))]
    ## Whether to return the immediate steps for check
    if(includeInter){
        setnames(data, old = c("tmp1", "tmp2", "tmp3", "tmp4", "dataCol", "grCol"),
                 new = c(paste0(type_name, c("_sum", "_gr", "_mixgr", "_imp")),
                         dataCol, grCol))
    } else {
        data[, tmp1 := NULL]
        data[, tmp2 := NULL]
        data[, tmp3 := NULL]
        setnames(data, old = c("tmp4", "dataCol", "grCol"),
                 new = c(paste0(type_name, "_imp"), dataCol, grCol))
    }
}

## Compute the imputation for the 5 different basket
nnimp(data = cprocess.dt, index = c("FAOST_CODE", "comGroup"),
      type_name = "countryComGroup")
nnimp(cprocess.dt, index = c("FAO_SUB_REG", "itemCode"),
      type_name = "subRegionCom")
nnimp(cprocess.dt, index = c("FAO_SUB_REG", "comGroup"),
      type_name = "subRegionComGroup")
nnimp(cprocess.dt, index = c("FAO_MACRO_REG", "itemCode"),
      type_name = "regionCom")
nnimp(cprocess.dt, index = c("FAO_MACRO_REG", "comGroup"),
      type_name = "regionComGroup")

## rows that are imputed
check = subset(cprocess.dt, subset = is.na(num),
    select = c("FAOST_CODE", "Year", "itemCode", "elementCode", "symb", "num",
               "origVal", grep("_imp", colnames(cprocess.dt), value = TRUE)))


## This the case where the basket change
check2 = subset(cprocess.dt, FAOST_CODE == 215 & itemCode == 1091)





## NOTE (Michael): We will not swtich between different imputation
##                 methods, so the cost of NA would be Inf. This
##                 approach is chosen because we prefer to have low
##                 relative error rather than low absolute error. It
##                 is true that switching between different
##                 methodology may result in lower absolute error
##                 (with respect to the unobserved data), but from a
##                 modeling approach it may possibly result in in
##                 consistent model.


## impType = grep("_imp", colnames(cprocess.dt), value = TRUE)
## errorType = grep("_error", colnames(cprocess.dt), value = TRUE)
## noInf = paste(paste0(errorType, " != Inf"), collapse = " & ")

## Looks like country/Comgroup is performing slightly better overall
## Compute the euclidean distance.
error = apply(subset(cprocess.dt, subset = eval(parse(text = noInf)),
                     select = grep("_error", colnames(cprocess.dt), value = TRUE)),
                     2, FUN = function(x) sqrt(sum(x^2)))


## NOTE (Michael): Use the time series which best approximate the
##                 origincal time series to do the imputation.

## Function to find the best imputation method.
## NOTE (Michael): Need to optimize and merge with nnimp function
optImp = function(data, dataCol, impCol, costFunc){
    FUN = match.fun(costFunc)
    ## noInf = paste(paste0(impCol, " != Inf"), collapse = " & ")
    n_imp = length(impCol)
    cost = double(n_imp)
    for(i in 1:n_imp){
        cost[i] = FUN(x = unlist(data[, dataCol, with = FALSE]),
                      y = unlist(data[, impCol[i], with = FALSE]))
    }

    ## cost = apply(subset(data, subset = eval(parse(text = noInf)),
    ##                     select = errorCol), 2, FUN = FUN)

    data$imp_name= impCol[which.min(cost)]
    data$final_imp = data[, impCol[which.min(cost)], with = FALSE]
    data
}


grType = grep("_gr", colnames(cprocess.dt), value = TRUE)
baseGr = "data_gr"
impGr = grType[grType != baseGr]
impType = grep("_imp", colnames(cprocess.dt), value = TRUE)
noInf = paste(paste0(errorType, " != Inf"), collapse = " & ")

cprocess.dt$key_split =
    with(cprocess.dt, paste(FAOST_CODE, itemCode, elementCode,
                                            sep = "_"))

system.time({
    reduced.lst = split(x = cprocess.dt, cprocess.dt$key_split)
})

## NOTE (Michael): If there is only one observation in the data, then
##                 the first imputation type (countryComGrp) will be
##                 used.
myCostFun = function(x, y){
    ## Make use of all information by replacing NA's in data
    x = na.locf(x, na.rm = FALSE)
    x[is.na(x)] = mean(na.omit(x))
    ## Fully penalize NA, so a consistent approach is used.
    y[is.na(y)] = Inf
    sqrt(sum((x - y)^2, na.rm = TRUE))
}
test = lapply(reduced.lst,
    FUN = function(x) optImp(data = x, dataCol = "num", impCol = impType,
    costFunc = myCostFun))

system.time({
    test2 = Reduce(f = function(x, y) rbind(x, y), x = test[-1],
                   init = test[[1]])
})

## This is shows that by replacing NA's in data gives a good
## approximation.
subset(test2, FAOST_CODE == 99 & itemCode == 645)

subset(test2, num == 0 & Year == 45)

## Percentage not imputed
sum(is.na(test2$final_imp))/sum(is.na(test2$num))










## TODO (Michael): Plot the unimputed value as well (num)
key = arrange(unique(test2[, list(FAOST_CODE, itemCode, elementCode)]),
    FAOST_CODE, itemCode, elementCode)

subkey = subset(key, FAOST_CODE == 231)

colTable = data.frame(breaks = unique(mtmp$variable),
                      col = c("red", rgb(0, seq(0.2, 1, length = 5), 0), "orange"),
                      stringsAsFactors = FALSE)


pdf(file = "check.pdf", width = 12)
for(i in 1:NROW(subkey)){
myRow = i
tmp = subset(test2, FAOST_CODE == subkey[myRow, FAOST_CODE] &
    itemCode == subkey[myRow, itemCode] & elementCode == subkey[myRow, elementCode],
    select = c("Year", "origVal", grep("_imp", colnames(test2), value = TRUE)))
mtmp = melt(tmp, id.var = "Year")

print(ggplot(data = mtmp, aes(x = as.numeric(Year), y = value)) +
    geom_point(aes(col = variable)) +
    geom_line(aes(col = variable), lty = 2) +
    scale_colour_manual(breaks = colTable$breaks, values = colTable$col) +
    labs(title = paste(subkey[i, ], collapse = "_")))
}

graphics.off()
system("open check.pdf")

## TODO (Michael): Can not be base on the first value, since the first
##                 value can be zero and also this does not account
##                 for intermediate observations. This is the reason
##                 why the imputed time series is much smoother than
##                 the observed time series.




cprocess.dt[, optImp(cprocess.dt, dataCol = "num", impCol = impType,
                     costFunc = function(x, y) sqrt(sum((x - y)^2, na.rm = TRUE))),
            by = c("FAOST_CODE")]


optImp(cprocess.dt, dataCol = "num", impCol = impType,
       costFunc = function(x, y) sqrt(sum((x - y)^2, na.rm = TRUE)))

optImp(cprocess.dt, dataCol = "num", impCol = impType,
       costFunc = function(x, y) sqrt(sum(abs(x - y), na.rm = TRUE)))

optImp(cprocess.dt, dataCol = "num", impCol = impType,
       costFunc = function(x ,y) 1 - cor(x, y, use = "complete.obs"))


## Plot to check the imputation
check.dt = subset(cprocess.dt, subset = FAOST_CODE == 1 & itemCode == 15 &
                  elementCode == 31, select = c("FAOST_CODE", "Year", "num",
              grep("_imp", colnames(cprocess.dt), value = TRUE)))



image(data.matrix(check.dt))

mcheck.dt = melt(data = check.dt, id.var = c("FAOST_CODE", "Year"))
ggplot(data = mcheck.dt, aes(x = as.numeric(Year), y = value)) +
    geom_line(aes(col = variable)) +
    scale_color_manual(values = plot_colors(n = 6)$Sub)



## Plot the imputation
par(mfrow = c(2, 3))
with(cprocess.dt, plot(num, eval(parse(text = impType[1]))))
with(cprocess.dt, plot(num, eval(parse(text = impType[2]))))
with(cprocess.dt, plot(num, eval(parse(text = impType[3]))))
with(cprocess.dt, plot(num, eval(parse(text = impType[4]))))
with(cprocess.dt, plot(num, eval(parse(text = impType[5]))))





