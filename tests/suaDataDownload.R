## Load the library and establish the connection.
## NOTE (Michael): the ojdbc14.jar and internal connection is required.
library(RJDBC)
library(FAOSTAT)
library(reshape2)

drv = JDBC(driverClass = "oracle.jdbc.driver.OracleDriver",
    classPath = "ojdbc14.jar")
conn = dbConnect(drv, "jdbc:oracle:thin:@lprdbwo1:3310:fstp",
    user = "demo", password = "demo")




getStandardizationData = function(conn, countryCode, year){
    valueCol = paste("num_", year, collapse = ", ", sep = "")
    symbCol = paste("symb_", year, collapse = ", ", sep = "")

    ## Extraction rate data
    extractionRateQuery =
        paste0("SELECT area, item, ele, ", valueCol, ", ", symbCol,
               " FROM tsv_ics_work_yr WHERE ele = 41 AND area = ",
               countryCode, " ORDER BY area, item, ele")
    extractionRateData =
        dbGetQuery(conn, extractionRateQuery)
    ## print(extractionRateData)

    extractionRateNormData =
        melt(extractionRateData, id.var = c("AREA", "ITEM", "ELE"))
    extractionRateNormData$Year =
        as.numeric(gsub("[^0-9]", "" ,extractionRateNormData$variable))
    extractionRateNormData$Type =
        gsub("[0-9|_]", "" , extractionRateNormData$variable)

    extractionRateNormData$variable = NULL
    extractionRateFinal =
        dcast(extractionRateNormData, AREA + ITEM + ELE + Year ~ Type,
              value.var = "value")
    extractionRateFinal$NUM = as.numeric(extractionRateFinal$NUM)
    colnames(extractionRateFinal) = c("Area.Code", "Item.Code",
                "Element.Code", "Year", "Num", "Symb")


    ## Sua data
    element = "51, 61, 71, 91, 101, 111, 121, 131, 141, 151"
    suaQuery =
        paste0("SELECT area, item, ele, ", valueCol, ",", symbCol,
               " FROM tsv_ics_work_yr WHERE ele in (",
               element,
               ") AND area in (",
               countryCode,
               ") ORDER BY area, item, ele")
    suaData = dbGetQuery(conn, suaQuery)
    suaNormData =
        melt(suaData, id.var = c("AREA", "ITEM", "ELE"))
    suaNormData$Year =
        as.numeric(gsub("[^0-9]", "" ,suaNormData$variable))
    suaNormData$Type =
        gsub("[0-9|_]", "" , suaNormData$variable)
    
    suaNormData$variable = NULL
    suaFinal =
        dcast(suaNormData, AREA + ITEM + ELE + Year ~ Type,
              value.var = "value")
    suaFinal$NUM = as.numeric(suaFinal$NUM)
    colnames(extractionRateFinal) = c("Area.Code", "Item.Code",
                "Element.Code", "Year", "Num", "Symb")
    

    ## input data
    inputQuery =
    paste0("SELECT area, item_parent, item_child, ",
           valueCol, ",", symbCol, " FROM input_from_procv ")
    inputData = dbGetQuery(conn, inputQuery)
    inputNormData =
        melt(inputData, id.var = c("AREA", "ITEM_PARENT", "ITEM_CHILD"))
    inputNormData$Year =
        as.numeric(gsub("[^0-9]", "" ,inputNormData$variable))
    inputNormData$Type =
        gsub("[0-9|_]", "" , inputNormData$variable)
    
    inputNormData$variable = NULL
    inputFinal =
        dcast(inputNormData, AREA + ITEM_CHILD + ITEM_PARENT +
                  Year ~ Type,
              value.var = "value")
    inputFinal$NUM = as.numeric(inputFinal$NUM)
    colnames(inputFinal) = c("Area.Code", "Child.Item.Code",
                "Item.Code", "Year", "Num", "Symb")
    
    list(extractionRateData = extractionRateFinal,
         suaData = suaFinal,
         inputData = inputFinal)
}

currentYear = 2011
test = getStandardizationData(conn, 231, currentYear)
str(test$extractionRateData)
benchmark = data.table(read.csv(file = "usa_demo_extract_data_full.csv"))
benchmark = benchmark[Year == currentYear, ]
setnames(benchmark, c("Num", "Symb"), c("right_num", "right_symb"))

check = merge(data.table(test$extractionRateData), benchmark,
    by = intersect(colnames(test$extractionRateData),
        colnames(benchmark)), all = TRUE)



currentYear = 2011
str(test$inputData)
benchmark = data.table(read.csv(file = "usa_demo_input_data.csv"))
benchmark = benchmark[Year == currentYear, ]
setnames(benchmark, c("Num", "Symb"), c("right_num", "right_symb"))

check = merge(data.table(test$inputData), benchmark,
    by = intersect(colnames(test$inputData),
        colnames(benchmark)), all = TRUE)



## Extraction rates
countryCode = 231
years = paste("num_", 1961:2011, collapse = ", ", sep = "")
symb = paste("symb_", 1961:2011, collapse = ", ", sep = "")
element = "41"
extractRate = dbGetQuery(conn,
    paste0("SELECT area, item, ele, ",
           years, ", ", symb,
           " FROM tsv_ics_work_yr WHERE ele in (",
           element,
           ") AND area in (",
           countryCode,
           ") ORDER BY area, item, ele"))


denormExtractRate = melt(extractRate, id.var = c("AREA", "ITEM", "ELE"))
denormExtractRate$Year =
    as.numeric(gsub("[^0-9]", "" ,denormExtractRate$variable))
denormExtractRate$Type =
    gsub("[0-9|_]", "" , denormExtractRate$variable)

denormExtractRate$variable = NULL
extractRateFinal =
    dcast(denormExtractRate, AREA + ITEM + ELE + Year ~ Type,
          value.var = "value")
extractRateFinal$NUM = as.numeric(extractRateFinal$NUM)
colnames(extractRateFinal)[1:3] = colnames(extractCheck)[c(1, 3, 5)]

check = merge(extractRateFinal, extractCheck, all = TRUE)
check = check[check$Year >= 2005, ]

intersect(colnames(extractRateFinal), colnames(extractCheck))

extractCheck =
    read.csv(file = "usa_demo_extract_data_full.csv",
             stringsAsFactors = FALSE)


extractRate = dbGetQuery(conn,
    paste0("SELECT * FROM tsv_ics_work_yr WHERE ele in (",
           element,
           ") AND area in (",
           countryCode,
           ") ORDER BY area, item, ele"))


## Sua data
countryCode = 231
years = paste("num_", 1961:2011, collapse = ", ", sep = "")
element = "51, 61, 71, 91, 101, 111, 121, 131, 141, 151"
suaData = dbGetQuery(conn,
    paste0("SELECT area, item, ele, ",
           years,
           " FROM tsv_ics_work_yr WHERE ele in (",
           element,
           ") AND area in (",
           countryCode,
           ") ORDER BY area, item, ele"))

## Input data
valueCol = paste("num_", 2001, collapse = ", ", sep = "")
symbCol = paste("symb_", 2001, collapse = ", ", sep = "")

inputQuery =
    paste0("SELECT area, item_parent, item_child, ",
           valueCol, ",", symbCol, " FROM input_from_procv ")
test = dbGetQuery(conn, inputQuery)


## Example query
years = paste("num_", 1961:2011, collapse = ", ", sep = "")
elements = "31, 51"
items = "15"

test =
    dbGetQuery(conn,
               paste0("SELECT area, item, ele, ",
                     years,
                     " FROM tsv_ics_work_yr WHERE ele in (",
                     elements,
                     ") AND item in (",
                     items,
                     ") ORDER BY area, item, ele"))




getSWSProduction = function(itemCode, elementCode, year, connection){
    years = paste("num_", year, collapse = ", ", sep = "")
    symbs = paste("symb_", year, collapse = ", ", sep = "")
    elements = paste(elementCode, collapse = ", ")
    items = paste(itemCode, collapse = ", ")
    dbGetQuery(connection,
               paste0("SELECT area, item, ele, ",
                      years,", ", symbs,
                      " From tsv_ics_work_yr WHERE ele in (",
                      elements,
                      ") AND item in (",
                      items,
                      ") ORDER BY area, item, ele"))
}

test = getSWSProduction(itemCode = 15, elementCode = c(31, 51),
    year = 1961:2011, connection = conn)


library(FAOSTAT)
source("../../support_functions/toLowerCamel.R")
allItemTable =
    unique(FAOmetaTable$itemTable[FAOmetaTable$itemTable$domainCode == "QC",
                                  c("itemCode", "itemName")])
for(i in 1:NROW(allItemTable)){
    print(allItemTable[i, ])
    tmp = try(
        getSWSProduction(itemCode = as.numeric(allItemTable[i, "itemCode"]),
                         elementCode = c(31, 51),
                         year = 1961:2011,
                         connection = conn)
        )
    if(!inherits(tmp, "try-error") & NROW(tmp) > 0){
        meltTmp = melt(tmp, id.vars = c("AREA", "ITEM", "ELE"))
        splits = strsplit(x = as.character(meltTmp$variable), split = "\\_")
        meltTmp$TYPE = sapply(splits, FUN = function(x) x[1])
        meltTmp$YEAR = sapply(splits, FUN = function(x) as.numeric(x[2]))
        meltTmp$variable = NULL
        castTmp = dcast(meltTmp, AREA + YEAR + ITEM ~ ELE + TYPE,
            value.var = "value")
        colnames(castTmp)[1:3] = c("areaCode", "year", "itemCode")
        colnames(castTmp) = gsub("51", "production", colnames(castTmp))
        colnames(castTmp) = gsub("31", "areaHarvested", colnames(castTmp))
        colnames(castTmp) = gsub("_NUM", "Value", colnames(castTmp))
        colnames(castTmp) = gsub("_SYMB", "Symb", colnames(castTmp))
        try({
            castTmp$areaHarvestedValue =
                as.numeric(castTmp$areaHarvestedValue)
            })
        try({
            castTmp$productionValue =
                as.numeric(castTmp$productionValue)
        })
        write.csv(castTmp,
                  file = paste0("../../sua_data/",
                      toLowerCamel(allItemTable[i, "itemName"]),
                      "SUA.csv"),
                  row.names = FALSE, na = "")
    } else {
        print("this query failed")
    }
}
