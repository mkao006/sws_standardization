library(reshape2)
library(RJDBC)
library(data.table)
library(lattice)
options(java.parameters = "-Xmx1500m")

## Connect to the database
drv = JDBC(driverClass = "oracle.jdbc.driver.OracleDriver",
    classPath = "~/ojdbc14.jar")
conn = dbConnect(drv, "jdbc:oracle:thin:@lprdbwo1:3310:fstp",
    user = "demo", password = "demo")


items = dbGetQuery(conn,
    "SELECT item, name_e FROM item WHERE item <= 1299")
colnames(items) = c("itemCode", "itemName")

getExtractRate = function(itemCode, itemName, conn){
    tmp = dbGetQuery(conn, paste0("SELECT * FROM tsv_ics_work_yr
                                  WHERE ele = '41'
                                  AND item = '", itemCode, "'"))
    if(NROW(tmp) == 0){
        tmp = NULL
    } else {
        tmp$itemName = itemName
    }
    tmp
}


extractionRate.lst =
    with(items,
         mapply(getExtractRate,
                itemCode = itemCode,
                itemName = itemName,
                MoreArgs = list(conn = conn))
         )



extractionRate.lst =
    extractionRate.lst[sapply(extractionRate.lst, NROW) != 0]
finalExtractionRate.lst =
    lapply(extractionRate.lst,
           FUN = function(x){
               colnames(x)[1:3] =
                   c("areaCode", "itemCode", "elementCode")
               melted = melt(x,
                   id.var = c("areaCode", "itemCode", "itemName",
                       "elementCode"))
               melted$Year =
                   as.numeric(gsub("[^0-9]", "", melted$variable))
               melted$type = gsub("[0-9|_]", "", melted$variable)
               melted$variable = NULL
               casted = data.table(dcast(melted, areaCode + itemCode +
                                             itemName +  elementCode +
                                                 Year ~ type,
                   value.var = "value"))
               casted[, NUM := as.numeric(NUM)]
               casted[SYMB == "M", NUM := as.numeric(NA)]
               casted
       }
)


myPlot = function(x){
    print(xyplot(NUM ~ Year|as.character(areaCode),
                 data = x, group = x$SYMB,
                 auto.key = TRUE,
                 main = paste0(unique(x$itemName), "(",
                     unique(x$itemCode), ")")))
}


pdf(file = "extractionRateTrend.pdf", width = 20, height = 15)
lapply(finalExtractionRate.lst, FUN = myPlot)
graphics.off()

## save(extractionRate.lst, file = "extractionRate.RData")
## load("extractionRate.RData")




ratio =
    dbGetQuery(conn,
               "SELECT * FROM aupus_ratios WHERE area = '9'")
