library(data.table)
library(plyr)
library(reshape2)

## Load the data
cpc.dt = data.table(read.csv(file = "cpcTofcl.csv", header = TRUE,
  stringsAsFactors = FALSE))
setkey(cpc.dt, ICSD_Code)
icsd.dt = data.table(read.csv(file = "t_ICSD_rawdata.csv", header = TRUE,
  stringsAsFactors = FALSE))
setkey(icsd.dt, ICSD_Code)
country.dt = data.table(read.csv(file = "t_countries_regions.csv", header = TRUE,
  stringsAsFactors = FALSE))
setnames(country.dt, old = "FAO_Country.Code", new = "FAO_country_code")

## Removing ID, they dont appear to be meaningful
cpc.dt[, ID := NULL]
icsd.dt[, ID := NULL]

## Need to double check the cartesian join
icsdFull.dt = merge(icsd.dt, cpc.dt, by = "ICSD_Code", allow.cartesian = TRUE)
icsdFinal.dt = merge(icsdFull.dt, country.dt, by = "ICSD_Country")

icsdWheat.dt = icsdFinal.dt[FCL_Code %in% 16 &
  !grepl("USD", Unit), list(FAO_country_code, FCL_Code, ICSD_Code, Year, Unit,
                            Value)]
icsdWheat.dt[, finalValue := sum(Value, na.rm = TRUE),
             by = c("FAO_country_code", "Year", "FCL_Code")]
icsdWheat.dt[, Value := NULL]
icsdWheat.dt[, ICSD_Code := NULL]
setnames(icsdWheat.dt, old = c("FAO_country_code", "FCL_Code", "finalValue"),
         new = c("Area.Code", "Item.Code", "Production_ICSD"))

sua.df = read.csv(file = "wheatProdEr.csv", header = TRUE)
msua.df = melt(sua.df, id.var = c("Area.Code", "Item.Code", "Element.Code",
                         "Element.Name"))
msua.df$Year = as.integer(gsub("[^0-9]", "", msua.df$variable))
msua.df$Type = ifelse(grepl("Num", msua.df$variable), "Value", "Symb")
msua.df$variable = NULL
msua.df$Element.Code = NULL

dmsua.dt = data.table(dcast(msua.df[msua.df$Item.Code %in% 16, ],
  Area.Code + Item.Code + Year ~ Element.Name + Type, value.var = "value"))

setnames(dmsua.dt, old = c("Extr Rate (Hg/Mt)_Symb", "Extr Rate (Hg/Mt)_Value",
                     "Production (Mt)_Symb", "Production (Mt)_Value"),
         new = c("ExtRate_FAO_symb", "ExtRate_FAO", "Production_FAO_symb",
         "Production_FAO"))
dmsua.dt[, ExtRate_FAO := as.numeric(ExtRate_FAO)]
dmsua.dt[, Production_FAO := as.numeric(Production_FAO)]

compare.dt = merge(dmsua.dt, icsdWheat.dt,
  by = c("Area.Code", "Item.Code", "Year"))

compare.dt[grepl("THOUSAND", Unit), Production_ICSD := Production_ICSD * 1000]
compare.dt[grepl("THOUSAND", Unit), Unit := gsub("THOUSAND", "", Unit)]

compare.dt[, ExtRate_ICSD_Imp := Production_ICSD/(Production_FAO/ExtRate_FAO)]

## with(compare.dt, plot(Production_ICSD, Production_FAO))
## abline(a = 0, b = 1, col = "red")

## with(compare.dt, plot(log(Production_ICSD), log(Production_FAO)))
## abline(a = 0, b = 1, col = "red")


with(compare.dt, plot(log(Production_ICSD, 10), log(Production_FAO, 10)))
abline(a = 0, b = 1, col = "red")


with(compare.dt[Production_FAO != 0 & ExtRate_ICSD_Imp <= 10000 &
                ExtRate_FAO <= 10000, ],
     plot(ExtRate_FAO, ExtRate_ICSD_Imp))
abline(a = 0, b = 1, col = "red", lty = 2)



with(compare.dt[abs(Production_FAO - Production_ICSD)/Production_FAO < 0.3, ],
     plot(ExtRate_FAO, ExtRate_ICSD_Imp, cex = 0.5,
          col = rgb(0, 0, 0, alpha = 0.5)))
abline(a = 0, b = 1, col = "red", lty = 2)

par(mfrow = c(2, 1))
hist(compare.dt[ExtRate_FAO <= 10000, ExtRate_FAO])
hist(compare.dt[ExtRate_ICSD_Imp <= 10000, ExtRate_ICSD_Imp])


compare.dt[Production_FAO >= 4e7 & Production_ICSD <= 1e7, ]

rmOutlier.dt = compare.dt[Area.Code != 41, ]
with(rmOutlier.dt[Production_FAO != 0 & ExtRate_ICSD_Imp < 100000, ],
     plot(ExtRate_FAO, ExtRate_ICSD_Imp))

## This shows that the extraction rate is calculated and is not
## feasible.
compare.dt[ExtRate_FAO >= 10000, ]
