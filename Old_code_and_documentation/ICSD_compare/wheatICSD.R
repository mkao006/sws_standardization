library(data.table)
library(plyr)
library(reshape2)
library(gridExtra)
library(FAOSTAT)

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
icsdFull.dt = merge(icsd.dt, cpc.dt[, list(ICSD_Label, ICSD_Code, CPC_2_Code,
  CPC_2_Title, FCL_Code, FCL_Title)], by = "ICSD_Code", allow.cartesian = TRUE)
icsdFinal.dt = merge(icsdFull.dt, country.dt, by = "ICSD_Country")

## Remove value, only keep quantities
icsdProd.dt = icsdFinal.dt[!grepl("USD", Unit),
  list(FAO_country_code, CPC_2_Code, FCL_Code, ICSD_Code, Year, Unit, Value)]


## This is a temporary hack only work for first level processed commodity
icsdProd.dt[, finalValue := sum(Value, na.rm = TRUE),
             by = c("FAO_country_code", "Year", "FCL_Code")]
icsdProd.dt[, Value := NULL]
icsdProd.dt[, ICSD_Code := NULL]
setnames(icsdProd.dt, old = c("FAO_country_code", "FCL_Code", "finalValue"),
         new = c("Area.Code", "Item.Code", "Production_ICSD"))





## Function to read and convert SUA data format to long format
read.sua = function(file, keys, ...){
  tmp = read.csv(file = file, ...)
  mtmp = melt(tmp, id.var = keys)
  mtmp$Year = as.integer(gsub("[^0-9]", "", mtmp$variable))
  mtmp$Type = ifelse(grepl("Num", mtmp$variable), "Value", "Symb")
  mtmp$variable = NULL
  mtmp
}

## Function to transform read sua data to wide format
foo = function(suaDF){
  suaDF$Element.Code = NULL
  suaDT = data.table(dcast(suaDF,
    Area.Code + Item.Code + Year ~ Element.Name + Type, value.var = "value"))
    setnames(suaDT, old = c("Extr Rate (Hg/Mt)_Symb", "Extr Rate (Hg/Mt)_Value",
                       "Production (Mt)_Symb", "Production (Mt)_Value"),
           new = c("ExtRate_FAO_symb", "ExtRate_FAO", "Production_FAO_symb",
             "Production_FAO"))
  suaDT[, ExtRate_FAO := as.numeric(ExtRate_FAO)]
  suaDT[, Production_FAO := as.numeric(Production_FAO)]
  suaDT
}




## Wheat first level
wheatFirst.dt = foo(read.sua(file = "wheatFirstLevel.csv",
  keys = c("Area.Code", "Item.Code", "Element.Code", "Element.Name")))


wheatSUA.df = read.sua(file = "wheatSUAselection.csv", 
  keys = c("Area.Code", "Item.Code", "Element.Code", "Element.Name"),
  stringsAsFactors = FALSE)

wheatSUA.df$Element.Name = substr(wheatSUA.df$Element.Name, 1,
  regexpr(" ", wheatSUA.df$Element.Name) - 1)
wheatSUA.df[wheatSUA.df$Element.Name == "Production", "Element.Name"] =
  "Production_Wheat"

wheatFirst.df = read.sua(file = "wheatFirstLevel.csv",
  keys = c("Area.Code", "Item.Code", "Element.Code", "Element.Name"))
wheatFlour.df = subset(wheatFirst.df, Item.Code == 16)
wheatFlour.df$Element.Name = substr(wheatFlour.df$Element.Name, 1,
  regexpr(" ", wheatFlour.df$Element.Name) - 1)
wheatFlour.df[wheatFlour.df$Element.Name == "Production", "Element.Name"] =
  "Production_Wheat_Flour"
wheatCheck.df = rbind(wheatSUA.df, wheatFlour.df)
wheatCheck.df$Item.Code = NULL
wheatCheck.df$Element.Code = NULL

dwc.df = dcast(wheatCheck.df, Area.Code + Year ~ Element.Name + Type,
  value.var = "value")
dwc.df[, grep("Value", colnames(dwc.df), value = TRUE)] =
  as.numeric(data.matrix(dwc.df[, grep("Value", colnames(dwc.df), value = TRUE)]))
dwc.dt = data.table(dwc.df)
wheatFlourICSD.dt = subset(icsdProd.dt, Item.Code == 16,
  select = c("Area.Code", "Year", "Production_ICSD"))
wheatFlourICSD.dt[, Production_ICSD := Production_ICSD * 1000]
wheatCheckFinal.dt = merge(dwc.dt, wheatFlourICSD.dt, by = c("Area.Code", "Year"))



wheatValue.dt = wheatCheckFinal.dt[, c("Area.Code", "Year",
  grep("Value", colnames(wheatCheckFinal.dt), value = TRUE), "Production_ICSD"),
  with = FALSE]

mwv.dt = melt(wheatValue.dt, id.var = c("Area.Code", "Year"))
mwv.dt$variable = as.character(mwv.dt$variable)
mwv.dt$lwd = ifelse(mwv.dt$variable %in% c("Production_Wheat_Flour_Value",
  "Production_ICSD", "Processed_Value"), 1.15, 1)

pdf("wheatFlourCheck.pdf", width = 10)
for(i in unique(mwv.dt$Area.Code)){
  print(ggplot(mwv.dt[mwv.dt$Area.Code == i &
                !mwv.dt$variable %in% c("Area_Value", "Extr_Value"), ],
               aes(x = Year, y = value)) +
        geom_line(aes(col = variable, linetype = variable,
                      size = lwd)) +
        scale_size(range = c(1, 2)))
}
graphics.off()
system("evince wheatFlourCheck.pdf&")


last.dt = mwv.dt[mwv.dt$variable %in% c("Processed_Value", "Stock_Value",
  "Production_Wheat_Flour_Value", "Production_ICSD"), ]
ggplot(last.dt[mwv.dt$Area.Code == 1, ], aes(x = Year, y = value)) +
  geom_line(aes(col = variable, linetype = variable))






combineFAOICSD =
  function(FAO, ICSD, key = c("Area.Code", "Item.Code", "Year"), item = 16){
  comb.dt = merge(FAO[Item.Code == item, ], ICSD[Item.Code == item, ],
    by = key)
  comb.dt[grepl("THOUSAND", Unit), Production_ICSD := Production_ICSD * 1000]
  comb.dt[grepl("THOUSAND", Unit), Unit := gsub("THOUSAND", "", Unit)]
  comb.dt[, ExtRate_ICSD := Production_ICSD/(Production_FAO/ExtRate_FAO)]
  comb.dt
}

compareFAOICSD = function(FAOICSD, title){
  prodRange = with(FAOICSD, range(c(Production_ICSD, Production_FAO), na.rm = TRUE,
    finite = TRUE))
  prod = ggplot(data = FAOICSD, aes(x = Production_ICSD, y = Production_FAO)) +
    geom_point() +
      geom_abline(intercept = 0, slope = 1, col = "red") +
        scale_x_continuous(limits = prodRange) +
          scale_y_continuous(limits = prodRange) +  
            facet_wrap(~Production_FAO_symb) +
              labs(title = title)
  
  ExtRateRange = with(FAOICSD, range(c(ExtRate_ICSD, ExtRate_FAO), na.rm = TRUE,
    finite = TRUE))
  er = ggplot(data = FAOICSD, aes(x = ExtRate_ICSD, y = ExtRate_FAO)) +
    geom_point() +
      geom_abline(intercept = 0, slope = 1, col = "red") +
        scale_x_continuous(limits = ExtRateRange) +
          scale_y_continuous(limits = ExtRateRange) +
            facet_wrap(~Production_FAO_symb)
  grid.arrange(prod, er)
}


scatterhist = function(x, y, xlab="", ylab="", breaks = 20, main, ...){
  opar = par()
  zones = matrix(c(2, 0, 1, 3), ncol = 2, byrow = TRUE)
  layout(zones, widths = c(0.8, 0.2), heights = c(0.2, 0.8))
  xhist = hist(x, plot = FALSE, breaks = breaks)
  yhist = hist(y, plot = FALSE, breaks = breaks)
  top = max(c(xhist$counts, yhist$counts))
  par(mar = c(3, 3, 1, 1))
  plot(x, y, main = main, ...)
  abline(a = 0, b = 1, col = "blue", lty = 2)
  abline(a = 0, b = 1.2, col = "red", lty = 2)
  abline(a = 0, b = 0.8, col = "red", lty = 2)
  par(mar = c(0, 3, 1, 1))
  barplot(xhist$counts, axes = FALSE, ylim = c(0, top), space = 0)
  par(mar = c(3, 0, 1, 1))
  barplot(yhist$counts, axes = FALSE, xlim = c(0, top), space = 0, horiz = TRUE)
  par(oma = c(3, 3, 0, 0))
  mtext(xlab, side = 1, line = 1, outer = TRUE, adj = 0, 
    at = 0.8 * (mean(x) - min(x))/(max(x)-min(x)))
  mtext(ylab, side = 2, line = 1, outer = TRUE, adj = 0, 
    at =(.8 * (mean(y) - min(y))/(max(y) - min(y))))
  par(opar)
}

## Wheat flour analysis
final.dt = combineFAOICSD(wheatFirst.dt, icsdProd.dt, item = 16)


ggplot(data = wheatFirst.dt[Item.Code == 16, ], aes(x = Year, y = ExtRate_FAO)) +
  geom_line(aes(col = factor(Area.Code))) +
  facet_wrap(~ExtRate_FAO_symb) +
  theme(legend.position = "none")

## Extraction rate should not exceed 1 for wheat flour
final.dt[ExtRate_FAO > 10000, ]


## We have zero production of wheat for Belgium but more than a
## million ton from ICSD.
final.dt[Production_ICSD >= 1000000 & Production_FAO < 10000, ]

prodRange = with(final.dt, range(c(Production_ICSD, Production_FAO), na.rm = TRUE,
  finite = TRUE))
ggplot(data = final.dt, aes(x = Production_ICSD, y = Production_FAO)) +
  geom_point(aes(col = factor(Area.Code))) +
  ## geom_line(aes(col = factor(Area.Code))) +  
  geom_abline(intercept = 0, slope = 1, col = "red", type = "dashed") +
  ## geom_abline(intercept = 0, slope = 1.2, col = "blue", lty = "dashed") +
  ## geom_abline(intercept = 0, slope = 0.8, col = "blue", lty = "dashed") +    
  scale_x_continuous(limits = prodRange) +
  scale_y_continuous(limits = prodRange) +
  facet_grid(Production_FAO_symb ~ Year, scales = "free") + 
  ## facet_wrap(~Production_FAO_symb) +
  theme(legend.position = "none")



compareFAOICSD(final.dt, "Flour of wheat excluding mixed flour")




with(final.dt,
     scatterhist(Production_FAO, Production_ICSD, breaks = 50,
                 xlab = "FAO", ylab = "ICSD", pch = 19,
                 ## col = rgb(0, 0, 0, alpha = 0.1),
                 main = "Comparison of production of wheat flour"))



with(final.dt[Production_FAO <= 1e7 & Production_ICSD <= 1e7, ],
     scatterhist(Production_FAO, Production_ICSD, breaks = 50,
                 xlab = "FAO", ylab = "ICSD", pch = 19,
                 col = rgb(0, 0, 0, alpha = 0.1),
                 main = "Comparison of production of wheat flour"))



with(final.dt[ExtRate_ICSD < 100000, ],
     scatterhist(ExtRate_FAO, ExtRate_ICSD, breaks = 50,
                 xlab = "FAO", ylab = "ICSD",
                 main = "Comparison of Extraction of wheat flour"))

## From this graph we can see that the input of FAO is over-estimated
## since the Extraction rate of FAO is much higher than that of ICSD.
with(final.dt[ExtRate_FAO/ExtRate_ICSD < 10, ],
     hist(ExtRate_FAO/ExtRate_ICSD, breaks = 1000))
abline(v = 1, col = "red", lty = 2)
abline(v = 1.2, col = "blue", lty = 2)
abline(v = 0.8, col = "blue", lty = 2)


with(final.dt[Production_FAO/Production_ICSD < 10, ],
     hist(Production_FAO/Production_ICSD, breaks = 1000))
abline(v = 1, col = "red", lty = 2)
abline(v = 1.2, col = "blue", lty = 2)
abline(v = 0.8, col = "blue", lty = 2)

with(final.dt[Production_FAO/Production_ICSD < 10, ],
     hist(log(Production_FAO)/log(Production_ICSD), breaks = 1000))
abline(v = 1, col = "red", lty = 2)
abline(v = 1.2, col = "blue", lty = 2)
abline(v = 0.8, col = "blue", lty = 2)


## Check whether the error is independet of time
final.dt[, Prod_diff := Production_FAO - Production_ICSD]
final.dt[, Prod_div := Production_FAO/Production_ICSD]

ggplot(data = final.dt, aes(x = Year, y = Prod_diff)) +
  geom_line(aes(col = factor(Area.Code))) +
  theme(legend.position = "none")

ggplot(data = final.dt, aes(x = Year, y = Prod_div)) +
  geom_line(aes(col = factor(Area.Code))) +
  coord_cartesian(ylim = c(0, 2)) + 
  theme(legend.position = "none")

test = final.dt[Production_FAO_symb == "C",
  Production_FAO/Production_ICSD]
test = test[!is.na(test) & is.finite(test) & test != 0]
ltest = log(test)

par(mfrow = c(1, 2))
fit = smsn.mix(ltest, nu = 3, g = 5, get.init = TRUE,
         criteria = TRUE, group = TRUE, family = "Normal", calc.im = FALSE)
mix.hist(ltest, fit, breaks = 100)
curve(fit$pii[1] * dnorm(x, mean = fit$mu[1], sd = sqrt(fit$sigma2[1])),
      add = TRUE, col = "orange")
curve(fit$pii[2] * dnorm(x, mean = fit$mu[2], sd = sqrt(fit$sigma2[2])),
      add = TRUE, col = "orange")
curve(fit$pii[3] * dnorm(x, mean = fit$mu[3], sd = sqrt(fit$sigma2[3])),
      add = TRUE, col = "orange")
curve(fit$pii[4] * dnorm(x, mean = fit$mu[4], sd = sqrt(fit$sigma2[4])),
      add = TRUE, col = "orange")
curve(fit$pii[5] * dnorm(x, mean = fit$mu[5], sd = sqrt(fit$sigma2[5])),
      add = TRUE, col = "orange")
## curve(fit$pii[6] * dnorm(x, mean = fit$mu[6], sd = sqrt(fit$sigma2[6])),
##       add = TRUE, col = "orange")
mix.print(fit)
mix.dens(ltest, fit)




test = final.dt[Production_FAO_symb == "C" & Year == 2000,
  Production_FAO/Production_ICSD]
test = test[!is.na(test) & is.finite(test) & test != 0]
ltest = log(test)

par(mfrow = c(1, 2))
fit = smsn.mix(ltest, nu = 3, g = 6, get.init = TRUE,
         criteria = TRUE, group = TRUE, family = "Normal", calc.im = FALSE)
mix.hist(ltest, fit)
mix.print(fit)
mix.dens(ltest, fit)



## Looks like FAO in general is over estimating, but sometimes
## under-estimating.
with(final.dt[ExtRate_FAO <= 10000 & ExtRate_ICSD <= 10000, ],
     scatterhist(ExtRate_FAO, ExtRate_ICSD, breaks = 50,
                 xlab = "FAO", ylab = "ICSD",pch = 19,
                 col = rgb(0, 0, 0, alpha = 0.1),
                 main = "Comparison of Extraction rate of wheat flour"))

hist(final.dt[, Production_FAO - Production_ICSD], breaks = 1000,
     xlim = c(-1e7, 1e7))
abline(v = 0, col = "red", lty = 2)

## From the extraction it seems that we are either over-estimating
## production/input or under-estimating the extraction rate. The
## over-estimation of production is more likely.
with(final.dt[(Production_FAO - Production_ICSD)/Production_FAO < 0.05 &
              ExtRate_ICSD <= 10000, ],
     scatterhist(ExtRate_FAO, ExtRate_ICSD, breaks = 50,
                 xlab = "FAO Extraction", ylab = "ICSD Extraction",
                 main = "Comparison of extraction rate of wheat flour"))


