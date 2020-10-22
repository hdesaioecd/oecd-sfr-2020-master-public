# # Example preprocessing script.
source("./lib/funcs.R")
filename <- "./data/2017 sfr model data/WB_Unemployment_updated2019.csv"
unemp <- fread(filename, header = TRUE,encoding = "UTF-8") #latest year is 2018
unemp$V64 <- NULL
unemp <- unemp %>% dplyr::rename(iso3c = `Country Name`, variablename = `Indicator Name`)
unemp <- unemp[, -c("Country Code", "Indicator Code")] #remove country and indicator code
unemp <- unemp %>% gather(year, value, -c(iso3c, variablename))
unemp <- setDT(unemp)[,.(iso3c,year = as.numeric(year),variablename = "unemp",value)] #keep these four columns
unemp$iso3c <- country.code.name(unemp$iso3c) 
unemp$iso3c <- country.code.name(unemp$iso3c) #apply twice to standardize country names (following convention in
#rest of script)
unemp <- na.omit(unemp) #remove all NAs
raw.data$unemp <- unemp
rmExcept("raw.data") 
