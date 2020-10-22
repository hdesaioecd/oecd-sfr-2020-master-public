# # Example preprocessing script.
source("./lib/funcs.R")
wb <- fread("./data/2017 sfr model data/ResourceRentDependence_Updated2019.csv",header = TRUE) #latest year is 2017.
wb$V64 <- NULL #eliminate strange column that appears as a result of importing spreadsheet.
wb <- wb %>% dplyr::rename(iso3c = `Country Name`, variablename = `Indicator Name`) %>% select(-c('Country Code','Indicator Code')) %>% 
  gather(year,value,-c(iso3c,variablename)) %>% select(iso3c,variablename,year,value)
wb$iso3c <- country.code.name(wb$iso3c)
wb <- na.omit(wb)
wb$iso3c <- country.code.name(wb$iso3c)
raw.data$wb <- wb
rmExcept("raw.data")
