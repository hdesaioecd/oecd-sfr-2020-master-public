# # Example preprocessing script.
source("./lib/funcs.R")
indicators <- raw.data$log %>% dplyr::filter(source == "WB")
filename <- "./data/2017 sfr model data/GDP_percapita_growthrate_2019.csv"
gdp_growth <- fread(filename,header = TRUE)
gdp_growth$V65 <- NULL #delete odd column that appears during import process - it is N/A
gdp_growth_clean <- clean_names(gdp_growth) #clean column headers
gdp_growth_clean <- setDT(gdp_growth_clean)[,!c("indicator_name","indicator_code","country_name")] %>% 
  dplyr::rename(iso3c = country_code)
gdp_growth_clean <- gdp_growth_clean %>% dplyr::mutate_if(is.logical,~as.numeric(.)) #change logical columns to numeric so that melting doesn't spit out an error
gdp_growth_melted <- melt(gdp_growth_clean,id.vars = "iso3c",variable.name = "year")
gdp_growth_melted$year <- gsub("x","",gdp_growth_melted$year)
gdp_growth_melted$year <- as.numeric(gdp_growth_melted$year)
gdp_growth_melted$iso3c <- country.code.name(gdp_growth_melted$iso3c)
gdp_growth_melted <- na.omit(gdp_growth_melted)
gdp_percap_final <- setDT(gdp_growth_melted)[,.(iso3c,year,value,variablename = "GDP per capita growth rate")]
raw.data$gdpgrowth <- gdp_percap_final
rmExcept("raw.data") 