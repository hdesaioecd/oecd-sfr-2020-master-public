source("./lib/funcs.R")
filename <- "./data/2017 sfr model data/urbanisation_updated2019.csv"
urbanisation_raw <- fread(filename,skip = 4,header = TRUE,encoding = "UTF-8")
urbanisation_raw$V64 <- NULL #get rid of this odd variable that pops up due to fread
#define function for data cleaning
urbanisation_clean <- clean_names(urbanisation_raw)
urbanisation_clean <- urbanisation_clean[,!c("country_code","indicator_name","indicator_code")] %>% 
  dplyr::rename(iso3c = country_name)

urbanisation_melted <- melt(urbanisation_clean,id.vars = "iso3c",
                            variable.name = "year")
urbanisation_melted$year <- gsub("x","",urbanisation_melted$year) #use this to replace the x that emerges from
#clean_names function.
#change variable names and classes - add variablename - these four columns are essential for later code.
urbanisation_subset <- urbanisation_melted[,.(iso3c,
                                              year = as.numeric(as.character(year)), #convert year to numeric
                                              value,
                                              variablename = "Urban population growth (annual %)")]
urbanisation_subset$iso3c <- country.code.name(urbanisation_subset$iso3c)
urbanisation_subset <- na.omit(urbanisation_subset)
urbanisation_subset$iso3c <- country.code.name(urbanisation_subset$iso3c)
raw.data$urbanisation <- urbanisation_subset
rmExcept("raw.data")