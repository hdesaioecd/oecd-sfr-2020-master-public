# 2017 Example preprocessing script.
source("./lib/funcs.R")
filename <- "./data/2017 sfr model data/Global Terrorism Index 2018 Overall Scores.xlsx"
gti_clean <- read_excel(filename,skip = 2) %>% 
  clean_names()
#change from wide to long format, and adjust type of value column to numeric; rename country column
gti_long <- gti_clean %>% 
  pivot_longer(names_to = "year",values_to = "value",-country) %>% 
  dplyr::mutate(value = as.numeric(value)) %>% 
  dplyr::rename(iso3c = country)
gti_long$year <- gsub("x","",gti_long$year) #remove x from year column
gti_long$year <- as.numeric(gti_long$year) #convert year to numeric
gti_long$variablename <- "gti" #create variable name

#align country code names
gti_long$iso3c <- country.code.name(gti_long$iso3c)

#add zeroes for missing countries
gti_long <- add.zeros.for.missing.countries(gti_long,raw.data)

#align country names again
gti_long$iso3c <- country.code.name(gti_long$iso3c)

raw.data$gti <- gti_long
rmExcept("raw.data")
