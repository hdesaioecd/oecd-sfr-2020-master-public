# # Example preprocessing script.
source("./lib/funcs.R")
filename <- "./data/2017 sfr model data/Education Index - 2019 update.csv" #imports 2019 update to education index, with 2018 being latest year
education <- fread(filename,skip = 1,header = T) %>% clean_names() %>% 
  remove_empty("cols") %>% 
  dplyr::rename_(.dots = setNames(names(.),tolower(gsub("x","",names(.))))) %>% 
  pivot_longer(names_to = "year",values_to = "value",-c(country,hdi_rank_2018)) %>%
  dplyr::mutate(value = as.numeric(value),
         variablename = "HDI Education",
         year = as.numeric(year)) %>% 
  dplyr::select(iso3c = country,year,value,variablename)
  
#apply country names
education$iso3c <- country.code.name(education$iso3c)
education$iso3c <- country.code.name(education$iso3c)
education_final <- education %>% filter(complete.cases(.))
raw.data$education <- education_final
rmExcept("raw.data") 
