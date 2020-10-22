# # Example preprocessing script.
source("./lib/funcs.R")
filename <- "./data/2017 sfr model data/2-GPI 2019 banded scores 2008-2019.xlsx"

dataset <- filename %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map_df(~ read_excel(path = filename,sheet = .x,range = "A4:Z167"),.id = "year") %>% 
  clean_names() %>% 
  dplyr::select(year,country,armed_services_personnel_rate,police_rate) %>% 
  pivot_longer(names_to = "variablename",values_to = "value",-c(country,year)) %>% 
  dplyr::filter(complete.cases(country))
dataset$iso3c <- country.code.name(dataset$country)
dataset$iso3c <- country.code.name(dataset$iso3c)
dataset_final <- setDT(dataset)[,.(iso3c,year,variablename,value)]
dataset_final <- dataset_final %>% 
  dplyr::mutate(variablename = gsub("armed_services_personnel_rate","ARMY",variablename),
         variablename = gsub("police_rate","POLI",variablename))
dataset_final <- na.omit(dataset_final)
raw.data$state.forces <- dataset_final
rmExcept("raw.data")
