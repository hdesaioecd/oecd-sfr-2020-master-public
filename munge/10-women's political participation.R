source("./lib/funcs.R")
filename <- "./data/2017 sfr model data/Share of seats in parliament (% held by women).csv"
gender_pol_raw <- fread(filename,header = T)
gender_pol_clean <- gender_pol_raw %>% clean_names() %>% select(-c("indicator_name","indicator_code","v65")) %>% 
  pivot_longer(names_to = "year",values_to = "value",-c(country_name,country_code)) %>% 
  mutate(year = gsub("x","",year),
         iso3c = country.code.name(country_code)) %>% 
  filter(complete.cases(.))
#this is explicitly not taking 2019 - data was collected in Dec. 2020, and there is at least two months' lag. 
gender_pol_final <- setDT(gender_pol_clean)[year > 2000 & year < 2019,.(iso3c,variablename = "Share of seats in parliament",
                                               year =  as.numeric(year),value)]
raw.data$gender <- gender_pol_final
rmExcept("raw.data")
