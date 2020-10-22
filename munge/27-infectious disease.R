# Example preprocessing script.
source("./lib/funcs.R")
indicators <- raw.data$log %>% dplyr::filter(source == "GBD and CSIS")
filename_new <- "./data/2017 sfr model data/GBD_IHME_latest_updated.csv"
infdis_raw <- fread(filename_new)
infdis_clean <- clean_names(infdis_raw)
#get rid of the measure, sex, age, and cause columns because there's only one value for them all.
#filter 'metric' to number, which aligns with SFR 2018
#change name of location column to iso3c
#don't need min and max
infdis_clean_final <- setDT(infdis_clean)[,.(value = sum(val,na.rm = T)),.(iso3c = location,year)
                                          ][,variablename := indicators$variablename[1],]
infdis_clean_final$iso3c <- country.code.name(infdis_clean_final$iso3c)

#import population statistics
filename_pop <- "./data/population data/world_pop_2018_timeseries.xlsx"
pop_raw <- read_excel(filename_pop,skip = 16)
#clean population dataset - remove unnecessary columns, change the name of the country column, and filter out country categories that will 
#wrongly assign iso codes in the country.code.name function.
pop_clean <- clean_names(pop_raw) %>% dplyr::select(-c(index,variant,notes,country_code,parent_code)) %>%
  dplyr::rename(iso3c = region_subregion_country_or_area) %>% 
  filter(type == "Country") %>% dplyr::select(-type) %>% 
  pivot_longer(names_to = "year",
               values_to = "population",-iso3c)
pop_clean$iso3c <- country.code.name(pop_clean$iso3c)
pop_clean$year <- gsub("x","",pop_clean$year)
pop_clean <- na.omit(pop_clean) %>% dplyr::mutate(year = as.numeric(year),
                                           population = as.numeric(population)*1000/100000)

infdis_joined <- infdis_clean_final %>% dplyr::left_join(pop_clean,by = c("iso3c","year")) %>% 
  dplyr::mutate(value = value/population) %>% dplyr::select(-population)

infdis_joined$iso3c <- country.code.name(infdis_joined$iso3c)
#check that there are no NAs in the iso3c column - there are not
raw.data$disease <- infdis_joined
rmExcept("raw.data")