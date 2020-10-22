#source functions
source("./lib/funcs.R")

filename <- "./data/2017 sfr model data/V-Dem-CY-Full+Others-v9.csv"
vdem_raw <- fread(filename,select = c("country_name","year","v2x_jucon","v2xlg_legcon","v2xcs_ccsi","v2xcl_acjst","v2clsocgrp","v2svstterr",
                                      "v2x_clphy","v2xnp_client","v2x_feduni","v2x_genpp"))

#subset relevant columns for analysis, and filter by year > 2000 to avoid old countries being included
vdem <- setDT(vdem_raw)[year > 2000,.(country_name,year,v2x_jucon,v2xlg_legcon,v2xcs_ccsi,v2xcl_acjst,v2clsocgrp,v2svstterr,
                               v2x_clphy,v2xnp_client,v2x_feduni,v2x_genpp)]

vdem$iso3c <- country.code.name(vdem$country_name) #note that this does not produce a match for Zanzibar; that's fine, exclude it from the analysis.
#fyi - as of September 30, 2020, due to an update of the countrycode package, countrycode does not recognise Somaliland as SOM. Hard code it for the sake of consistency with the report results.
vdem$iso3c <- ifelse(vdem$country_name == "Somaliland","SOM",vdem$iso3c)

#note that there are two values for Palestine - Palestine/Gaza and Palestine/West Bank. The iso3 code for them is the same - PSE.
#note also that there are two values for 'SOM' - Somalia and Somaliland. Below, there is code to take the minimum value between both,
#which is what was done in previous years.
#run a quick check to ensure there aren't problem with other countries:
#vdem.n <- vdem[,.(.N),.(iso3c)][order(N)] #this only outputs PSE and SOM as being problematic - SSD is going to have less years because South Sudan
#became a country in 2011
#change from wide to long, and pick out relevant variables, and change variablename to character
setDT(vdem)
vdem <- melt(vdem,id.vars = c("iso3c","year","country_name"))
vdem <- vdem[,.(iso3c,variablename = as.character(variable),year,value)]
vdem <- na.omit(vdem) #this is the step you lose Zanzibar

#take care of the issues with Palestine and Somalia here -
#take the minimum value as the tiebreaker (does this for all countries, but it only matters for Palestine and Somalia/Somaliland)
vdem <- vdem %>% group_by(iso3c, variablename, year) %>% 
  dplyr::summarise(value = min(value))
setDT(vdem)

# GINI --------------------------------------------------------------------
##this script incorporates GINI estimates from v-8 of VDEM, as v-9 did not 
##report GINI. Integrate this into the main vdem dataset after processing.
gini_raw_filename <- "./data/2017 sfr model data/V-Dem-CY+Others-v8.csv"
gini_raw <- fread(gini_raw_filename, select = c("country_name","year",
                                                "e_peginiwi"),encoding = "UTF-8")
gini_clean <- gini_raw[year >= 2012,.(iso3c = country_name,variablename = "e_peginiwi",
                                      year,value = e_peginiwi)]
gini_clean$iso3c <- country.code.name(gini_clean$iso3c)
#this does not match Zanzibar - that's fine as there's no value.
#there aren't values for Palestine or Somaliland, so the earlier adjustment does not matter.  
gini_clean <- na.omit(gini_clean)
#remove VNM - this a long explanation (contact Harsh Desai if you're curious) but has to do with package and dataset versioning...
gini_clean <- gini_clean %>% filter(iso3c != "VNM")

##need GINI estimates for St. Lucia and Samoa from the World Bank
filename <- c("./data/2017 sfr model data/GINI_WB_2019.csv")
wb_gini <- fread(filename,header = T)
wb_gini_clean <- clean_names(wb_gini)
wb_gini_filtered <- wb_gini_clean[country_name %in% c("St. Lucia","Samoa")] %>%
  select(-c("country_name","indicator_name","indicator_code")) %>% 
  tidyr::pivot_longer(-country_code,names_to = "year",values_to = "value") %>% 
  filter(complete.cases(value))

wb_gini_filtered$year <- gsub("x","",wb_gini_filtered$year)
wb_gini_filtered$year <- as.numeric(wb_gini_filtered$year)

wb_gini_final <- wb_gini_filtered %>% dplyr::group_by(iso3c = country_code) %>% 
  dplyr::filter(year == max(year)) %>% dplyr::select(iso3c,year,value) %>% dplyr::mutate(variablename = "e_peginiwi",
                                                                    value = as.numeric(value),
                                                                    variablename = as.character(variablename))
setDT(wb_gini_final)
vdem <- rbind(vdem,gini_clean,wb_gini_final)
vdem <- na.omit(vdem) #one last omit check
vdem$iso3c <- country.code.name(vdem$iso3c)

vdem <- vdem %>% dplyr::filter(year >= 2012)
raw.data$vdem <- vdem
rmExcept("raw.data")