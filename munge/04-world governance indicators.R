source("./lib/funcs.R")

##code for integrating data from WGI data catalog (https://datacatalog.worldbank.org/dataset/worldwide-governance-indicators)
filename <- "./data/2017 sfr model data/WGIData_2019.csv"
wgi_raw <- fread(filename,header = TRUE)
wgi_clean <- wgi_raw %>% clean_names() %>% remove_empty("cols") #removes empty column at the end
indicators <- c("CC.EST","GE.EST","RQ.EST","RL.EST","VA.EST","PV.EST")
wgi_filtered <- setDT(wgi_clean)[indicator_code %in% indicators,
                          .(country_code,indicator_code,
                            x2000,x2002,x2003,x2004,x2005,x2006,x2007,
                            x2008,x2009,x2010,x2011,x2012,x2013,x2014,
                            x2015,x2016,x2017,x2018)]
indicator_names <- c("ControlofCorruption" = "CC.EST",
                     "GovernmentEffectiveness" = "GE.EST",
                     "RegulatoryQuality" = "RQ.EST",
                     "RuleofLaw" = "RL.EST",
                     "VoiceandAccountability" = "VA.EST",
                     "PoliticalStability" = "PV.EST")
wgi_gathered <- wgi_filtered %>% 
  tidyr::gather(year,value,-c(country_code,indicator_code)) %>%
  dplyr::mutate(variablename = as.factor(indicator_code),
               variablename = fct_recode(variablename,!!!indicator_names)) #this last line will revise the indicator names
wgi_gathered$year <- gsub("x","",wgi_gathered$year)

wgi_final <- setDT(wgi_gathered)[,.(iso3c = country_code,variablename,year,value)]
wgi_final$iso3c <- country.code.name(wgi_final$iso3c)
wgi_final <- na.omit(wgi_final)

raw.data$wgi <- wgi_final
rmExcept("raw.data")