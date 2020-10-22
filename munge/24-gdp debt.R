# # Example preprocessing script.
source("./lib/funcs.R")
filename <- "./data/2017 sfr model data/WEOOct2019all.xlsx"
weo_raw <- read_xlsx(filename) #the warning on import has to do with blank cells - this is fine.
weo_clean <- clean_names(weo_raw)
weo_clean <- setDT(weo_clean)[weo_subject_code == "GGXWDG_NGDP",-c("weo_country_code",
                                                            "subject_notes","weo_subject_code",
                                                            "units","subject_descriptor","scale",
                                                            "country_series_specific_notes")]

#estimates are based on statistical information available through Sept. 30, 2019 - 
#we are deviating below from previous years' practice - however, using estimates can be imprecise, such as in the case of Venezuela. 
#Instead, we will use the value of whichever column is the same as the 'estimates start after' year for a given country. 
#This way we can reduce the arbitrariness of using a random year and assigning it to the year of 'estimates_start_after' (which is what was done in
#2016 and 2018)
#The year under 'estimates start after' should be your latest year, and then everything before that is used in the time series. 
##This reduces the uncertainty of estimates while ensuring that you are using the full time series instead of imputations. 

#first make sure that all columns are the same type - change them to character for now, and then get rid of the last column (estimates start after)
weo_long <- weo_clean %>% mutate_all(~as.character(.)) %>% select(-country)

#then change to long format and reformat all the columns correctly
weo_long <- weo_long %>% gather(.,"year","value",-c("iso","estimates_start_after"))
weo_long$year <- gsub("x","",weo_long$year)
weo_long <- weo_long %>% mutate_at(c("estimates_start_after",
                                     "year",
                                     "value"),~as.numeric(.))
#follow these steps carefully:
weo_subset <- weo_long %>% filter(estimates_start_after == year) %>% select(-estimates_start_after)

weo_subset_early_yrs <- weo_long %>% filter(year < estimates_start_after) %>% select(-estimates_start_after)

#bind the above two datasets - this means that estimates will not be used - additional checks show that this is fine practice.
weo_final <- rbind(weo_subset,weo_subset_early_yrs) %>% arrange(year)
weo_final <- na.omit(weo_final)
weo_final$variablename <- "General government gross debt"
weo_final$iso3c <- country.code.name(weo_final$iso)
weo_final <- setDT(weo_final)[,.(iso3c,year,value,variablename)]
raw.data$gdpdebt <- weo_final
rmExcept("raw.data") 