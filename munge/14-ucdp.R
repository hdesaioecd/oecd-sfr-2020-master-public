source("./lib/funcs.R")

########### NON-STATE + ONE-SIDED CONFLICT WITH GEO DATA ####################
load("./data/2017 sfr model data/ged191.RData")
ucdp_syria <- readRDS("./data/2017 sfr model data/ged_syria.RDS")

#import the latest UCDP data release ONLY to make adjustments to the time series in Syria prior to 2016. This is tricky, and this adjustment was only made due to
#the availability of data at the time of analysis. At time of analysis, Syria_GED (UCDP 2018 version) did not have a time series prior to 2016. 
#On June 16, 2020, UCDP released 
#an update featuring a time series for Syria. However, in order to not upset the analysis for the framework in latest years, 
#we are using the 2018 version of the UCDP dataset for all countries from 2016 to 2018, 
#and for the time series for all countries except Syria. For Syria, we are using the UCDP 2019 release (on 16/6/2020) ONLY for years
#prior to 2016, to fill in the time series. This adjustment is sub-optimal and will be streamlined in future frameworks using UCDP data - 
#see caveats in spreadsheet.
#HOWEVER, despite this adjustment, the time series of Syria is consistent across both versions, with only very slight differences
#(i.e., 8,261 deaths using UCDP 2018 in 2018 vs. 8,131 deaths using UCDP 2019 in 2018). Thus, we feel confident that this approach is not disruptive.

load("./data/2017 sfr model data/ged201.RData")

ucdp_ns <- ged191 %>%
  dplyr::select(-c(geom_wkt,geometry)) %>% 
  as_tibble() %>% 
  dplyr::filter(type_of_violence != 1 & side_a_new_id >= 160) %>% 
  dplyr::select(country, year, best) %>%
  dplyr::group_by(year,country) %>% 
  dplyr::summarise(value = sum(best)) %>%
  dplyr::rename(location = country)

ucdp_syria_ns <- ucdp_syria %>% 
  dplyr::select(-c(geom_wkt,geometry)) %>% 
  as_tibble() %>% 
  dplyr::filter(type_of_vi != 1 & side_a_new >= 160,
                                              year < 2019) %>%
  dplyr::select(country,year,best) %>% 
  dplyr::group_by(year,country) %>% dplyr::summarise(value = sum(best)) %>% 
  dplyr::rename(location = country) %>% 
  dplyr::mutate(location = as.character(location))

ucdp_syria_previous <- ged201 %>%
  dplyr::filter(type_of_violence != 1 & side_a_new_id >= 160,
                country == "Syria",
                year %in% c(1989:2015)) %>% 
  dplyr::group_by(year,location = country) %>% 
  dplyr::summarise(value = sum(best,na.rm = T))

ucdp_syria_ns <- rbind(ucdp_syria_ns,ucdp_syria_previous) %>% dplyr::arrange(year)
                                              
ucdp <- rbind(ucdp_ns,ucdp_syria_ns)

####Adjust character names in location column, i.e., North Yemen becomes Yemen, and create iso3c variable
ucdp$location <- gsub("Yemen \\(North Yemen\\)", "Yemen", ucdp$location)
ucdp$iso3c <- country.code.name(ucdp$location)

# Per capita calculation for NS -------------------------------------------

#incorporate population estimates
#note these are time series
#also need to multiply value by 1000 (as raw source data is expressed in thousands)
#using UNDESA estimates
pop_raw <- read_excel("./data/population data/world_pop_2018_timeseries.xlsx",skip = 16)
pop_clean <- clean_names(pop_raw)
#need to filter out the following row because otherwise, the country.code.name function will pick that up as
#the population of China, which is incorrect (due to the string 'China' being in the name'). 
#only keeping population 2007 >
pop_clean <- setDT(pop_clean)[type == "Country",
                       .(iso3c = region_subregion_country_or_area,x2007,x2008,x2009,x2010,x2011,x2012,x2013,
                         x2014,x2015,x2016,x2017,x2018)] #note that the values are all character vectors
pop_clean$country <- country.code.name(pop_clean$iso3c)
pop_gathered <- pop_clean %>% dplyr::select(-country) %>% tidyr::gather(year,value,-c(iso3c))
pop_gathered$year <- gsub("x","",pop_gathered$year) #get rid of the x in the year column
pop_gathered$year <- as.numeric(pop_gathered$year) #ensure year variable is numeric
pop_gathered$value <- as.numeric(pop_gathered$value)*1000 #multiple here by 1000 for absolute values, and ensure 
#they are numeric instead of character
pop_gathered <- setDT(pop_gathered)[,.(iso3c,year,population = value)] 

setDT(ucdp)
#ensure both ucdp and population datasets have iso and not country names.
pop_gathered$iso3c <- country.code.name(pop_gathered$iso3c)

ucdp_merged <- merge(ucdp,pop_gathered,by = c("iso3c","year"))
ucdp_merged$value <- ucdp_merged$value/(ucdp_merged$population/100000) #this computes per capita estimates
#reduce to four key variables: 
ucdp_merged <- ucdp_merged[,.(iso3c,year,value,variablename = "non_state_one_sided")]
#note: the following line will not run if you don't have multiple dataframes in your raw.data list. 
ucdp_merged <- add.zeros.for.missing.countries(ucdp_merged,raw.data) #warnings are fine - coercing factor into character
#extend time series
ucdp_merged <- extend.time.series(ucdp_merged,replace.with = 0)

# Calculate three-year avg. -----------------------------------------------

#load the zoo package - you will use the rollmeanr function, set to 3 
library(zoo)
#be sure to group by iso3c - this piece of code is pedantic at the last two steps
#but shows, step-by-step, what happens:
ucdp_merged_final <- ucdp_merged %>% group_by(iso3c) %>% #group by iso3c
    dplyr::mutate(finalvalue = rollmeanr(value,k = 3,fill = 0)) %>% dplyr::select(-value) %>% #use rollmeanr, k = 3
    dplyr::rename(value = finalvalue)

ucdp_merged_final$iso3c <- country.code.name(ucdp_merged_final$iso3c) 
raw.data$ucdp <- ucdp_merged_final
rmExcept("raw.data") 