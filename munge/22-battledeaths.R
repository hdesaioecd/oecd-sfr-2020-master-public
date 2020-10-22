source("./lib/funcs.R")
#import the latest UCDP data release ONLY to make adjustments to the time series in Syria prior to 2016. This is tricky, and this adjustment was only made due to
#the availability of data at the time of analysis. At time of analysis, Syria_GED (UCDP 2018 version) did not have a time series prior to 2016. 
#On June 16, 2020, UCDP released 
#an update featuring a time series for Syria. However, in order to not upset the analysis for the framework in latest years, 
#we are using the 2018 version of the UCDP dataset for all countries from 2016 to 2018, 
#and for the time series for all countries except Syria. For Syria, we are using the UCDP 2019 release (on 16/6/2020) ONLY for years
#prior to 2016, to fill in the time series. This adjustment is sub-optimal and will be streamlined in future frameworks using UCDP data - 
#see caveats in spreadsheet.
#HOWEVER, despite this adjustment, the time series of Syria is consistent across both versions, with only very slight differences
#(i.e., 11,974 deaths using UCDP 2018 in 2018 vs. 11,965 deaths using UCDP 2019 in 2018). Thus, we feel confident that this approach is not disruptive.

#import both UCDP releases, keeping in mind the caveats spelled out above. 
load("./data/2017 sfr model data/ged191.RData")
load("./data/2017 sfr model data/ged201.RData")

########### STATE-BASED CONFLICT WITH GEO DATA ####################
ucdp <- ged191 %>% 
  dplyr::select(-c(geom_wkt,geometry)) %>% 
  as_tibble() %>% 
  dplyr::filter(type_of_violence == 1) %>% 
  dplyr::select(country, year, best) %>%
  dplyr::group_by(year, country) %>% 
  dplyr::summarise(value = sum(best,na.rm = T)) %>% 
  dplyr::rename(location = country)

####Add Syria
ucdp_syria <- readRDS("./data/2017 sfr model data/ged_syria.rds") %>% 
  dplyr::select(-c(geom_wkt,geometry)) %>% 
  as_tibble() %>% 
  dplyr::filter(type_of_vi == 1) %>% 
  dplyr::select(country,year,best) %>% 
  dplyr::group_by(year,country) %>% dplyr::summarise(value = sum(best,na.rm = T)) %>% 
  dplyr::rename(location = country) %>% 
  dplyr::filter(year != 2019) %>% 
  dplyr::mutate(location = as.character(location))

####ADD PREVIOUS YEARS (BEFORE 2016 TO SYRIA)
ucdp_syria_previous <- ged201 %>% 
  dplyr::filter(type_of_violence == 1,
                country == "Syria",
                year %in% c(1989:2015)) %>% 
  dplyr::group_by(year,location = country) %>% 
  dplyr::summarise(value = sum(best,na.rm = T))

#combine full Syria time series here  
ucdp_syria <- rbind(ucdp_syria,ucdp_syria_previous) %>% 
  dplyr::arrange(year)

#rbind syria to the rest of the data
ucdp <- rbind(ucdp, ucdp_syria)

#create iso3c variable
ucdp$location <- gsub("Yemen \\(North Yemen\\)", "Yemen", ucdp$location)
ucdp$iso3c <- country.code.name(ucdp$location)

# Per Capita Adjustment ---------------------------------------------------
#incorporate population estimates
#note these are time series
#also need to multiply value by 1000 (as raw source data is expressed in thousands)
pop_raw <- read_excel("./data/population data/world_pop_2018_timeseries.xlsx",skip = 16)
pop_clean <- clean_names(pop_raw)
#need to filter out this row because otherwise, the country.code.name function will pick that up as
#the population of China, which is incorrect (due to the string 'China' being in the name'). 
pop_clean <- setDT(pop_clean)[type == "Country",
                              .(iso3c = region_subregion_country_or_area,
                                x2010,x2011,x2012,x2013,x2014,x2015,x2016,x2017,x2018)]
pop_clean$country <- country.code.name(pop_clean$iso3c)
pop_clean <- na.omit(pop_clean)
pop_gathered <- pop_clean %>% tidyr::gather(year,value,-c(country,iso3c))
pop_gathered$year <- gsub("x","",pop_gathered$year)
pop_gathered$year <- as.numeric(pop_gathered$year) #creates a warning of NAs introduced by coercion,
#but that's fine for this instance.
pop_gathered$value <- as.numeric(pop_gathered$value)*1000  #creates a warning of NAs introduced by coercion,
#but that's fine for this instance.
setDT(pop_gathered)
pop_gathered <- pop_gathered[,.(iso3c,year,population = value)]

#integrate back into the dataset.
pop_gathered$iso3c <- country.code.name(pop_gathered$iso3c)
ucdp <- merge(ucdp,pop_gathered,by = c("iso3c","year")) #this removes all ucdp values before 2010;
#that's fine for our time series.

#compute deaths per 100,000
ucdp <- setDT(ucdp)[,.(iso3c,year,value = value/(population/100000))]

# address outliers here by taking the log
ucdp$value <- log(ucdp$value + 1)

indicators <- raw.data$log %>% dplyr::filter(source == "UCDP-BD")
ucdp$variablename <- indicators$variablename
ucdp <- ucdp[, c("iso3c", "variablename", "year", "value")]
#### add zeros for missing values
ucdp <- add.zeros.for.missing.countries(ucdp, raw.data) #adds zero for countries with no observations
ucdp <- extend.time.series(ucdp, replace.with = 0) #adds zero for years with no observations

###### 
ucdp$iso3c <- country.code.name(ucdp$iso3c)
raw.data$bd <- ucdp
rmExcept("raw.data") 