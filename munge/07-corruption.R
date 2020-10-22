#import functions
source("./lib/funcs.R")
#load filename
filename <- "./data/2017 sfr model data/2018_CPI_FullDataSet.xlsx"

#import the time series, and skip the first two lines to align column headers
ti_raw <- read_excel(filename, "CPI Timeseries 2012 - 2018", skip = 2)
#clean column names
ti_clean <- clean_names(ti_raw)
#determine which columns to keep
ti_clean_columns_keep <- c("iso3","cpi_score_2018","cpi_score_2017","cpi_score_2016",
                           "cpi_score_2015","cpi_score_2014","cpi_score_2013","cpi_score_2012")
ti_clean <- ti_clean[,ti_clean_columns_keep]

#turn dataset into long format
ti_long <- ti_clean %>% gather(year,value,-iso3)

ti_long <- na.omit(ti_long)
#get rid of prefix in year column
ti_long$year <- gsub("cpi_score_","",ti_long$year)
ti_long$year <- as.numeric(ti_long$year)

#apply country name and iso code alignment - first rename iso3 column
ti_long <- setDT(ti_long)[,.(iso3c = iso3,year,value)]
ti_long$iso3c <- country.code.name(ti_long$iso3c)

indicators <- raw.data$log %>% dplyr::filter(source == "TI")
ti_long$variablename <- unique(indicators$variablename)
raw.data$ti <- ti_long
rmExcept("raw.data")
