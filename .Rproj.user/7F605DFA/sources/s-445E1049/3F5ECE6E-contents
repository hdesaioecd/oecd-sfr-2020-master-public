source("./lib/funcs.R")
filename <- "./data/2017 sfr model data/INFORM2020_TREND_2010_2019_v040_ALL_2.xlsx"
inform <- read_xlsx(filename)
#adjust the year variable so that it reflects date of collection, not date of publication
#according to the guidance from SFR 2018, INFORM variables are built 2 years out from the raw data (this is also confirmed).
#This means that INFORMYear - 2 is the correct year assignment - the previous SFR
#used DataYear - 1, which is also correct because DataYear = INFORMYear - 1. However, the most recent
#trends dataset does not have a DataYear, only an INFORMYear. It has a SurveyYear, but this is unreliable for many variables. Stick to the 
#convention of INFORM year. 
inform_clean <- setDT(inform)[,
                       .(iso3c = Iso3,value = IndicatorScore,
                         variablename = IndicatorName, year = INFORMYear - 2)]
#import INFORM variables - there should be five. 
indicators <- raw.data$log %>% dplyr::filter(source == "INFORM")
#change strings in variablename to match the indicator names for natural disasters risk and violent conflict risk
inform_clean$variablename <- gsub("Human Hazard", "Human",inform_clean$variablename)
inform_clean$variablename <- gsub("Natural Hazard", "Natural",inform_clean$variablename)

#the following adjustments do two things:
# 1) filter out irrelevant rows, in that they have iso3c that are not countries - this is necessary so that country.code.name does not
# spit out an error
# 2) filter to the relevant indicators
inform_final <- inform_clean[variablename %in% indicators$variablename &
                              nchar(iso3c) == 3]
#transform back to country names instead of three character codes - this is important for standardization for script 999. 
inform_final$iso3c <- country.code.name(inform_final$iso3c)
raw.data$inform <- inform_final
rmExcept("raw.data")
