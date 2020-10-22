source("./lib/funcs.R")
filename <- "./data/2017 sfr model data/institutional profiles database.xlsx"
ipd <- read_excel(filename, "Indicators",skip = 2) #need to skip by two because of the way the spreadsheet is formatted
ipd_clean <- clean_names(ipd) #clean names to ensure standardization
ipd_clean <- setDT(ipd_clean)[,.(iso3c = iso_3_code,year,value = organisations_violentes,variablename = "A204")]

#before reshaping country codes, need to change string for iso names so that they are not excluded
#namely for Eritrea and Comoros
#this is tricky because ER also appears in other strings - to do this, convert iso3c to a factor
ipd_clean$iso3c <- as.factor(ipd_clean$iso3c)

#then, rename the levels of the factor
ipd_clean$iso3c <- plyr::revalue(ipd_clean$iso3c,c("ER" = "ERI","KM" = "COM"))

#finally, reconvert to characters
ipd_clean$iso3c <- as.character(ipd_clean$iso3c)

#now, apply country code name - just once
ipd_clean$iso3c <- country.code.name(ipd_clean$iso3c)
raw.data$ipd <- ipd_clean
rmExcept("raw.data")
