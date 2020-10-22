#import functions
source("./lib/funcs.R")
#import filename
filename <- "./data/2017 sfr model data/homicide_country_data_updated2019.csv"
homi_raw <- fread(filename,header = TRUE,strip.white = TRUE)
homi_clean <- clean_names(homi_raw)
#remove unnecessary columns - see below for columns we're keeping - to update, just need to add one extra year
homi_clean <- setDT(homi_clean)[,.(iso3c = country,variablename = indicator,x2000,x2001,x2002,x2003,x2004,x2005,x2006,x2007,
                            x2008,x2009,x2010,x2011,x2012,x2013,x2014,x2015,x2016,x2017)]
#change to long format
homi_long <- homi_clean %>% gather(year,value,-c(iso3c,variablename))
homi_long$year <- gsub("x","",homi_long$year)
homi_long$year <- as.numeric(homi_long$year) #ensure year is numeric
#there're multiple values for some countries due to the structure of UNODC data
# - here is how you treat those, based on precedent established in SFR 2018:
# for Iraq (Kurdistan Region and Central Iraq)
##first, remove all NAs
homi_long <- na.omit(homi_long)
#then assign Iraq to"Central Iraq" in the year 2014
homi_long$iso3c[homi_long$year == 2014 & homi_long$iso3c == "Iraq (Central Iraq)"] <- "Iraq"
#remove Kurdistan and Central Iraq from previous years using:
iraq_exclude <- c("Iraq (Central Iraq)","Iraq (Kurdistan Region)")
#compute the inverse of %chin% (a data.table function) using Negate, which is a base R function
"%!chin%" <- Negate("%chin%")
homi_long <- setDT(homi_long)[iso3c %!chin% iraq_exclude] #this excludes rows where the strings in iraq_exclude are present

##for UK, the previous dataset uses "United Kingdom of Great Britain and Northern Ireland" - this isn't 
##exactly right, but it's close enough and we should follow precedent...
##to fix this - spell out what you're excluding explicitly:
uk_exclude <- c("United Kingdom (Scotland)","United Kingdom (England and Wales)",
                "United Kingdom (Northern Ireland)")
#exclude the 'uk_exclude' territories using %!chin%, which is previously computed as the inverse of 
#%chin%, which is a function in data.table
homi_long <- homi_long[iso3c %!chin% uk_exclude]

###note: this process will address all name duplications (Iraq and UK were the only instances) - 
###you can double-check by exporting the dataframe
###and doing a pivot table in Excel to ensure that all countries have one value per year.###

#align country names and iso codes 
homi_long$iso3c <- country.code.name(homi_long$iso3c)
homi_long$iso3c <- country.code.name(homi_long$iso3c) #do this twice to get back to standardized country names

raw.data$homi <- homi_long
rmExcept("raw.data") 