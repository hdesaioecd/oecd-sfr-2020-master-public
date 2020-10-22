source("./lib/funcs.R")
indicators <- raw.data$log %>% dplyr::filter(source == "OECD")
filename <- "./data/2017 sfr model data/SIGI_updated2019.csv"
sigi_raw <- fread(filename)
sigi_clean <- clean_names(sigi_raw)
#In the dataframe, RPI__2 == restricted physical integrity value
#assign year 2017 - this was confirmed to be the year of data collection and analysis from Harsh correspondence with SIGI team
sigi_clean <- sigi_clean[var == "RPI__2" & region_2 == "All regions" & 
                           income_2 == "All income groups",.(iso3c = country,
                                                                       year  = 2017,
                                                                       variablename = "sigi",
                                                                       value)] 
#using RPI_2 code for restricted physical integrity under "var".
#filter out "all regions" and "all income groups" because otherwise 
#it will create duplicate values for each country, as each country
#is assigned a region and an 'all regions'/income group + "all income groups" classification. 
#finally, ensure year is a numeric variable
sigi_clean <- na.omit(sigi_clean) #this should not delete any rows - nrow = 132; use as check. 
sigi_clean$iso3c <- country.code.name(sigi_clean$iso3c)

#check for OECD countries that are not covered by SIGI
#this was a calculation done in the previous SFR 2018
#first, import list of OECD countries and standardize their iso3c codes
oecd.countries <- read_excel("./data/additional data/OECD countries.xlsx") #note as of 28-Oct-2019, there are 36 OECD countries
oecd.countries$iso3c <- country.code.name(oecd.countries$Country) #add iso3c column
#check that SIGI does not have this data, and then follow steps below to fill them in
##this should produce a vector with Israel, Iceland, and Luxembourg included in the list: 
oecd.countries.out <- setdiff(oecd.countries$iso3c, sigi_clean$iso3c)
#this should produce a vector with 33 OECD countries, which are covered by SIGI:
oecd.countries.in <- intersect(oecd.countries$iso3c, sigi_clean$iso3c)
#this should define a vector of 1:33 - check your environment to confirm
pos <- which(sigi_clean$iso3c %chin% oecd.countries.in) #%chin% is function in data.table and much faster than %in%
#this produces an average of the 33 OECD countries in oecd.countries.out, using the location of those countries in
#the sigi_clean dataset - so do not re-order the dataset before this! and check output of pos
#and average:
average <- mean(as.numeric(sigi_clean$value[pos]), na.rm = T)
oecd.countries.out <- data.frame(iso3c = oecd.countries.out, variablename = "sigi", year = 2017, value = average) #assign the average value to the 
#three countries in oecd.countries.out - ISL, ISR, and LUX. 
sigi_clean <- rbind(sigi_clean, oecd.countries.out)
#change iso3c to character vector, not factor
sigi_clean$iso3c <- as.character(sigi_clean$iso3c)

sigi_clean$iso3c <- country.code.name(sigi_clean$iso3c) #re-standardize to country names, not codes,
#which follows the convention in the rest of the framework
raw.data$sigi <- sigi_clean
rmExcept("raw.data") 
