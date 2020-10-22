source("./lib/funcs.R")
#ILO is the primary source for NEET data - fill-in gaps with OECD data where available.
filename <- c("./data/2017 sfr model data/ILO_NEET.xlsx")
ilo_raw <- read_excel(filename)
#creates new names - total...4 is % of youth - this is the one used. 
ilo_clean <- clean_names(ilo_raw)
indicator_name <- "Share of youth not in education, employment, or training, total (% of youth population)"
#note that this dataset captures a time series
ilo_clean <- setDT(ilo_clean)[,.(iso3c = reference_area,year = time, value = total_4,
                                 variablename = indicator_name)]
#apply country.code function twice to standardize country names
ilo_clean$iso3c <- country.code.name(ilo_clean$iso3c)
ilo_clean <- na.omit(ilo_clean) #omit all NAs - this should get rid of rows for ANT, which is fine

#check that ILO data covers all OECD countries.
oecd.countries <- read_excel("./data/additional data/OECD countries.xlsx") #note as of 28-Oct-2019, there are 36 OECD countries
oecd.countries$iso3c <- country.code.name(oecd.countries$Country) #add iso3c column
#the code below should show 36 countries. it is showing 35. the missing one is South Kore, but that's not available in ILOSTAT estimates either.
test <- intersect(ilo_clean$iso3c,oecd.countries$iso3c)
#there is thus no value in further integrating OECD data
ilo_clean$iso3c <- country.code.name(ilo_clean$iso3c)
raw.data$neet <- ilo_clean
rmExcept("raw.data") 
