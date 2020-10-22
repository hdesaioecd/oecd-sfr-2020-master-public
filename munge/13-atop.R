#load functions
source("./lib/funcs.R")
filename <- "./data/2017 sfr model data/atop4_01sy.csv"
#Note: you should really read the codebook
#import the dataset and then filter year > 2000 to make it more manageable (and also so that it does not capture obsolete countries such as 
#Bavaria, which aren't needed)
atop_raw <- fread(filename) %>% filter(year >= 2000)
#the first column, state, is using COW codes - need to convert to country names or iso codes
#this is already built in to the country code function in the country code package.
#see link to the origin and destination here: https://cran.r-project.org/web/packages/countrycode/countrycode.pdf
#note: after the  countrycode function, there will not be a match for 816 from the correlates of war dataset. This is an issue with the
#function itself, as it should be capturing Vietnam as 816. Insert this mapping as a custom match.
atop_raw$iso3c <- countrycode(atop_raw$state,"cown","country.name",custom_match = c("816" = "Vietnam"))
atop_raw$iso3c <- countrycode(atop_raw$iso3c,"country.name","iso3c")
atop_raw$iso3c <- country.code.name(atop_raw$iso3c)
#the key value is the "number" variable, which measures the total number of alliances the state is a member of during the year of observation
atop_clean <- setDT(atop_raw)[,.(iso3c,year,value = number,variablename = "atop")]
raw.data$atop <- atop_clean
rmExcept("raw.data") 
