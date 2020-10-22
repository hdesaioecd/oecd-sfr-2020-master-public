# This script processes all the indicators in a standard format for the PCA

source("./lib/funcs.R")
output_folders <- c("./data_out2020/")
if(!dir.exists("./data_out2020"))
{
  dir.create("./data_out2020")
}
save(list = ls(), file = "./cache/errortesting/errortesting.RData")
load("./cache/errortesting/errortesting.RData")

sfr.log <- raw.data$log %>% dplyr::select(reportname, variablename, dimension, type, doesmoreincreasefragility, 
                                          include)
old.raw <- raw.data

#set log in list to null because you don't need it.
raw.data$log <- NULL

#apply this to adjust the class of variables for each dataframe.
raw.data <- lapply(raw.data, function(x){
  x$iso3c <- as.character(x$iso3c)
  x$variablename <- as.character(x$variablename)
  x$year <- as.numeric(as.character(x$year))
  x$value <- as.numeric(as.character(x$value))
  return(x)
}) 

#combine all list elements into a dataset
raw.data <- bind_rows(raw.data) #note this has four variables: iso3c, variablename, year, value
#create country name column so that dataframe has both country names and iso3s
raw.data$country <- raw.data$iso3c
raw.data$iso3c <- country.code.name(raw.data$iso3c)
country.code.check = raw.data %>% dplyr::select(country, iso3c) %>% distinct()
write.csv(country.code.check, file = "./unit-tests/country-code-conversions.csv", row.names = F)
message("country code conversions written to ./unit-tests/country-code-conversions.csv")
message("****Please check that these are converted correctly****")
raw.data <- na.omit(raw.data) #get rid of all NAs

#dplyr::filter data that is relevant for the index (include = 1)
raw.data <- left_join(raw.data, sfr.log,by = "variablename") %>% 
  dplyr::filter(include == 1) %>% 
  dplyr::select(iso3c,country,dimension,type,reportname,year,value,doesmoreincreasefragility) %>% 
  dplyr::rename(variablename = reportname)

#check time series
time.span = raw.data %>% dplyr::group_by(variablename) %>%
  dplyr::summarise(min.year = min(year), max.year = max(year), timespan = max.year-min.year+1,
                   n = n()) %>% 
  dplyr::mutate(expected = timespan*175,
                delta = expected-n)
time.span.filter <- time.span %>% filter(timespan == 1)
fwrite(time.span.filter, "./graphs/only-one-year-of-data.csv")
fwrite(time.span,"./graphs/full_range_years.csv")

#test that data has formatted properly #i.e. one data point per country-variablename
sfr.time.series <- raw.data %>% dplyr::filter(year >= 2010)
#calculate most recent year here:
raw.data <- most.recent(raw.data)
raw.data <- raw.data %>% dplyr::filter(year >= 2010)

###### remove countries with less than threshold percentage of data
threshold <- 0.7
availability <- as.data.frame(table(raw.data$iso3c, raw.data$variablename)) %>% dplyr::rename(iso3c = Var1,
                                                                                              variablename = Var2)

missing_variables_export <- availability %>% dplyr::filter(Freq == 0)
fwrite(missing_variables_export,paste0(output_folders,"Data Availability - Missing Variables by Context.csv"))

availability <- availability %>% dplyr::group_by(iso3c) %>% dplyr::summarise(n = n(), missing = sum(Freq == 0)/n()) %>% 
  dplyr::mutate(iso3c = as.character(iso3c))
availability$location <- country.code.name(availability$iso3c)
availability$actual <- 1 - availability$missing
#export this
fwrite(availability,paste0(output_folders,"/Data Availability - by Context.csv"))

##import ODA eligible recipients
ODA_recipients <- read_xls("./data/additional data/DAC-CRS-CODES.xls",sheet = "Recipient") %>% 
  clean_names() %>% remove_empty("cols") %>% dplyr::select(-x13)
ODA_recipients$iso3c <- country.code.name(ODA_recipients$recipient_name_en)
ODA_recipients <- ODA_recipients %>% dplyr::filter(complete.cases(iso3c),dac_income_group != "MADCTs") %>% 
  dplyr::select(iso3c,dac_income_group,country = recipient_name_en,region)
##import regional and income group classifications from the World Bank here
WB_categories <- read_xls("./data/additional data/WB_2020_classifications.xls","Groups") %>% 
  clean_names() %>% 
  select(iso3c = country_code,region = group_name)

#import and join population
population <- read_excel("./data/population data/world_pop_2018_timeseries.xlsx",
                         sheet = "ESTIMATES",skip = 16) %>% clean_names() 
pop_bycountry <- population %>%  
  dplyr::filter(type == "Country") %>% dplyr::select(country = region_subregion_country_or_area,x2019) %>% 
  dplyr::mutate(x2019 = as.numeric(x2019))
pop_bycountry$iso3c <- country.code.name(pop_bycountry$country)

availability_wclass <- availability %>%
  dplyr::filter(actual > .7) %>% 
  left_join(WB_categories, by = "iso3c") %>%
  left_join(pop_bycountry %>% dplyr::select(-country), by = 'iso3c')

###by region
WB_categories_byregion <- WB_categories %>% dplyr::group_by(region) %>% 
  dplyr::summarise(count_total = n())

WB_categories_byregion_pop <- WB_categories %>% left_join(pop_bycountry,by = "iso3c") %>% 
  dplyr::group_by(region) %>% 
  dplyr::summarise(pop_all = sum(x2019,na.rm = T))

availability_byregion <- availability_wclass %>% dplyr::group_by(region) %>% 
  dplyr::summarise(count_coverage = n()) %>% left_join(WB_categories_byregion,by = "region") %>% 
  dplyr::mutate(proportion = count_coverage/count_total*100) 

availability_byregion_pop <- availability_wclass %>% dplyr::group_by(region) %>% 
  dplyr::summarise(pop_coverage = sum(x2019)) %>% 
  left_join(WB_categories_byregion_pop,by = "region") %>% 
  dplyr::mutate(proportion = pop_coverage/pop_all*100)

global_coverage <- availability %>% 
  dplyr::filter(actual > .7) %>% 
  left_join(pop_bycountry,"iso3c") %>% 
  dplyr::summarise(value = sum(x2019,na.rm = T))

###export all of this analysis
list_coverage <- list("Count" = availability_byregion,
                      "Population" = availability_byregion_pop,
                      "Global" = global_coverage)
openxlsx::write.xlsx(list_coverage,paste0(output_folders,"Data Availability - Region and IG Shares.xlsx"))
#remove countries here: 
availability <- availability %>% dplyr::filter(1 - missing >= threshold) %>% ungroup()
raw.data <- raw.data %>% dplyr::filter(iso3c %in% as.character(availability$iso3c))

data.matrix <- raw.data %>% dplyr::group_by(variablename) %>% dplyr::summarise(min.year = min(year), max.year = max(year),                                                                                num.countries = length(unique(iso3c)))
fwrite(data.matrix,paste0(output_folders,"Data Availability - by Indicator.csv"))

#raw data matrix
data.matrix <- raw.data %>% dplyr::select(iso3c, country, variablename, dimension, year, value) %>% distinct() %>%
  spread(year, value)
fwrite(data.matrix, paste0(output_folders,"/Raw Dataset (Latest Year).csv"))

# invert indicators to be in the same direction
pos <- raw.data$doesmoreincreasefragility == 0
raw.data$value[pos] <- -raw.data$value[pos]

pre_imputed <- raw.data %>% 
  dplyr::group_by(variablename) %>% 
  dplyr::summarise(n = n())

# impute ## CHECK HERE
raw.data <- impute(raw.data, use.precomputed = F)
raw.data <- raw.data %>% mutate()

#minor correction to ensure that formal alliances are not represented in fractions/decimals but rather whole numbers - round the imputed values.
raw.data <- raw.data %>% mutate(imputed = ifelse(variablename == "Formal alliances",round(imputed),imputed))

raw.data$country <- oecd.country.name(raw.data$iso3c, short = T)
raw.data$country <- iconv(raw.data$country, "latin1", "UTF-8")

#do the above for the time series - the latest year is what you will use for the PCA
#reverse the value of the variables where doesmoreincreasefragility = 0
pos <- sfr.time.series$doesmoreincreasefragility == 0
sfr.time.series$value[pos] <- -sfr.time.series$value[pos]

#only include the iso3c's in sfr.time.series - because they're the ones who meet the threshold.
sfr.time.series = sfr.time.series %>% dplyr::filter(iso3c %in% unique(raw.data$iso3c))

#create temporary dataset - this code is pedantic, but remove value column and change the name of the 
#imputed column to reflect value. the only difference between temp and raw.data is that column.
temp = raw.data %>% dplyr::select(-value) %>% 
  dplyr::rename(value = imputed)
temp = temp[, names(sfr.time.series)]
temp_counted <- temp %>% group_by(variablename) %>% 
  dplyr::summarise(n = n())

export_imputation <- pre_imputed %>% 
  dplyr::left_join(temp_counted,"variablename") %>% 
  dplyr::rename(actual = n.x,
                total = n.y) %>% 
  dplyr::mutate(difference = total-actual) %>% 
  dplyr::arrange(-difference)
fwrite(export_imputation,paste0(output_folders,"./imputed.csv"))

sfr.time.series <- rbind(sfr.time.series, temp)  %>% dplyr::distinct() #use distinct values, thereby
#removing possible duplication on the rbind between sfr.time.series and temp
sfr.time.series <- setDT(sfr.time.series)[,.(value = mean(value,na.rm = T)),.(iso3c,year,variablename)] 
#you can check with this,-> if you do unique(test$N), value = 1
#test <- sfr.time.series[,.N,.(iso3c,year,variablename)]
# sfr.time.series <- interpolate(sfr.time.series %>% dplyr::select(iso3c, year, variablename, value))
# sfr.time.series <- setDT(sfr.time.series)[,.(iso3c,year,reportname = variablename,value,imputed = yhat)]
# sfr.time.series <- left_join(sfr.time.series,sfr.log,by = "reportname")
# sfr.time.series <- setDT(sfr.time.series)[year >= 2010 & include == 1,.(iso3c,year,variablename = reportname,imputed,dimension,type,
#                                                                  doesmoreincreasefragility,include)]
# sfr.time.series$value <- sfr.time.series$imputed

sfr.time.series <- interpolate(sfr.time.series %>% dplyr::select(iso3c, year, variablename, value)) %>%
  dplyr::rename(imputed = yhat, reportname = variablename) %>%
  left_join(sfr.log,by = "reportname") %>%
  dplyr::filter(year >= 2010,include == 1) %>%
  dplyr::select(-variablename) %>%
  dplyr::rename(variablename = reportname) %>%
  dplyr::mutate(value = imputed)

sfr.time.series$imputed = round(sfr.time.series$imputed,3)
raw.data$imputed = round(raw.data$imputed,3)
x = most.recent(sfr.time.series) %>% select(-year) %>% arrange(iso3c, variablename) %>%
  dplyr::select(iso3c, variablename,imputed)
y = raw.data  %>% select(-year) %>% arrange(iso3c, variablename) %>%
  dplyr::select(iso3c, variablename,imputed)
#test time series matches most recent year
test <- identical(x,y)
expect_that(test, equals(TRUE))

# #minor correction to ensure that formal alliances are not represented in fractions/decimals but rather whole numbers - round.
sfr.time.series <- sfr.time.series %>% mutate(value = ifelse(variablename == "Formal alliances",round(value),value))

#data direction check
df.check <- sfr.time.series %>% dplyr::group_by(variablename, dimension) %>% 
  dplyr::summarise(worst = iso3c[which(value == min(value))[1]], 
                   worst.year = year[which(value == min(value))[1]], 
                   worst.value = min(value), 
                   best = iso3c[which(value == max(value))[1]], 
                   best.year = year[which(value == max(value))[1]], 
                   best.value = max(value)) 

pos <- sfr.time.series$type == "Coping"
sfr.time.series$variablename[pos] <- paste(sfr.time.series$variablename[pos], " (C)", sep = "")
sfr.time.series$variablename[!pos] <- paste(sfr.time.series$variablename[!pos], " (R)", sep = "")

df.check <- sfr.time.series %>% dplyr::group_by(variablename) %>% 
  dplyr::summarise(worst = iso3c[which(value == min(value))[1]], 
                   worst.year = year[which(value == min(value))[1]], 
                   worst.value = min(value), 
                   best = iso3c[which(value == max(value))[1]], 
                   best.year = year[which(value == max(value))[1]], 
                   best.value = max(value)) 
fwrite(df.check,paste0(output_folders,"Data Check - Directionality.csv"))
rmExcept(c("raw.data", "sfr.time.series"))