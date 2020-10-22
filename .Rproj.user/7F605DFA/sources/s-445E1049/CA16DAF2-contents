source("./lib/funcs.R")
filename_gender_labourforce <- "./data/2017 sfr model data/women_men_labourforce_ratio.csv"
filename_total_labourforce <- "./data/2017 sfr model data/total_labourforce_participation.csv"

filenames <- c(filename_gender_labourforce,filename_total_labourforce)

#write function for the cleaning of both files - they have the exact same structure, so just iterate over each filename.   
readdata <- function(x){
  df_temp <- fread(x,sep = ",",header = T)
  df_clean <- clean_names(df_temp)
  df_final <- df_clean %>% select(-c(country_name,indicator_code,v65)) %>% 
    pivot_longer(-c(country_code,indicator_name),names_to = "year",values_to = "value") %>% 
    dplyr::rename(iso3c = country_code)
  df_final$year <- gsub("x","",df_final$year)
  df_final$year <- as.numeric(df_final$year)
  df_final$iso3c <- country.code.name(df_final$iso3c)
  df_final <- na.omit(df_final)
}
#don't worry about the warnings - those are regional categories
labour_data <- lapply(filenames,readdata) %>% rbindlist()
labour_data$indicator_name <- as.factor(labour_data$indicator_name)
labour_data$indicator_name <- recode(labour_data$indicator_name,"Ratio of female to male labor force participation rate (%) (modeled ILO estimate)" = "female_labour")
labour_data$indicator_name <- recode(labour_data$indicator_name,"Labor force participation rate, total (% of total population ages 15+) (modeled ILO estimate)" = "labour_rate")
labour_data$indicator_name <- as.character(labour_data$indicator_name)
labour_data <- setDT(labour_data)[,.(iso3c,variablename = indicator_name,year,value)]
raw.data$labour_data <- labour_data
rmExcept("raw.data") 
