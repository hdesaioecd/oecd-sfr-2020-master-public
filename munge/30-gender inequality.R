# # Example preprocessing script.
source("./lib/funcs.R")
filename <- "./data/2017 sfr model data/Gender Inequality - 2019 update.csv" #imports 2019 update to gender_ineq index, with 2018 being latest year
gender_ineq_raw <- fread(filename,skip = 1,header = T) %>% clean_names() 
gender_ineq_clean <- setDT(gender_ineq_raw)[,
                                        .(country,x2010,x2011,x2012,x2013,x2014,
                                          x2015,x2016,x2017,x2018)]
gender_ineq_melted <- melt(gender_ineq_clean,id.vars = c("country"),
                         variable.name = "year")
gender_ineq_melted$year <- gsub("x","",gender_ineq_melted$year) #remove x from the column description
gender_ineq_final <- gender_ineq_melted[,.(iso3c = country,year,value = as.numeric(value),variablename = "Gender Inequality Index")]
#apply country names
gender_ineq_final$iso3c <- country.code.name(gender_ineq_final$iso3c)
gender_ineq_final$iso3c <- country.code.name(gender_ineq_final$iso3c)
gender_ineq_final <- na.omit(gender_ineq_final)
raw.data$gender_ineq <- gender_ineq_final
rmExcept("raw.data") 
