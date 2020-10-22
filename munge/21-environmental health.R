# # Example preprocessing script.
source("./lib/funcs.R")
filename <- "./data/2017 sfr model data/epi_updated2019.csv"
epi <- fread(filename,encoding = "UTF-8")
#HLT is the environmental health variable according to latest codebook
#use this in the calculation
epi <- setDT(epi)[,.(iso3c = iso,value = EPI.current, year = 2018, variablename = 
                "EPI")] #use the iso codes - there're encoding errors
#in the country column that aren't fixed by setting the encoding to utf-8. Must
#be issue with the file.
epi$iso3c <- country.code.name(epi$iso3c)
raw.data$envhealth <- epi
rmExcept("raw.data") 

