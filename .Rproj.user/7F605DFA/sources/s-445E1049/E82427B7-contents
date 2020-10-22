# Example preprocessing script.
source("./lib/load-libraries.R")
#this imports a dataframe with 44 unique rows:
sfr.log <- read_excel("./data/indicator master list/SFR2020 Indicator Master List.xlsx", "raw.data.for.R")
sfr.log <- sfr.log %>% fill(dimension)
raw.data <- list(log = sfr.log)
rmExcept("raw.data")
