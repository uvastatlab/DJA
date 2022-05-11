# Download median household income by census tract in Albemarle County

library(tidycensus)
library(dplyr)
library(stringr)

# vars <- load_variables(2020, "acs5", cache = TRUE)
# median household income
mhi <- get_acs(geography = "tract", variables = "B19013_001",
               state = "VA", county = "Albemarle", year = 2019)
mhi <- mhi %>% 
  mutate(CensusTract = str_extract(NAME, pattern = "[0-9]{3}\\.?[0-9]{0,2}"))
mhi$NAME <- NULL

saveRDS(mhi, file = "data/median_household_income.rds")
