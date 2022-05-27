
# Code along 1 ------------------------------------------------------------

# Create a new data frame called "pre1900"...

# - that contains homes built before 1900 AND have never been remodeled
# - that only has YearBuilt, TotalValue, and FinSqFt columns, 
# - and that is sorted in ascending order by YearBuilt

pre1900 <- homes %>% 
  filter(Remodeled == 0 & YearBuilt < 1900) %>% 
  select(YearBuilt, TotalValue, FinSqFt) %>% 
  arrange(YearBuilt)


# Code along 2 ------------------------------------------------------------

# Create a new data frame called "values_by_year"...

# - that contains median home values by YearBuilt in new column "MedianValue" 
# - that contains number of homes per YearBuilt in new column called "n". 
# - that is arranged by YearBuilt 
# - that only has YearBuilt, MedianValue, and n columns.

values_by_year <- homes %>% 
  group_by(YearBuilt) %>% 
  summarize(MedianValue = median(TotalValue), 
            n = n()) %>% 
  arrange(YearBuilt) %>% 
  select(YearBuilt, MedianValue, n)


# Code along 3 ------------------------------------------------------------

# Count up the number of homes in each CensusTract for those records not missing
# YearBuilt or CensusTract, and arrange in descending order

homes %>% 
  filter(!is.na(YearBuilt) & !is.na(CensusTract)) %>% 
  count(CensusTract) %>% 
  arrange(desc(n)) %>% 
  print(n = Inf)


# Code along 4 ------------------------------------------------------------

# (1) Of all homes on over 10 acres of land (LotSize), what proportion are in
# each HSDistrict?

homes %>% 
  filter(LotSize > 10) %>% 
  count(HSDistrict) %>% 
  mutate(p = n/sum(n))

# (2) What is the proportion of homes over 10 acres of land (LotSize) within
# each school district?

homes %>% 
  group_by(HSDistrict) %>% 
  summarise(n = n(), 
            over10 = sum(LotSize > 10),
            p = over10/n)

# Base R solution
xtabs(~ HSDistrict + (LotSize > 10), data = homes) %>% 
  proportions(margin = 2)
xtabs(~ HSDistrict + (LotSize > 10), data = homes) %>% 
  proportions(margin = 1)



# Code Along 5 ------------------------------------------------------------

# Create a new variable called PriceCategory with the following levels based on
# TotalValue:

# 200,000 or less
# 200,001 - 500,000
# 500,001 - 1,000,000
# over 1,000,000

homes <- homes %>% 
  mutate(PriceCategory = cut(TotalValue, 
                             breaks = c(-Inf, 200000, 500000, 1e6, Inf),
                             labels = c("200,000 or less",
                                        "200,001 - 500,000",
                                        "500,001 - 1,000,000",
                                        "over 1,000,000")))

homes %>% 
  count(PriceCategory)

# dplyr
homes <- homes %>%
  mutate(PriceCategory = case_when(
    TotalValue <= 2e5 ~ "200,000 or less",
    between(TotalValue, 2e5 + 1, 500000) ~ "200,001 - 500,000",
    between(TotalValue, 5e5 + 1, 1e6) ~ "500,001 - 1,000,000",
    TotalValue > 1e6 ~ "Over 1,000,000"
    ))

homes %>%
  count(PriceCategory)


# Code along 6 ------------------------------------------------------------

# Extract the day of the week of the Last Sale Date and save into column called
# SaleDay using the wday() function. Set the label argument to TRUE.

homes <- homes %>% 
  mutate(SaleDay = wday(LastSaleDate, label = TRUE))

barplot(table(homes$SaleDay))



# Code along 7 ------------------------------------------------------------

# The following code downloads data on median household income by census tract.
# It was obtained from the 2019 American Community Survey.

mhi <- readRDS(url("https://github.com/uvastatlab/DJA/raw/main/data/median_household_income.rds"))
mhi

# The following code calculates median home values by census tract.
med_home <- homes %>% 
  group_by(CensusTract) %>% 
  summarize(med_value = median(TotalValue))
med_home

# Merge the median home income data with the median home value data using a left
# join with med_home on the left.

d <- left_join(med_home, mhi, by = "CensusTract")
d

plot(estimate ~ med_value, data = d)
cor.test(~ estimate + med_value, data = d)
