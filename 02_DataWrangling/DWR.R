# Data Wrangling in R
# Data Justice Academy
# Summer 2022

# Clay Ford, UVA Library



# Packages ----------------------------------------------------------------

library(dplyr)
library(tibble)
library(lubridate)


# Read in data ------------------------------------------------------------

# Read in the rds file, which is the native storage format for an R object.

homes <- readRDS(url("https://github.com/uvastatlab/DJA/raw/main/data/albemarle_homes_2022.rds"))


# Intro to dplyr ----------------------------------------------------------

# dplyr - A Grammar of Data Manipulation

# dplyr has 6 functions with the same names as functions loaded when R starts.
# The dplyr functions "mask" the base R functions. That means when you call
# filter(), you'll use the dplyr version of filter().

# dplyr provides simple "verbs", functions that correspond to the most common
# data manipulation tasks, to help you translate your thoughts into code. All of
# the dplyr functions take a data frame (or tibble) as the first argument.

# Rows:
# - filter() chooses rows based on column values.
# - slice() chooses rows based on location.
# - arrange() changes the order of the rows.

# Examples
filter(homes, Age > 300)
slice(homes, 1:4)
arrange(homes, YearBuilt)

# NOTE: none of these updated the homes data frame!

# Columns:
# - select() changes whether or not a column is included.
# - rename() changes the name of columns. (new name = old name)
# - mutate() changes the values of columns and creates new columns.

# Examples
select(homes, YearBuilt, Age, TotalValue)
rename(homes, year_built = YearBuilt, year_remodeled = YearRemodeled)
mutate(homes, post2000 = ifelse(YearBuilt > 1999, 1, 0))

# NOTE: none of these updated the homes data frame!

# dplyr provides the %>% (pipe) operator that allows you to "pipe" a result from
# one step into the next step. You can use the pipe to rewrite multiple
# operations that you can read left-to-right, top-to-bottom (reading the pipe
# operator as “then”).

# Use Ctrl + Shift + M (Win) or Cmd + Shift + M (Mac) to insert %>% 

# Example:
filter(homes, Age > 300)
# becomes....
homes %>% filter(Age > 300)

# Example: find homes smaller than 300 FinSqFt, show their Total Value and
# FinSqFt, calculate price per square feet, and then sort in descending order
# with most expensive homes first

homes %>% 
  filter(FinSqFt < 300) %>% 
  select(FinSqFt, TotalValue) %>% 
  mutate(PerSqFt = round(TotalValue/FinSqFt, 2)) %>% 
  arrange(desc(TotalValue))  # desc = in descending order

# If we wanted to save the result, we need to assign it to an object:
tiny_houses <- homes %>% 
  filter(FinSqFt < 300) %>% 
  select(FinSqFt, TotalValue) %>% 
  mutate(PerSqFt = round(TotalValue/FinSqFt, 2)) %>% 
  arrange(desc(TotalValue))


# Tibbles -----------------------------------------------------------------

# Tibbles are data frames with different behaviors. They are frequently used
# with dplyr. The tibble package provides the as_tibble() function for
# converting a data frame into a tibble.

tiny_houses_tbl <- as_tibble(tiny_houses)

# Tibbles versus data frames - two key differences (not all)

# (1) They print differently. tibbles include metadata. For large data frames,
# it only prints the first few rows.
tiny_houses_tbl
tiny_houses

# (2) Tibbles are always Tibbles, not vectors
tiny_houses_tbl[,1]
tiny_houses[,1] # vector


# tibble and dplyr belong to a collection of packages known as the Tidyverse.

# Let's convert our homes data frame to a tibble.
homes <- as_tibble(homes)
homes

# Grouped data ------------------------------------------------------------

# dplyr can create grouped data frames, most often for creating summaries.

# - group_by() specifies how to group a data frame
# - summarize() collapses a group into a single row.

# Example: take homes, then group by Condition, then find the median totalvalue
# of homes within each Condition

homes %>% 
  group_by(Condition) %>% 
  summarize(median_total_value = median(TotalValue))

# A base R approach
aggregate(TotalValue ~ Condition, data = homes, median)

# Example: take homes, then group by CensusTract, then find the median
# totalvalue of homes within each CensusTract, then arrange by median value

homes %>% 
  group_by(CensusTract) %>% 
  summarize(median_total_value = median(TotalValue)) %>% 
  arrange(median_total_value)

# - not showing all the rows. 
# - treating missing census tracts as their own group

# One way to fix is to filter out rows missing CensusTract, and then use print()
# with n = Inf to print all rows.
homes %>% 
  filter(!is.na(CensusTract)) %>% 
  group_by(CensusTract) %>% 
  summarize(median_total_value = median(TotalValue)) %>% 
  arrange(median_total_value) %>% 
  print(n = Inf)

# Doing the same in Base R
aggregate(TotalValue ~ CensusTract, data = homes, median) %>% 
  arrange(TotalValue)


# Code along 1 ------------------------------------------------------------

# Create a new data frame called "pre1900"...

# - that contains homes built before 1900
# - that have never been remodeled
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



# Counting cases ----------------------------------------------------------

# We showed how to use the dplyr function n() to count cases. There is also a
# count() function for this.

homes %>% 
  group_by(HSDistrict) %>% 
  summarize(n = n())

# using count(); notice it returns a column called "n"; use "name" argument to
# create a different name. For example, name = "total"
homes %>% 
  count(HSDistrict)

# Notice the result is a data frame/tibble.

# base R table() returns a vector
table(homes$HSDistrict)

# We can count by multiple groups. Number of homes by HSDistrict and Condition.

homes %>% 
  group_by(HSDistrict, Condition) %>% 
  count() 

# Base R returns a matrix (2-way table)
table(homes$HSDistrict, homes$Condition)

# How about three groups? Number of homes by HSDistrict, Condition and Cooling.

homes %>% 
  group_by(HSDistrict, Condition, Cooling) %>% 
  count()

# Base R returns an array (3D table) but does not include NA!
table(homes$HSDistrict, homes$Condition, homes$Cooling)

# To return NA we include usena = "ifany"
table(homes$HSDistrict, homes$Condition, homes$Cooling, useNA = "ifany")

# xtabs() uses formula notation like aggregate(), but uses addNA = TRUE
xtabs(~ HSDistrict, data = homes)
xtabs(~ HSDistrict + Condition, data = homes)
xtabs(~ HSDistrict + Condition + Cooling, data = homes, addNA = TRUE)


# Code along 3 ------------------------------------------------------------

# Count up the number of homes in each CensusTract for those records not missing
# YearBuilt or CensusTract.

homes %>% 
  filter(!is.na(YearBuilt) & !is.na(CensusTract)) %>% 
  count(CensusTract) %>% 
  print(n = Inf)


# Calculating proportions -------------------------------------------------

# A proportion is number of "successes" out of total number of "trials".

# Number of homes in each Condition
x <- table(homes$Condition)

# Proportion of Albemarle county homes in each Condition
x/sum(x)

# Doing this in dplyr requires using mutate() and the formula above
homes %>% 
  count(Condition) %>% 
  mutate(p = n/sum(n))

# Base R with pipes
table(homes$Condition) %>% proportions() 

# Two groups is a little more complicated. This calculate proportions within 1
# of the 14 levels.
homes %>% 
  count(Condition, Remodeled) %>% 
  mutate(p = n/sum(n))      # p totals to 1 within data frame

# To calculate proportion with each level of Condition, we need to use group_by()
homes %>% 
  count(Condition, Remodeled) %>% 
  group_by(Condition) %>%   
  mutate(p = n/sum(n))      # p totals to 1 within Condition

# In base R this is referred to as the margin
xtabs(~ Condition + Remodeled, data = homes) %>% 
  proportions(margin = 1)   # p totals to 1 within the rows (margin = 1)


# Code along 4 ------------------------------------------------------------

# Of all homes on over 10 acres of land (LotSize), what proportion are in each HSDistrict?

homes %>% 
  filter(LotSize > 10) %>% 
  count(HSDistrict) %>% 
  mutate(p = n/sum(n))

# What is the proportion of homes over 10 acres of land (LotSize) within each school district?

homes %>% 
  group_by(HSDistrict) %>% 
  summarise(n = n(), 
            over10 = sum(LotSize > 10),
            p = over10/n)



# Recoding variables ------------------------------------------------------

# Look at HalfBath
table(homes$FullBath)

# What if wanted to create a new column called FullBath2 with levels :

# 0
# 1-2
# 3-5
# 6+

# The dplyr function case_when() can help do this. It is often used with
# mutate(). LHS (left-hand side) is a logical test. If TRUE, the RHS (right-hand
# side) value is assigned.

homes %>% 
  mutate(FullBath2 = case_when(
    FullBath < 1 ~ "0",
    FullBath %in% 1:2 ~ "1-2",
    FullBath %in% 3:5 ~ "3-5",
    FullBath >= 6 ~ "6+"
    )) %>% 
  select(FullBath, FullBath2) %>% 
  arrange(desc(FullBath)) 


# Look at Condition
table(homes$Condition)

# What if I wanted 3 levels:

# Below Average (Very Poor, Poor,Fair)
# Average (Average)
# Above Average (Good, Excellent)

# Remember it's a factor
str(homes$Condition)

# We have to first convert to character type.
homes %>% 
  mutate(Condition = as.character(Condition)) %>% 
  mutate(Condition2 = case_when(
    Condition %in% c("Very Poor", "Poor", "Fair") ~ "Below Average",
    Condition %in% c("Good", "Excellent") ~ "Above Average",
    TRUE ~ Condition
  )) %>% 
  select(Condition, Condition2) 

# NOTE: I didn't assign the result to an object so these changes were not saved.

# The base R cut() function is also good for this type of work. By default intervals are "(a,b]", that is greater than or equal to a, less than b.

# Let's say we want to create a variable called "Century" to identify what
# century the house was built.

range(homes$YearBuilt, na.rm = TRUE)

homes <- homes %>% 
  mutate(Century = cut(YearBuilt, 
                       breaks = c(1600, 1700, 1800, 1900, 2000, 2100),
                       labels = c(17, 18, 19, 20, 21)))

table(homes$Century)



# Code Along 6 ------------------------------------------------------------

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


# Formatting dates --------------------------------------------------------

# We have a column of dates called LastSaleDate.
head(homes$LastSaleDate)

# They are formatted as character type
str(homes$LastSaleDate)

# This means we can't sort them by year, or calculate elapsed time.

# The lubridate package provides functions for working with dates.

# The basic collection of functions are permutations of the letters "m", "d",
# "y" that we use to specify the date format. Since the LastSaleDate is formatted as mm/dd/yyyy, we use the mdy() function.

homes <- homes %>% 
  mutate(LastSaleDate = mdy(LastSaleDate))

# Now it is formatted as Date.
str(homes$LastSaleDate)

# With it formatted as a Date we can do things like extract the month using the month() function.

homes <- homes %>% 
  mutate(SaleMonth = month(LastSaleDate, label = TRUE))

# Which months seem to have the most sales?
barplot(table(homes$SaleMonth))


# Merging/Joining data ----------------------------------------------------




# Helper functions --------------------------------------------------------

# dplyr provides a number of "helper" functions for selecting many columns at
# once. These are useful with select(). Here are few commonly used helpers.

# contains(match)
# ends_with(match)
# starts_with(match)
# : (range of columns next to each other)
# ! (Not these variables)
# everything()  (everything else not already specified)
# where(function) (return columns where function returns TRUE)
 
homes %>% select(contains("Value"))
homes %>% select(ends_with("District"))
homes %>% select(starts_with("Last"))
homes %>% select(YearBuilt:Condition)
homes %>% select(!(YearBuilt:Condition))
homes %>% select(FP, Age, everything())  # good for reordering columns
homes %>% select(where(is.integer))





# Appendix: across() ------------------------------------------------------

# across() allows you to apply the same transformation to multiple columns.

# The select() helpers can be used inside the across() function for selecting
# which columns to transform.

# Example: change all prices (in the columns LandValue:LastSalePrice) to be per
# 1000 dollars (ie, price/1000) The code `function(x)x/1000` is anonymous
# function that we create on-the-fly.

homes %>% 
  mutate(across(LandValue:LastSalePrice, function(x)x/1000)) %>% 
  select(LandValue:LastSalePrice)

# dplyr also provides shorthand syntax for defining functions using ~ and .x

homes %>% 
  mutate(across(LandValue:LastSalePrice, ~ .x/1000)) %>% 
  select(LandValue:LastSalePrice)

