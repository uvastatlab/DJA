# Data Wrangling in R
# Data Justice Academy
# Summer 2022

# Clay Ford, UVA Library


# Packages ----------------------------------------------------------------

library(dplyr)
library(tibble)
library(lubridate)

# dplyr and lubridate have functions with the same names as functions loaded
# when R starts. The dplyr and lubridate functions "mask" the base R functions.
# That means when you use, say, lag(), you'll use the dplyr version of lag().

# If you wanted to use the base R lag(), you would use stats::lag(). Preceding a
# function with its package and two colons says to explicitly use the function
# in that package.

# Read in data ------------------------------------------------------------

# Read in the Albemarle homes rds file, which is the native storage format for
# an R object.

homes <- readRDS(url("https://github.com/uvastatlab/DJA/raw/main/data/albemarle_homes_2022.rds"))


# Intro to dplyr ----------------------------------------------------------

# dplyr - A Grammar of Data Manipulation

# dplyr provides functions (or "verbs" as it likes to call them) that correspond
# to the most common data manipulation tasks. The idea is to help you translate
# your thoughts into code. All of the dplyr functions take a data frame as the
# first argument.

# Functions that work on Rows:
# - filter() chooses rows based on column values.
# - slice() chooses rows based on location.
# - arrange() changes the order of the rows.

# Examples
filter(homes, Age > 300)
slice(homes, 1:4)
arrange(homes, YearBuilt)

# NOTE: none of these updated the homes data frame!

# Functions that work on Columns:
# - select() changes whether or not a column is included.
# - rename() changes the name of columns. (new name = old name)
# - mutate() changes the values of columns and creates new columns.

# Examples
select(homes, YearBuilt, Age, TotalValue)
rename(homes, year_built = YearBuilt, year_remodeled = YearRemodeled)
mutate(homes, post2000 = ifelse(YearBuilt > 1999, 1, 0))

# NOTE: none of these updated the homes data frame!

# A cheatsheet for dplyr is built into RStudio. Go to Help...Cheat Sheets...Data
# Transformation with dplyr. 

# The pipe operator -------------------------------------------------------

# dplyr provides the %>% (pipe) operator that allows you to "pipe" a result from
# one function into the next function. You can use the pipe to combine multiple
# functions that you can read left-to-right, top-to-bottom (reading the pipe
# operator as “then”).

# Use Ctrl + Shift + M (Win) or Cmd + Shift + M (Mac) to insert %>% 

# Example:
filter(homes, Age > 300)
# becomes....
homes %>% filter(Age > 300)

# "Take homes and then filter rows where Age > 300"

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

# NOTE: you will often see "pipeline" code such as this in online books, blogs,
# and Stack Overflow posts. The author may even say how "easy" dplyr makes
# working with data. But keep in mind the code took much longer to write than it
# takes for you to read it. Just because dplyr code is relatively easy to read
# does not mean it's always going to be fast to write.


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

# Notice how it prints to the console
homes

# Grouped data ------------------------------------------------------------

# dplyr can create grouped data frames, most often for creating summaries.

# - group_by() specifies how to group a data frame
# - summarize() calculates summaries by group

# Example: 

# take homes, 
# then group by Condition, 
# then find the median totalvalue of homes within each Condition

homes %>% 
  group_by(Condition) %>% 
  summarize(median_total_value = median(TotalValue))

# A base R approach
aggregate(TotalValue ~ Condition, data = homes, median)

# Example: 

# take homes, 
# then group by CensusTract, 
# then find the median totalvalue of homes within each CensusTract, 
# then arrange by median value

homes %>% 
  group_by(CensusTract) %>% 
  summarize(median_total_value = median(TotalValue)) %>% 
  arrange(median_total_value)

# Notice...
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

# Doing the same with Base R and arrange()
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

# We showed how to use the dplyr function n() to count cases. 

homes %>% 
  group_by(HSDistrict) %>% 
  summarize(n = n())

# There is also a count() function for this. Notice it returns a column called
# "n"; use "name" argument to create a different name. For example, name =
# "total"
homes %>% 
  count(HSDistrict)

# Notice the result is a data frame/tibble.

# base R table() returns a vector
table(homes$HSDistrict)

# We can count by multiple groups. Number of homes by HSDistrict and Condition.
homes %>% 
  group_by(HSDistrict, Condition) %>% 
  count() 

# Base R returns a matrix (2-way table). I find this easier to read for
# exploratory purposes.
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
# YearBuilt or CensusTract, and arrange in descending order

homes %>% 
  filter(!is.na(YearBuilt) & !is.na(CensusTract)) %>% 
  count(CensusTract) %>% 
  arrange(desc(n)) %>% 
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
xtabs(~ Condition, data = homes) %>% proportions() 

# Two groups is a little more complicated. This calculate proportions within
# each of the 14 combinations.
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
xtabs(~ HSDistrict + (LotSize > 10), data = homes)
xtabs(~ HSDistrict + (LotSize > 10), data = homes) %>% 
  proportions(margin = 2)
xtabs(~ HSDistrict + (LotSize > 10), data = homes) %>% 
  proportions(margin = 1)


# Recoding variables ------------------------------------------------------

# Look at FullBath
homes %>% 
  count(FullBath)

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
homes %>% 
  count(Condition)

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

# The base R cut() function is also good for this type of work. By default
# intervals are "(a,b]": greater than a, less than equal to b.

# Let's say we want to create a variable called "Century" to identify what
# century the house was built.

range(homes$YearBuilt, na.rm = TRUE)

homes <- homes %>% 
  mutate(Century = cut(YearBuilt, 
                       breaks = c(1600, 1700, 1800, 1900, 2000, 2100),
                       labels = c(17, 18, 19, 20, 21)))

homes %>% 
  count(Century)



# Code Along 5 ------------------------------------------------------------

# Create a new variable called PriceCategory with the following levels based on
# TotalValue:

# 200,000 or less
# 200,001 - 500,000
# 500,001 - 1,000,000
# over 1,000,000

homes <- homes %>% 
  mutate(PriceCategory = case_when(
    TotalValue <= 2e5 ~ "200,000 or less",
    between(TotalValue, 2e5 + 1, 500000) ~ "200,001 - 500,000",
    between(TotalValue, 5e5 + 1, 1e6) ~ "500,001 - 1,000,000",
    TotalValue > 1e6 ~ "Over 1,000,000",
  ))

homes %>% 
  count(PriceCategory)


homes <- homes %>% 
  mutate(PriceCategory = cut(TotalValue, 
                             breaks = c(-Inf, 200000, 500000, 1e6, Inf),
                             labels = c("200,000 or less",
                                        "200,001 - 500,000",
                                        "500,001 - 1,000,000",
                                        "over 1,000,000")))

homes %>% 
  count(PriceCategory)


# Formatting dates --------------------------------------------------------

# We have a column of dates called LastSaleDate.
head(homes$LastSaleDate)

# They are formatted as character type
str(homes$LastSaleDate)

# This means we can't sort them by year, or do things like calculate elapsed
# time between LastSaleDate.

# The lubridate package provides functions for working with dates.

# The basic collection of functions are permutations of the letters "m", "d",
# "y" that we use to specify the date format. Since the LastSaleDate is
# formatted as mm/dd/yyyy, we use the mdy() function.

homes <- homes %>% 
  mutate(LastSaleDate = mdy(LastSaleDate))

# Now it is formatted as Date.
str(homes$LastSaleDate)

# With it formatted as a Date we can do things like extract the month using the
# month() function.

homes <- homes %>% 
  mutate(SaleMonth = month(LastSaleDate, label = TRUE))

# Which months seem to have the most sales?
barplot(table(homes$SaleMonth))


# Code along 6 ------------------------------------------------------------

# Extract the day of the week of the Last Sale Date and save into column called
# SaleDay using the wday() function. Set the label argument to TRUE.

homes <- homes %>% 
  mutate(SaleDay = wday(LastSaleDate, label = TRUE))

barplot(table(homes$SaleDay))

# Merging/Joining data ----------------------------------------------------

# Let's create some fake data to demonstrate merging/joining data:
left <- data.frame(id=1:3,
                   x=c(12, 14, 16))
right <- data.frame(id=2:4,
                    y=c("a", "b", "c"))
left; right

#### LEFT JOIN

# If we want to retain everything in the left data frame and merge only what 
# has a matching id in the right data frame, we do a LEFT JOIN.
left; right
left_join(left, right, by = "id")

# Notice all rows from left are retained and NA is created in the y column where
# the right had no matching id. This is why it's called a "left join".

#### RIGHT JOIN

# If we want to retain everything in the right data frame and merge only what 
# has a matching id in the left data frame, we do a RIGHT JOIN.
left; right
right_join(left, right, by = "id")

# Notice all rows from right are retained and NA is created in the x column
# where the left had no matching id. This is why it's called a "right join".

#### INNER JOIN

# If we want to retain only those rows with matching ids in BOTH data sets, we
# do an INNER JOIN.
left; right
inner_join(left, right, by = "id")

# Notice only those records with matching ids are joined.

#### FULL JOIN

# If we wanted to merge ALL rows regardless of match, we do a FULL JOIN.
left; right
full_join(left, right, by = "id")

# Notice all rows from both data frames are retained and NAs are created in 
# columns where rows did not have matching ids in the other data set. 

#### SEMI JOIN

# This is called a "filtering" join. We don't actually merge or join the data
# sets. Instead we filter one data set based on whether it has any matches in
# another data set.

# Which rows in left have a match in right?
left; right
semi_join(left, right, by = "id")

#### ANTI JOIN

# With Anti Join, we filter one data set based on whether it does NOT have any
# matches in another data set.

# Which rows in left do NOT have a match in right?
left; right
anti_join(left, right, by = "id")



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

# Done! -------------------------------------------------------------------

# That's enough data wrangling for one day! 

# Data wrangling is often a frustrating, time-consuming process. Every data set
# is different. It's normal to struggle a bit, make mistakes, Google for help,
# patch together code snippets, etc. Who merges data every day? Who recodes
# variables every day? Be kind to yourself and others. We're all doing the best
# we can.




# Appendix: select() helpers and across() ---------------------------------

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


# across() allows you to apply the same transformation to multiple columns.

# The select() helpers can be used inside the across() function for selecting
# which columns to transform.

# Example: change all prices (in the columns LandValue:LastSalePrice) to be per
# 1000 dollars (ie, price/1000) The code `function(x)x/1000` is an anonymous
# function that we create on-the-fly.

homes %>% 
  mutate(across(LandValue:LastSalePrice, function(x)x/1000)) %>% 
  select(LandValue:LastSalePrice)

# dplyr also provides shorthand syntax for defining functions using ~ and .x

homes %>% 
  mutate(across(LandValue:LastSalePrice, ~ .x/1000)) %>% 
  select(LandValue:LastSalePrice)


# Appendix: reshaping data ------------------------------------------------


# It's often helpful to think of data as "wide" or "long". 

# Example of a wide data frame. Notice each person has multiple test scores
# that span columns.
wide <- data.frame(name=c("Clay","Garrett","Addison"), 
                   test1=c(78, 93, 90), 
                   test2=c(87, 91, 97),
                   test3=c(88, 99, 91))
wide

# Example of a long data frame. This is the same data as above, but in long
# format. We have one row per person per test.
long <- data.frame(name=rep(c("Clay","Garrett","Addison"),each=3),
                   test=rep(1:3, 3),
                   score=c(78, 87, 88, 93, 91, 99, 90, 97, 91))
long

# The long format is actually preferable for many scenarios in R. This is
# sometimes referred to as "tidy data". In tidy data, each variable is a column
# and each observation is a row. Here we have 3 variables: name, test, and
# score. Each row represents a single observation on a student.

# With data in this format we can easily summarize and plot the data. For example:

# mean score per student
aggregate(score ~ name, data = long, mean)
# mean score per test
aggregate(score ~ test, data = long, mean)

# line plot of scores over test, grouped by name
ggplot(long, aes(x = factor(test), y = score, 
                 color = name, group = name)) +
  geom_point() +
  geom_line() +
  xlab("Test")

#### reshape wide to long

# The tidyr package provides functions for reshaping data. 
library(tidyr)

# To reshape wide data into long format we use the pivot_longer() function.
wide
pivot_longer(wide, 
             cols = test1:test3, 
             names_to = "test", values_to = "score")

# The first argument is the dataset to reshape

# The second argument describes which columns need to be reshaped.

# The names_to argument gives the name of the variable that will be created from
# the data stored in the column names, i.e. test

# The values_to argument gives the name of the variable that will be created
# from the data stored in the cell value, i.e. score


#### reshape long to wide 

# This is less common. For this we use the tidyr function pivot_wider().
long
pivot_wider(long, 
            id_cols = name, 
            names_from = test, 
            values_from = score,
            names_prefix = "test")

# The first argument is the dataset to reshape

# The second argument describes which columns need to be reshaped.

# The names_from argument gives the name of the variable that contains the data
# that will be used to create the column names.

# The values_from argument gives the name of the variable that contains the
# data that will be used to populate the cells.

# The names_prefix argument lets us prepend "test" to the column names.


# Example: the following data from the Guttmacher Institute, a “research and
# policy organization committed to advancing sexual and reproductive health and
# rights.” They recently released its latest estimates of annual pregnancies,
# births, and abortions among women in the US. (https://osf.io/kthnf/)

pba <- readRDS('https://github.com/uvastatlab/DJA/raw/main/data/NationalAndStatePregnancy_PublicUse.rds')

# This is a wide data set with 103 columns. 
ncol(pba)

# Each row contains all pregnancy, birth, and abortion data for one state in one
# year.

# Let's reshape the birthrate portion of the data to long format.
pbaL <- pba %>% 
  select(state, year, contains("birthrate")) %>% 
  pivot_longer(
    cols = !c(state, year),    # reshape all columns except state and year
    names_to = "age_group",    # put column names into column named "age_group"
    names_prefix = "birthrate", # remove the "birthrate" prefix
    values_to = "rate")        # the column name for data stored in cells

# See all the different age groups. 
unique(pbaL$age_group)

# 1517 means "15-17", etc
# lt15 means "less than 15"

# Why do this? Makes it easier for visualizing trends over time.

# Example: compare birth rate trends in groups "1517", "1819", and "2024" in all
# 50 states.

ggplot(
  filter(pbaL, 
         age_group %in% c("1517", "1819", "2024"))) + 
  aes(x = year, y = rate, color = age_group) +
  geom_line() +
  facet_wrap(~state)

# Birth rates among these age groups are falling in all 50 states.