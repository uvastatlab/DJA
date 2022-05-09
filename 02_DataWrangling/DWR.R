
homes <- read.csv("https://github.com/uvastatlab/DJA/raw/main/data/albemarle_homes_2022.csv")


# Intro to dplyr ----------------------------------------------------------

# dplyr - A Grammar of Data Manipulation
library(dplyr)

# provides simple "verbs", functions that correspond to the most common data
# manipulation tasks, to help you translate your thoughts into code.

# Rows:
# - filter() chooses rows based on column values.
# - slice() chooses rows based on location.
# - arrange() changes the order of the rows.

# Columns:
# - select() changes whether or not a column is included.
# - rename() changes the name of columns.
# - mutate() changes the values of columns and creates new columns.
# - relocate() changes the order of the columns.

# Groups of rows:
# - summarize() collapses a group into a single row.

# All of the dplyr functions take a data frame (or tibble) as the first
# argument. dplyr provides the %>% (pipe) operator that allows you to "pipe" a
# result from one step into the next step. You can use the pipe to rewrite
# multiple operations that you can read left-to-right, top-to-bottom (reading
# the pipe operator as “then”).

# Example: find homes smaller than 300 FinSqFt, show their Total Value and
# FinSqFt, calculate price per square feet, and then sort in descending order
# with most expensive homes first

homes %>% 
  filter(FinSqFt < 300) %>% 
  select(FinSqFt, TotalValue) %>% 
  mutate(PerSqFt = round(TotalValue/FinSqFt, 2)) %>% 
  arrange(desc(TotalValue))

# Use Ctrl + Shift + M (Win) or Cmd + Shift + M (Mac) to insert %>% 

# If we wanted to save the result, we need to assign it to an object:
tiny_houses <- homes %>% 
  filter(FinSqFt < 300) %>% 
  select(FinSqFt, TotalValue) %>% 
  mutate(PerSqFt = round(TotalValue/FinSqFt, 2)) %>% 
  arrange(desc(TotalValue))


# Tibbles -----------------------------------------------------------------

# Tibbles are data frames with different behaviors. They are frequently used
# with dplyr. The tibble package provides functions for converting a data frame
# into a tibble.

library(tibble)
tiny_houses_tbl <- as_tibble(tiny_houses)

# Tibbles versus data frames - a few differences

# (1) They print differently. For large data frames, it only prints the first
# few rows.
tiny_houses_tbl
tiny_houses

# (2) Tibbles are always Tibbles, not vectors
tiny_houses_tbl[,1]
tiny_houses[,1] # vector

# (3) Tibbles do not support partial matching
tiny_houses_tbl$Fin
tiny_houses$Fin

# tibble and dplyr belong to a collection of packages known as the Tidyverse.

# Let's convert our homes data frame into a tibble.
homes_tbl <- as_tibble(homes)
homes_tbl



# Grouped data ------------------------------------------------------------

# dplyr verbs can be used with grouped data frames.

# - group_by() specifies how to group a data frame

# Example: find the median totalvalue of homes within each census tract and then
# sort by median total value.
homes %>% 
  group_by(CensusTract) %>% 
  summarize(median_total_value = median(TotalValue)) %>% 
  arrange(median_total_value)

# Notice: 

# - the print method is dropping the decimal values of the census tracts
# - not showing all the rows. 
# - treating missing census tracts as their own group

# One way to fix is to mutate CensusTract as a character type, filter out rows
# missing CensusTract, and then use print() with n = Inf to print all rows.
homes %>% 
  mutate(CensusTract = as.character(CensusTract)) %>% 
  filter(!is.na(CensusTract)) %>% 
  group_by(CensusTract) %>% 
  summarize(median_total_value = median(TotalValue)) %>% 
  arrange(median_total_value) %>% 
  print(n = Inf)

# Doing the same in Base R
aggregate(TotalValue ~ CensusTract, data = homes, median) %>% 
  arrange(TotalValue)

