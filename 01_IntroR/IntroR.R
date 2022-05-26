# Introduction to R
# Data Justice Academy
# Summer 2022

# Clay Ford, UVA Library


# Welcome to R! -----------------------------------------------------------

# This R script is intended to get you started doing data analysis with R. It
# contains instructions and tips for using R as well as examples of data
# analysis.

# Text with a "#" in front of it are called comments.


# Set your working directory ----------------------------------------------

# The working directory is the default place a program looks for files or saves
# files.

# The first thing we often do in an R script is set our working directory. You 
# usually set your working directory to where your data files are located. In
# this script, we want to set our working directory to where you downloaded the
# workshop files.

# To set working directory via point-and-click:

# (1) Session...Set Working Directory...Choose Directory. In the dialog,
# highlight the directory and click Open.

# (2) Use the Files tab. Navigate to folder and select "Set As Working
# Directory" under More


# To set working directory with R code:
# use setwd() function; path must be in quotes

# In RStudio, you can use the TAB key in the quotes to auto-complete the path.
# Try it! 

# Start with "/" to start from your root directory.
# Start with "~/" to start from your home directory.


# CODE ALONG #1 -----------------------------------------------------------

# Set your working directory to the folder on the computer that contains the
# workshop files.

setwd("")


# Loading data ------------------------------------------------------------

# To analyze data in R, we first need to load the data. We often do that by
# importing data.

# You can import just about any kind of data into R: Excel, Stata, SPSS, SAS, 
# CSV, JSON, fixed-width, TXT, DAT, shape files, and on and on. You can even 
# connect to databases. The best way to figure out how: Google "how to import 
# <type> files into R." This will usually involve installing and loading a
# special R package, which we'll talk about later.

# Today we'll import a CSV file.

# Data: Albemarle County real estate data
# Downloaded from Albemarle County web site
# https://www.albemarle.org/government/community-development/gis-mapping/gis-data

# File name: albemarle_homes.csv

# Import a CSV file and create a "data.frame"; here are three ways to do it:

# (1) read file from working directory on computer. If we've already set our 
# working directory to the location where the CSV file is located, we just need
# to put the name of the file in quotes.
homes <- read.csv("albemarle_homes_2022.csv")

# (2) read from a web site. If the file is on the web, simply copy-and-paste the
# URL and enclose in quotes:
homes <- read.csv("https://github.com/uvastatlab/DJA/raw/main/data/albemarle_homes_2022.csv")

# (3) use the RStudio "Import Dataset" button in the Environment window. Click
# on "Import Dataset", select "From Text (base)...", and navigate to file.



# Inspecting Data ---------------------------------------------------------

# Click on "homes" in the environment window to browse the data. You can only
# browse the data. You cannot edit.

# view structure of data
str(homes)

# Can also click the blue arrow next to the name in the Environment window. 

# View the first few and last few rows
head(homes)
tail(homes)

# quick summary of all columns; not useful for character data or when you have
# many columns.
summary(homes)

# Accessing columns of data -----------------------------------------------

# We can access the column of a data frame by using `$`. Example:
homes$YearBuilt

# Tip: use Ctrl + L to clear console

# Enter homes$ below and see how RStudio automatically prompts you with the
# column names.


# We sometimes want to work with columns of the data frame

# basic summary stats for numeric columns
summary(homes$TotalValue)

# first 6 values
head(homes$TotalValue)

# basic histogram to visualize the distribution of TotalValue
hist(homes$TotalValue)

# simple scatterplot of TotalValue versus FinSqFt
plot(x = homes$FinSqFt, y = homes$TotalValue)

# Counts of homes by high school district
table(homes$HSDistrict)

# CODE ALONG #2 -----------------------------------------------------------

# (1) Use the summary() function on the FullBath column of the homes data frame.
# Does the mean make sense?



# (2) Use the table() function on the FullBath column of the homes data frame.
# How many homes have 0 full baths? What's the most common number of full baths?



# (3) How many homes are in "Very Poor" condition?



# Subsetting data ---------------------------------------------------------

# We often want to extract a subset of our data that meets a certain condition.

# - All homes in Average or better condition
# - All homes with more than 3000 FinSqFt
# - All homes built after 2000 in the Albemarle HS District

# We can subset our data using the subset() function. 
# The basic syntax is subset(data, condition). 

# Comparison operators:
#   <      less than
#   >      greater than
#   <=     less than or equal to
#   >=     greater than or equal to
#   ==     equal
#   !=     not equal

# Show homes with TotalValue greater than 10,000,000
# 1e7 = 10,000,000 (ie, 1 with 7 zeroes after it)
subset(homes, TotalValue > 1e7)

# Show all homes with a Condition of Very Poor (notice we use two equal signs)
subset(homes, Condition == "Very Poor")

# Sometimes it can help to view the results using View()
View(subset(homes, Condition == "Very Poor"))


# Show homes in "Very Poor" condition built before 1900
# (use & for AND, use | for OR)
View(subset(homes, Condition == "Very Poor" & YearBuilt < 1800))


# We may want to save our subsetted data into a new data frame.

# Example: all homes with condition Average, Good or Excellent;
# Define multiple equalities using %in% operator
homes2 <- subset(homes, Condition %in% c("Average", "Good", "Excellent"))

# Tip: make sure the values are spelled correctly!

# CODE ALONG #3 -----------------------------------------------------------

# Create a new data frame consisting of homes built before 1900 and call it
# "homes3"



# Some basic data manipulation --------------------------------------------

# You'll almost always need to do some type of data manipulation before
# proceeding to analysis or plotting. Here are 5 very common tasks.

# (1) Deriving new columns (or variables) based on calculations

# Notice below we just add whatever column name we want to create after the
# dollar sign and perform the calculation.

# Divide TotalValue by FinSqFt to get PricePerSqFt
homes$PricePerSqFt <- homes$TotalValue/homes$FinSqFt

# Add a new column for log-transformed TotalValue
homes$logTotalValue <- log(homes$TotalValue)


# (2) creating an indicator variable with ifelse()

# An indicator variable takes two values, such as 0 and 1. "Was home built in
# 2008 or later? 1 if yes, 0 otherwise." We can use the ifelse() function for
# this.

# create an indicator for homes built in 2008 or later. If YearBuilt is greater
# than or equal to 2008, set to 1, otherwise 0. 
# syntax: ifelse(condition, value if TRUE, value if FALSE)
homes$After2008 <- ifelse(homes$YearBuilt >= 2008, 1, 0)


# (3) Creating Factors

# In R, a factor is a categorical variable with unique levels. We often want to
# take a character variable and convert it to factor. This is useful if we plan
# to use a categorical variable in a statistical analysis such as regression. We
# can use the factor() function for this. 

# Convert Condition column to factor
homes$Condition <- factor(homes$Condition)


# (4) Setting the order of levels in a factor

# After creating a factor, we may want the levels in a certain order.

# Notice the order of Condition: alphabetical
levels(homes$Condition)
table(homes$Condition)


# Let's set the order to Substandard, Poor, Fair, Average, Good, Excellent;
# We can do that with the factor function and the levels argument
homes$Condition <- factor(homes$Condition, 
                          levels = c("Not Rated", "Very Poor", "Poor", "Fair", 
                                     "Average", "Good", "Excellent"))

# Now the results are in the desired order
table(homes$Condition)
barplot(table(homes$Condition))


# (5) removing columns (variables)

# Assign NULL to the column:
homes$logTotalValue <- NULL
homes$After2008 <- NULL


# CODE ALONG #4 -----------------------------------------------------------


# Let's add an indicator called Pre20thCentury that has 1 if a home was built
# before 1900 and a 0 otherwise




# Basic summary stats -----------------------------------------------------


# to calculate frequencies of a factor or character variable use the table
# function:
table(homes$Condition) 


# Of course we already saw that table works on numeric variables as well. Best
# to use with integers.
table(homes$HalfBath)

# find mean of numeric columns
mean(homes$TotalValue)
mean(homes$TotalRooms) # NA?

# By default R returns NA (not available) for certain calculations if there is
# any missing data. Use na.rm=TRUE to override:
mean(homes$TotalRooms, na.rm = TRUE)

# How many missing?
summary(homes$TotalRooms)

# Which homes have missing total rooms? Use is.na() as condition to subset
subset(homes, is.na(TotalRooms))

# A few other summary functions
median(homes$TotalValue)
sd(homes$TotalValue) # standard deviation
range(homes$TotalValue) # returns min and max values; see also min() and max()

# Counting number of conditions satisfied

# How many homes have a TotalValue over 1 million?

# The following generates a vector of TRUE/FALSE values:
homes$TotalValue > 1e6

# In R, TRUE = 1 and FALSE = 0, so we can do math with TRUE/FALSE values. How
# many TRUES?
sum(homes$TotalValue > 1e6)


# Proportion of homes with TotalValue over 1 million:
# Taking the mean of 0,1 data returns percent of 1's
mean(homes$TotalValue > 1e6)

# How many homes with a TotalRooms greater than 10?
sum(homes$TotalRooms > 10)

# We have NAs in the TotalRooms column, so we need to tell R to ignore them:
sum(homes$TotalRooms > 10, na.rm = TRUE)
mean(homes$TotalRooms > 10, na.rm = TRUE)



# CODE ALONG #5 -----------------------------------------------------------

# What proportion of Albemarle county homes have more than 4 bedrooms?




# Summarizing data by group -----------------------------------------------

# Descriptive Statistics such as contingency tables and summary stats by group

# for contingency tables (or cross tabs) use the table function
# syntax: table(row variable, column variable)

# cross tab of Remodeled and HS Disrict
table(homes$Remodeled, homes$HSDistrict)

# calculate percents with the proportions() function. 

# First we save the table object as tab1
tab1 <- table(homes$Remodeled, homes$HSDistrict)

# print the table
tab1

# proportions by Remodeled; rows proportions sum to 1
proportions(tab1, margin = 1) 
# proportions by HSDistrict; columns proportions sum to 1
proportions(tab1, margin = 2) 

# use the round() function to round the number of decimals to a certain number
# of digits
round(proportions(tab1, margin = 2), 2)


# For basic numeric summary stats we can use the aggregate() function.
# syntax: aggregate(numeric ~ group, data, stat function)
# Read "~" as "grouped by"
# NOTE: aggregate ignores missing data by default

# median home value by hsdistrict
aggregate(TotalValue ~ HSDistrict, data = homes, median)

# mean home value by HSDistrict
aggregate(TotalValue ~ HSDistrict, data = homes, mean)

# median TotalValue by Remodeled and HSDistrict
aggregate(TotalValue ~ Remodeled + HSDistrict, homes, median)



# CODE ALONG #6 -----------------------------------------------------------

# Find the mean FinSqFt by HSDistrict




# Simple Graphics ---------------------------------------------------------

# The plot function creates a scatterplot if given two numeric vectors

# scatter plot of TotalValue versus FinSqFt
plot(x = homes$FinSqFt, y = homes$TotalValue)

# same with formula interface: y ~ x
plot(TotalValue ~ FinSqFt, data = homes)

# plot log transformed data
plot(log(TotalValue) ~ log(FinSqFt), data = homes)

# Customize the labels
plot(log(TotalValue) ~ log(FinSqFt), data = homes, 
     main = "Finished Sq Ft vs Total Value",
     ylab = "Log Total Value",
     xlab = "Log Finished Sq Ft")

# use the Zoom button to see bigger image
# Use the Export button to save image as JPG, TIF, PDF, etc.

# histograms
# Histogram of market value
hist(homes$TotalValue) # skew
hist(log(homes$TotalValue)) # more symmetric


# boxplots - visualize the distribution of a numeric value by a categorical
# variable; visualize the five number summary (Min, 25%, Median, 75%, Max)

# boxplot(numeric ~ group)
boxplot(FinSqFt ~ Condition, data = homes, 
        main = "Finished Sq Ft by Condition")



# Packages ----------------------------------------------------------------

# Packages are collections of functions and/or data created by other R users. 
# You will certainly want to install some packages! 

# What packages are available?:
# https://cran.r-project.org/web/packages/

# To see what packages you have installed, click the Packages tab in RStudio.

# To install a package in RStudio:

# 1. click the Install button on the Packages tab
# 2. type in the name of the package
# 3. click install.

# or use the install.packages() function

# Packages only need to be installed once, though they occasionally need to be 
# updated. Click the Update button on the Packages tab to see which packages 
# have available updates.

# Note: Packages often have dependencies. This means installing one package
# will also install other packages it depends on. 

# The ggplot2 package 

# a package that allows you to create plots using the Grammar of Graphics.

# Only install once. It will likely install several other packages it depends on.
install.packages("ggplot2")

# Load the package. Need to do once per R session if you want to use it.
library(ggplot2)


# ggplot2 examples - scatterplots
ggplot(homes, aes(x = FinSqFt, y = TotalValue)) + 
  geom_point()

# separate plot for each HSDistrict
ggplot(homes, aes(x = FinSqFt, y = TotalValue)) + 
  geom_point() +
  facet_wrap(~HSDistrict)

# add smooth trend line
ggplot(homes, aes(x = FinSqFt, y = TotalValue)) + 
  geom_point() +
  geom_smooth() +
  facet_wrap(~HSDistrict)

# We'll cover more ggplot2 in the data visualization workshop

# Basic statistics examples -----------------------------------------------

# The t-test: Comparing two means

# Does mean TotalValue differ between remodeled and unremodeled homes?
t.test(TotalValue ~ Remodeled, data = homes)

# ANOVA: Comparing more than two means

# Does mean TotalValue differ between HSDistrict?
aov.out <- aov(TotalValue ~ HSDistrict, 
               data = homes)
summary(aov.out)

# Where are the differences? Get pairwise differences with Tukey's Honestly Significant Differences.
TukeyHSD(aov.out)

# simple linear regression

# Can we summarize the relationship between TotalValue and FinSqFt value with a
# straight line? Regress TotalValue on FinSqFt, and save to "mod".
mod <- lm(TotalValue ~ FinSqFt, data = homes) 

# summary of the model
summary(mod) 

# Interpretation: Each additional square foot adds about $280 to the TotalValue
# (If this is a good model)

# Is this a "good" model? Calling plot on a model object and specifying "which =
# 1" produces a "residual vs fitted" plot.
plot(mod, which = 1)

# Ideally points will be evenly distributed around 0. That's not the case here.
# Our fitted straight line is vastly under-predicting many homes.

# One approach to address this is to log transform the variables.
# Notice the relationship seems more linear.
plot(log(TotalValue) ~ log(FinSqFt), data = homes)

# Fit a new regression model using log-transformed data
mod2 <- lm(log(TotalValue) ~ log(FinSqFt), data = homes) 

# summary of the model
summary(mod2) 

# Interpretation:  A 1% increase square footage increases TotalValue by about
# 1.1% (If this is a good model)

# check "residual vs fitted" plot. This looks better.
plot(mod2, which = 1)


# We'll learn more about linear modeling in the linear modeling workshop.


# Saving R objects --------------------------------------------------------

# Let's say we plan to do extensive work with the Albemarle homes data. It would
# be nice to not have to re-run the read.csv() function (plus any other data
# cleaning or analysis code) every time we want to work with the data in R. We
# can do that by saving R objects as .Rda (or .Rdata) files using the save()
# function. These are binary files that can be read into R using the load()
# function. The nice thing about .Rda files is that they can store multiple data
# objects.

# For example, here's how we can save the homes, tab1, mod, and mod2 objects in
# one .Rda file called "homes_work.Rda":
save(homes, tab1, mod, mod2, file = "homes_work.Rda")

# Now remove those objects from memory:
rm(homes, tab1, mod, mod2)

# Now load those objects into memory by simply loading "homes_work.Rda":
load("homes_work.Rda")

# We can also save single R objects using the saveRDS function. These files
# should have an .rds extension. To read these files back into R, use the
# readRDS() function. Unlike loading an Rda file, you need assign the result of
# readRDS to an object. Here's an example using the homes data frame.

# Save the homes data frame as "homes.rds" in your working directory
saveRDS(homes, file = "homes.rds")

# remove homes from the memory
rm(homes)

# read homes back into memory; you can name it whatever you want.
homes <- readRDS("homes.rds")


# Bonus material! ---------------------------------------------------------

# Stuff I'm not sure we'll have time for but you might like to review in your
# free time.


# Indexing brackets -------------------------------------------------------

# We can use indexing brackets to select portions of data frame:
# [row number(s),column number(s)/name(s)]

# show first 6 records of first 2 columns
# 1:6 = 1,2,3,4,5,6
homes[1:6,1:2] 

# first six rows; nothing after the comma means "show all columns"
homes[1:6,] 

# columns 2 and 3; nothing before the comma means "show all rows"
homes[,2:3] 

# can also use column names; they need to be entered as a "vector"
homes[1:6,c("TotalValue","CensusTract")]

# c("TotalValue","CensusTract") creates a "vector"; the c() function means "combine"

# First 10 cells of the TotalValue column
homes$TotalValue[1:10]

# Notice we didn't need a comma because the column only has one dimension.


# Recode a continuous variable into categories ----------------------------


# Here we recode LotSize into four categories: 0-1, 1-5, 5-10, 10+
# using the cut() function
# Inf = Infinity; here just means anything greater than 10
homes$LotSizeCat <- cut(homes$LotSize, breaks = c(-Inf,1,5,10,Inf))
summary(homes$LotSizeCat)

# We can add labels:
homes$LotSizeCat <- cut(homes$LotSize, breaks = c(-Inf,1,5,10,Inf), 
                       labels = c("1 acre or less","1 - 5 acres","5 - 10 acres","10+ acres"))
summary(homes$LotSizeCat)


# Function and programming example ----------------------------------------

# Use the function() function to write your own functions!

# The current Real Estate Tax Rate in Albemarle County is $.854 per hundred of
# assessed value. Let's write a function to calculate a home's real estate tax.

# Let's say a home is $385,000. What is the real estate tax owed?
385000/100 * 0.854

# Now let's write a function where we can plug in a homes value and get the tax
# owed. We'll call the function "reTax".

reTax <- function(value) value/100 * 0.854

# test the function:
reTax(value = 385000)
reTax(value = 455000)

# The function is "vectorized" and can be used to add a column to the homes data
# frame.

homes$RealEstateTax <- reTax(homes$TotalValue)

# We can also make the function more robust by checking the input is numeric.
# Notice we put multiple lines of code between curly braces. The is.numeric()
# returns TRUE if value is number. Putting a ! in front checks the opposite: is
# value NOT numeric. If TRUE, then the stop() function returns a custom error
# message.

reTax <- function(value){
  if(!is.numeric(value)) stop("value must be a number")
  value/100 * 0.854
} 

# test
reTax("387000")
reTax(387000)


# When you close R, the reTax function will be removed from memory.



