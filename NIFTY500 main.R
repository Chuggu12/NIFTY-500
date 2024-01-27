# Install packages 
 install.packages("tidyverse")
 install.packages("lubridate")
 install.packages("dplyr")
 install.packages("magrittr")
 install.packages("ggplot2")

library(tidyverse) # for data tidying
library(lubridate) # for handling date fields
library(dplyr) # for tabular data manipulation
library(magrittr)
 library(date) #to extract date
library(ggplot2) # for visualization
 
# The tidyverse is a collection of R packages that are designed to work together seamlessly. The tidyverse packages share a common design philosophy, grammar, and data structures. This makes it easy to learn and use the packages, and it also makes it easy to combine them to create powerful data analysis workflows.
 
# Here are some of the most popular tidyverse packages:
   
# ggplot2: A powerful visualization package that makes it easy to create beautiful and informative plots.
# dplyr: A powerful data manipulation package that makes it easy to wrangle and clean data.
# tidyr: A package for tidying data, which means formatting it in a way that is easy to work with.
# readr: A package for reading data from different file formats.
# purrr: A package for functional programming in R.
# tibble: A modern re-imagining of data frames in R.
# stringr: A package for working with strings in R.
# forcats: A package for working with factors in R.
# lubridate: A package for working with dates and times in R.

 
 # Read the S&P 500 csv file
getwd()
  Nifty500 <- read.csv("Nifty500.csv")
str(Nifty500) 
 
#_________________________________________________________________Sorting of Data___________________________________________________________________________ 


 Nifty500$date = as.numeric(gsub("[^0-9\\.]", "", Nifty500$date))
 Nifty500$date
 
 Nifty500$Open = as.numeric(gsub("[^0-9\\.]", "", Nifty500$Open))
 Nifty500$Open
 
 Nifty500$High = as.numeric(gsub("[^0-9\\.]", "", Nifty500$High))
 Nifty500$High
 
 Nifty500$Close = as.numeric(gsub("[^0-9\\.]", "", Nifty500$High))
 Nifty500$Close
 
 Nifty500$Low = as.numeric(gsub("[^0-9\\.]", "", Nifty500$High))
 Nifty500$Low
 
 Nifty500$volume = as.numeric(gsub("[^0-9\\.]", "", Nifty500$High))
 Nifty500$volume
 Nifty500$year=date.mdy(Nifty500$date)$year
 
 year
 Nifty500 <- mutate(Nifty500, date = as.Date(date, format = "%d/%m/%Y"))
 Nifty500$quarter = ((as.numeric(dat$month)-1)%/%3) +1
 
# Get the data types of each column
column_types <- sapply(Nifty500, class)
print(column_types)


# Print the updated dataframe
print(Nifty500)

str(Nifty500)
# Show unique values in the "year" column of the Nifty500
unique_years <- unique(Nifty500$year)
unique_years

# Filter for 2019 data
closing_2019 <- Nifty500 %>%
  filter(year == '2019')

# Filter for 2020 data
closing_2020 <- Nifty500 %>%
  filter(year == '2020')

# Filter for 2021 data
closing_2021 <- Nifty500 %>%
  filter(year == '2021')

# Filter for 2022 data
closing_2022 <- Nifty500 %>%
  filter(year == '2022')

# Calculate mean and median of close for year 2019
mean(closing_2019$close)
median(closing_2019$close)

# Calculate mean and median of close for year 2020
mean(closing_2020$close)
median(closing_2020$close)

# Calculate mean and  median of close for year 2021
mean(closing_2021$close)
median(closing_2021$close)

# Calculate mean and  median of close for year 2022
mean(closing_2022$close)
median(closing_2022$close)


Nifty500 %>%
  # Filter for closing price of years 2019 to 2022
  filter(year %in% c("2019", "2020", "2021", "2022")) %>%
  # Group by size
  group_by(year) %>%
  # Get mean_close and median_close
  summarize(mean_close = mean(close),
            median_close = median(close))
#from this mean and median, we look out skewness to know abouyt the investment


# Mean vs Median
Nifty500 %>%
  # Create histogram of close
  ggplot(aes(close)) +
  geom_histogram(fill = "royalblue4")

# the use of mean is compared to the median with the histogram, to see hows the spread  

Nifty500 %>%
  # Get mean_close and median_close
  summarize(mean_close = mean(close),
            median_close = median(close))

# A: Mean is greater than the Median because the data is skewed to the right. 
# Extreme outliers on the right are pulling the Mean to have a greater value.

#NOw to know for a particular company investment @MRK
  def= Nifty500 %>%
  filter(Name == 'MRK')
def$Close= as.numeric(gsub("[^0-9\\.]", "", def$Close))

Nifty500 %>%  
summarize(defmean=mean(def$Close),
          defmed=median(def$Close))
#since here mean is less than median hence our data is 


#NOw to know for a particular company investment @MRK
def= Nifty500 %>%
  filter(Name == 'ADBE')
def$Close= as.numeric(gsub("[^0-9\\.]", "", def$Close))

Nifty500 %>%  
  summarize(defmean=mean(def$Close),
            defmed=median(def$Close))
#since here mean is more than median hence our data is ....



#NOw to know for a particular company investment @XOM
def= Nifty500 %>%
  filter(Name == 'XOM')
def$Close= as.numeric(gsub("[^0-9\\.]", "", def$Close))

Nifty500 %>%  
  summarize(defmean=mean(def$Close),
            defmed=median(def$Close))
#since here mean is more than median hence our data is ....


#NOw to know for a particular company investment @
def= Nifty500 %>%
  filter(Name == '')
def$Close= as.numeric(gsub("[^0-9\\.]", "", def$Close))

Nifty500 %>%  
  summarize(defmean=mean(def$Close),
            defmed=median(def$Close))
#since here mean is ___ than median hence our data is ....

#NOw to know for a particular company investment @CCL
def= Nifty500 %>%
  filter(Name == 'CCL')
def$Close= as.numeric(gsub("[^0-9\\.]", "", def$Close))

Nifty500 %>%  
  summarize(defmean=mean(def$Close),
            defmed=median(def$Close))
#since here mean is more than median hence our data is ....


#USE: we are using closing price to measure skewness of data by use of mean and median since skewness also includes the extremes of the dataset instead of
#focusing only on the average. Hence, investors take note of skewness tells about returns on investments. The average of the data set works out if an investor 
#holds a position for the long term. Therefore, extremes need to be looked at when seek short-term and medium-term security positions.
#However, because of skewness risk, it is better to obtain the performance estimations based on skewness. Moreover, the occurrence of return distributions coming close to normal is low.
#Skewness risk occurs when a symmetric distribution is applied to the skewed data. The financial models seeking to estimate an asset’s future performance consider a normal distribution. However, skewed data will increase the accuracy of the financial model.

#RESULT: If a return distribution shows a positive skew, investors can expect recurrent small losses and few large returns from investment. Conversely, a negatively skewed distribution implies many small wins and a few large losses on the investment.
#Hence, a positively skewed investment return distribution should be preferred over a negatively skewed return distribution since the huge gains may cover the frequent – but small – losses. However, investors may prefer investments with a negatively skewed return distribution. It may be because they prefer frequent small wins and a few huge losses over frequent small losses and a few large gains.

#____________________________________________________________________ Mode ______________________________________________________________________________________
mode(Nifty500$close) # Shows the data type of close


table(Nifty500$close) # Counts the number of times a value shows


# Print the value and frequency of the mode
mode_ind <- which.max(table(Nifty500$close)) # Shows the which value appeared the most time (mode), and at which column of the table
mode_ind

mode_close <- names(table(Nifty500$close))[which.max(table(Nifty500$close))]
mode_close
#: C. Less than the median because the mean is pulled at the right tail, higher values of the curve
# The mode is 34.5, found at column 4877
#we are validating data from mode, and
#drawing regression and support line


# How many times did mode_close appear in the data set?
mode_frequency <- max(table(Nifty500$close))
mode_frequency
#use: to know how much deviation had been happend in years, basically we are varfying the above result of skewness by use of mode.

#__________________________________________________________________________________________________________________________________________________________________
#In certain cases, mode can be an extremely helpful measure of central tendency. One of its biggest advantages is that it can be
#applied to any type of data, whereas both the mean and median cannot be calculated for nominal data. It is also not affected by
#extreme values in datasets with quantitative data. Thus, it can provide insights into almost any dataset despite the data distribution.



# MEASURES OF SPREAD
# Describes how spread apart or close together the data points are

# Variance - distance from each data point to the mean.
# variance - the sum of the square of each data point distance to the mean, divided by the no. of data points
# ⬆️ variance, ⬆️ spread  

# Standard Deviation (SD) - square root of the variance. 2 hours (sd) is easier to understand than 4 hours^s (variance)

# Mean Absolute Deviation (MAD) - sum of of absolute distance of each data point to the mean, divided by the no. of data points

# Standard Deviation vs Mean Absolute Deviation
# Similar, but not the same
# - SD squares distances, penalizes longer distances more than shorter ones
# - MAD penalizes each distance equally
# One is not better, but SD is more commonly used than MAD

# Why?
# Mathematical properties: Standard deviation is a more powerful measure of variability than mean absolute deviation. This is because standard deviation is based on the squared deviations from the mean, which gives it more weight to outliers. Mean absolute deviation, on the other hand, is based on the absolute deviations from the mean, which gives it less weight to outliers.
# Interpretability: Standard deviation is easier to interpret than mean absolute deviation. This is because standard deviation is measured in the same units as the data, while mean absolute deviation is measured in the units of the data divided by the number of data points.
# Statistical theory: Standard deviation is used in many statistical theories, such as the central limit theorem and the normal distribution. Mean absolute deviation is not used as frequently in statistical theories.


# However, there are also some cases where mean absolute deviation is a better measure of variability than standard deviation. 
# For example, mean absolute deviation is less sensitive to outliers than standard deviation, so it may be a better choice 
# when there are a few outliers in the data set.


#____________________________________________________________Variance and Standard Deviation___________________________________________________________________#

# Calculate variance and sd of the closing close for each year
Nifty500 %>% 
  group_by(year) %>% 
  summarize(var_close = var(close),
            sd_close = sd(close))

# Create subgraphs for each year: histogram of closing close
ggplot(Nifty500, aes(close)) +
  # Create a histogram
  geom_histogram(fill = "royalblue4") +
  # Create a separate sub-graph for each closing close
  facet_wrap(~ year)

#USE:Standard deviation is a basic mathematical concept that measures volatility in the market or the average amount by which individual data points differ
#from the mean, if the spread is more i.e the std dev, and variance is more then the stock will be more volatile and we do less investment there
#the another use is to know what is the median of the data and how much data is concentrated up there, to predict about the future of a stock

#_________________________________________________________________QUARTILES______________________________________________________________________________________#

# Quartiles - are a way to divide a set of numbers into four equal parts. 
# Imagine you have a group of people lined up based on their heights. 
# The first quartile (Q1) would be the height where a quarter of the people are shorter than that, and three-quarters are taller. 
# The second quartile (Q2) is the middle height, where half of the people are shorter and half are taller. 
# The third quartile (Q3) is the height where three-quarters of the people are shorter and one-quarter are taller. 
# It's like splitting the line of people into four equal sections to understand the distribution of heights.

# Quantiles - also known ar percentiles, are a generalized version of quartile
# For splitting the data into more pieces or parts


# Interquartile Range (IQR) - Difference of Q3 and Q1 (upper and lower whiskers of the box plot)
# Box plots are used for representing quartiles

# Outliers - Outliers are values that are significantly different from the other values in a dataset.
# They can be much larger or much smaller than the majority of the data points.
# Outliers stand out from the typical pattern of the data.
# They can affect the overall analysis or interpretation of the data.
# It is important to identify and consider outliers separately.


# Calculate the quartiles of the closing close
quantile(Nifty500$close)


filtered_Nifty500 <- Nifty500 %>%
  filter(close < 150)

quantiles_close <- quantile(filtered_Nifty500$close)
quantiles_close

#Quartiles are used to calculate the interquartile range, which is a measure of variability around the median. The interquartile range is simply calculated 
#as the difference between the first and third quartile: Q3–Q1. In effect, it is the range of the middle half of the data that shows how spread out the data is.

# Create boxplot
boxplot(filtered_Nifty500$close, main = "Boxplot of Close Values", ylab = "Close", ylim = range(filtered_Nifty500$close))

# Add labels to whiskers
text(1, quantiles_close[2], paste("Q1:", quantiles_close[2]), pos = 2)
text(1, quantiles_close[3], paste("Q2:", quantiles_close[3]), pos = 2)
text(1, quantiles_close[4], paste("Q3:", quantiles_close[4]), pos = 2)

#USE: we use quartile to analyse how 50% of data is concentrated there in mid of the bar and other 50% are varying in upper and closer outliers
#with use of box plot function from 2019 to 22, then viewing it individually per year too
# where in box plot the rectangular fig tell us about the IQR, and upper and lower bound by the outliers 

# Calculate the six quantiles that split up the closing close data into 5 pieces
quintiles_close <- quantile(filtered_Nifty500$close, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1))
#here we are calculating 6 quantiles just too see the varyness of the data to check the volatility of the stock

# Create boxplot with use of above concept just to analyse the stocks pictorically
boxplot(filtered_Nifty500$close, main = "Boxplot of Close Values", ylab = "Close", ylim = range(filtered_Nifty500$close), medcol = "red", whisklty = 2)

# Add labels to whiskers
text(1, quintiles_close[2], paste("Q1:", quintiles_close[2]), pos = 2)
text(1, quantiles_close[3], paste("Q2:", quintiles_close[3]), pos = 2)
text(1, quintiles_close[4], paste("Q3:", quintiles_close[4]), pos = 2)
text(1, quintiles_close[5], paste("Q4:", quintiles_close[5]), pos = 2)


# Calculate the deciles of the closing close
quantile(Nifty500$close, probs = seq(0, 1, 0.1))
#dividing the stock per year too analyse more closely about its volatility


#Finding outliers using IQR

# Calculate average closing per day: ave_closing_by_year
ave_closing_by_year <- Nifty500 %>%
  group_by(year) %>%
  summarize(ave_close = mean(close))

print(ave_closing_by_year)



# Compute the first and third quantiles and IQR of ave_close
q1 <- quantile(ave_closing_by_year$ave_close, 0.25)
q3 <- quantile(ave_closing_by_year$ave_close, 0.75)
iqr <- q3 - q1

print(iqr)

# Calculate the lower and upper cutoffs for outliers
lower <- q1 - 1.5 * iqr
upper <- q3 + 1.5 * iqr

print(lower)
print(upper)

# Filter ave_closing_by_year to find outliers
ave_closing_by_year %>%
  filter(ave_close < lower | ave_close > upper)

# 2022 is the year with an average closing price above the upper whisker (outlier)


# Create a new column 'month_year' based on 'date'
Nifty500 <- mutate(Nifty500, month_year = format(date, "%b %Y"))

print(Nifty500)


# Create subgraphs for each year: histogram of closing close
ggplot(Nifty500, aes(close)) +
  # Create a histogram
  geom_histogram(fill = "royalblue4") +
  # Create a separate sub-graph for each closing close
  facet_wrap(~ month_year)
#we analyzing our spreadness of graph monthly, previously we had done yeraly basis

#Finding outliers using IQR

# Calculate average closing per day: ave_closing_by_year
ave_closing_by_month_year <- Nifty500 %>%
  group_by(month_year) %>%
  summarize(ave_close = mean(close))

print(ave_closing_by_month_year)

# Compute the first and third quartiles and IQR of ave_close
q1 <- quantile(ave_closing_by_month_year$ave_close, 0.25)
q3 <- quantile(ave_closing_by_month_year$ave_close, 0.75)
iqr <- q3 - q1

print(iqr)

# Calculate the lower and upper cutoffs for outliers
lower <- q1 - 1.5 * iqr
upper <- q3 + 1.5 * iqr

print(lower)
print(upper)


# Filter ave_closing_by_year to find outliers
ave_closing_by_month_year %>%
  filter(ave_close < lower | ave_close > upper)

# Dec 2022 is an outlier with an average closing price of 106
#the main use of iqr is to check the spread only if the iqr is thick in length(box plot) then the candle is good, we can tell the investment is less volatile
#and vice-versa