#########################################
# CHAPTER 10: EXPOLARTORY DATA ANALYSIS #
#########################################

#The tools of EDA are as follows:  
# 1. visualization,
# 2. transformation,
# 3. modeling.

# install.packages("tidyverse")
library(tidyverse)

head(diamonds)
################
# 1. Variation #
################
# Variation: difference within a variable's values
# Understanding variation is crucial for data analysis, as it helps (1) identify patterns, (2) anomalies, and the (3) overall distribution of data.


ggplot(diamonds) + 
  geom_histogram(aes(x = carat), binwidth = 0.5) + 
  ggtitle("Distribution of all carats") 
# Right skewedness

summary(diamonds['carat']) 

### 1.1. Typical Values

# Visualization of `carat` distribution for smaller diamonds  
smaller <- diamonds |> 
  filter(carat < 3)

ggplot(smaller, aes(x = carat)) + 
  geom_histogram(binwidth = 0.01) + 
  ggtitle("Distribution of smaller carats") 

# Right_skewed_in_each_group
# Peaks at 0.5, 1, 1.5, 2
# Clusters indicates subgroups
# Q. Why are there are more diamonds at whole carats & common fractions of carats?

### 1.2. Unusual Values

# Outliers: unusual observations; data points that don't seem to fit the pattern
# They might be due to data entry errors, extremes of the data collection, or new discoveries, etc.

# Distribution of 'y' variable
ggplot(diamonds, aes(x = y)) +
  geom_histogram(binwidth = 0.5) +
  labs(title = "Distribution of y (width) in Diamonds")

# Distribution of 'y' variable
# `coord_cartesian()` has `xlim` & `ylim` arguments to zoom in
ggplot(diamonds, aes(x = y)) +
  geom_histogram(binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50)) + 
  labs(title = "Distribution of y (width) in Diamonds")

unusual <- diamonds |>
  filter(y <3 | y > 20) |>
  select(price, x, y, z) |>
  arrange(y)

unusual

summary(diamonds["price"])

##################
# 1. We know that diamonds cannot have a width of 0mm, so these values must be incorrect.
#    * That means the `NA`s or missing data were coded as 0.
#    * We can re-code these values as `NA`s to prevent misleading calculation
# 2. We can suspect the prices of width 32mm & 59mm diamonds: they are more than an inch long but too cheap!!

# There are two options to move on: drop or replace

# 1. Drop the entire row with strange values
#diamonds2 <- diamonds |> 
#  filter(between(y,3,20))

# 2. Replace the unusual values with missing values (Recommended)
diamonds2 <- diamonds |>
  mutate(y = if_else(y < 3 | y > 20, NA, y))
diamonds2 |> 
  filter(is.na(y))

ggplot(diamonds2, aes(x = x, y = y)) + 
  geom_point(na.rm = TRUE) # `na.rm = TRUE` is set to suppress the warning due to NA values in 9 rows

# Sometimes NA is critical to understand the data 

# install.packages("nycflights13")
library(nycflights13)
View(flights)
# For this dataset, NA in dep_time indicates that the flight is cancelled.
flights_mutated <- nycflights13::flights |>
  mutate(
    cancelled = is.na(dep_time), 
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + (sched_min / 60)
  ) 
# `%/%` (Modulo): an operator gives the remainder of the division of two numbers.
# `%%` (Integer Division):  an operator performs integer division, giving the quotient without the fractional part.

head(flights_mutated) 

##################
# 2. Covariation #
##################
# Covariation is the tendency for the values of two or more variables to vary together in a related way.

### 2.1. A Categorical and a Numerical Variable

ggplot(diamonds) +
  geom_freqpoly(aes(x = price, color = cut), binwidth = 500, linewidth=0.75)

# To compare the group, count might not be the best option.
# Normalization by de#nsity is required.

# `y = after_stat(density)`: Normalized value by dividing the frequency of each section by the total number of data
# Summation of `density` is 1, indicating the relative distribution of the data.
ggplot(diamonds) +
  geom_freqpoly(aes(x = price, y = after_stat(density), color = cut), binwidth = 500, linewidth=0.75)

ggplot(diamonds) +
  geom_boxplot(aes(x = cut, y = price)) + 
  coord_flip()
# Q. Does it mean fair diamonds (the lowest quality) have the highest average price?

# Price & carat has relationship (which will be discussed later.)
ggplot(diamonds, aes(x = carat, y = price, color = cut)) +
  geom_point(alpha = 0.5, size = 1) +
  scale_x_log10() + # Log scale for x-axis (Carat)
  scale_y_log10() + # Log scale for y-axis (Price)
  labs(
    title = "Relationship between Carat and Price",
    x = "Carat (log scale)",
    y = "Price (log scale)"
  ) +
  theme_minimal()


### 2.2. Two Categorical Variables

# ggplot2's `geom_count()`
# The size of each circle in the plot displays how many observations occurred at each combination of values.
ggplot(diamonds, aes(x = cut, y = color)) +
  geom_count()

# dplyr
diamonds |> 
  count(color, cut) |>
  arrange(desc(n)) |>
  head(10)

# `geom_tile`
diamonds |> 
  count(color, cut) |>  
  ggplot() +
  geom_tile(aes(x = color, y = cut, fill = n))

### 2.3. Two Numerical Variables

ggplot(smaller) +
  geom_point(aes(x = carat, y = price), alpha = 0.1)

# Bin one continuous variable (`carat`) so it acts like a categorical variable
ggplot(smaller) + 
  geom_boxplot(aes(x = carat, y = price,
                   group = cut_width(carat, 0.1))) # cut_width(x, width)

### 2.4. Pattenrs & Models
# If a systematic relationship exists between two variables it will appear as a pattern in the data.
# Models: a tool for extracting patterns out of data.
# It’s possible to use a model to remove the very strong relationship.

# install.packages("tidymodels")
library(tidymodels)

#Log transform the values of `carat` & `price` first.
diamonds <- diamonds |>
  mutate(
    log_price = log(price),
    log_carat = log(carat)
  )

View(diamonds)

# Fit a model to the log-transformed values.
diamonds_fit <- linear_reg() |>
  fit(log_price ~ log_carat, data = diamonds)
diamonds_fit

# `augment()`: a function calculating predicted value and residual by dataset, `diamonds_fit` and  `diamonds`
# Exponentiate the residuals to put them back on the scale of raw prices.
diamonds_aug <- augment(diamonds_fit, new_data = diamonds) |>
  mutate(.resid = exp(.resid))

# Residuals only deal with the values remaining after removing the predicted relationships (the part explained by the model).
ggplot(diamonds_aug) + 
  geom_point(aes(x = carat, y = .resid))
# By using exponentiated residual, meaning prices removed the relationship between carat and price, we can see the **relationship is relative to their size**.

ggplot(diamonds_aug) + 
  geom_boxplot(aes(x = cut, y = .resid)) + 
  coord_flip()
# After removing the relationship between carat and price, we can see that **better quality diamonds are more expensive.**