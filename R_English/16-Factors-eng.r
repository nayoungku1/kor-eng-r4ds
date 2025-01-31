##############################
## Chapter 16: Factors      ##
## Presented by Nayoung Ku  ##
##############################

## 0. Introduction
### Factor: Categorical data, often stored as "factors" in R, represents variables with a  fixed set of possible values.

### Why is it important?: Data integrity, accurate analysis, effective visualization

### Objectives:
#1. Create `factor` variables
#2. Explore the General Social Survey dataset via `forcats::gss_cat`
#3. Reorder factor levels
#4. Modify factor levels

library(tidyverse)

########################

## 1. Create factors

### `factor()`: create factor from a character or numeric vector 
### Example: Month

# Create a list of the valid levels
month_levels <- c(
    "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

x1 <- c("Dec", "Apr", "Jan", "Mar")
x2 <- c("Dec", "Apr", "Jam", "Mar") # Issue #1: typo problem

print(class(x1))
print(x1) 
sort(x1) # Issue #2: Alphabetic order does not match with month level

# Using factors can fix the issues with strings
# Create a factor >> follow specific order
y1 <- factor(x1, levels = month_levels)
print(class(y1))
print(y1) #Before sort: same with chr
sort(y1) 

y2 <- factor(x2, levels = month_levels)
print(class(y2))
#**WARNING**: Value not in the level (typo) is automatically converted to NA
y2 

print(factor(x1)) #not defining specific level -> alphabetic order
levels(y1)

### `forcats::fct()`: similar with `factor()` but...
# Error in `fct()`:  All values of `x` must appear in `levels` or `na`
y2 <- fct(x2, levels = month_levels) #Error: Missing level, "Jam"

### `col_factor()`: Define a column as factor when you read a file
# df <- read_csv(csv, col_types = cols(month = `col_factor(month_levels)`))
csv <- "
month,value
Jan,12
Feb,56
Mar,12
"
df <- read_csv(csv, col_types = cols(month = col_factor(month_levels)))
df#$month

### [Tip] If you want to order the levels to match the order of the first appearance in the data: 1. `unique()`; 2. `fct_inorder()`

#`unique()`
x1 <- c('Dec','Apr','Jan','Mar',"Dec")
f1 <- factor(x1, levels = unique(x1)) 
f1
#`fct_inorder()`
f2 <- x1 |> factor() |> fct_inorder()
sort(f2)

########################

## 2. General Social Survey (`gss_cat`) Dataset

### Dataset introduction: A tibble with 21483 obervations & 9 columns

head(gss_cat) #General Social Survey, a long-running US survey 
?gss_cat

# simple EDA

gss_cat |>
    count(race)
# What is the most common relig in this survey? 
gss_cat |>
    count(relig) |>
    arrange(desc(n)) 
# What’s the most common partyid?
gss_cat |>
    count(partyid) |>
    arrange(desc(n)) 
levels(gss_cat$partyid)

# Average hours for watching tv by religion
relig_summary <- gss_cat |>
  group_by(relig) |> 
  summarize(
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  ) |>
    arrange(desc(tvhours))

relig_summary

ggplot(relig_summary, aes(x = tvhours, y = relig)) +
  geom_point() 
# It is not easy to compare since the pre-defined `relig` level sorts them!

levels(gss_cat$relig)

########################

## 3. Reordering Factors
#Factors can be reordered based on logical or statistical criteria

### A. Use `fct_reorder()` to reorder levels based on another variable.
#`fct_reorder(factor_var, numeric_var)`

reordered_reli_tv <- relig_summary |>
    mutate(
        relig = fct_reorder(relig, tvhours) #relig(factor) is reorganized by tvhours
    ) |> arrange(relig)
reordered_reli_tv

levels(reordered_reli_tv$relig)
levels(relig_summary$relig)

ggplot(relig_summary, aes(x = tvhours, y = fct_reorder(relig, tvhours))) +
  geom_point()

### B. Use `fct_relevel()` to move a level to the front or specify the exact order.
#`fct_relevel(factor, levels)` 
# any levels not mentioned will be left in their existing order
f <- factor(c("a", "b", "c", "d"), levels = c("b", "c", "d", "a"))
levels(fct_relevel(f))
levels(fct_relevel(f, "a"))
levels(fct_relevel(f, "b", "a"))

rincome_summary <- gss_cat |>
    group_by(rincome) |>
    summarize(
        age = mean(age, na.rm = TRUE),
        tvhours = mean(tvhours, na.rm = TRUE),
        n = n()
    ) 
rincome_summary

rincome_summary|>
    #ggplot(aes(x = age, y = rincome)) +
    ggplot(aes(x = age, y = fct_relevel(rincome,"Not applicable"))) +
        geom_point()

### C. For changing the order of a bar plot,
#1. by number of observations with each level (largest first, decreasing manner): `fct_infreq()` 
#2. Sort in reverse order: `fct_rev()`

levels(gss_cat$marital)
gss_cat |>
    mutate(
        marital = marital |> 
            fct_infreq() |>
            fct_rev()
          ) |>
    ggplot(aes(x = marital)) +
        geom_bar()

########################

## 4. Modifying Factor Levels

#We can change the values of the levels!

### A. Rename levels with `fct_recode()`:
# `fct_recode(factor_var, "new_level" = "old_level" )`
levels(gss_cat$partyid)

gss_cat |>
    mutate(
        partyid = fct_recode(partyid, 
            # Changing name: <new name> = <old name>
            "Republican, strong" = "Strong republican",
            "Republican, weak" = "Not str republican",
            "Independent, near rep" = "Ind,near rep",
            "Independent, near dem" = "Ind,near dem",
            "Democrat, weak" = "Not str democrat",
            "Democrat, strong" = "Strong democrat",
            # Combine groups by recycling the name
            "Other" = "No answer",
            "Other" = "Don\'t know",
            "Other" = "Other party"
        )
    ) |>
    count(partyid)

### B. Collapse levels with `fct_collapse()`:
# `fct_collapse(factor_var, new_level = c("old_level1", "old_level2"))`

# If you have too many levels: `fct_collapse()` is much useful! 
gss_cat |>
    mutate(
        partyid = fct_collapse(partyid,
            "other" = c("No answer","Don\'t know","Other party"),
            "rep" = c("Strong republican","Not str republican"),
            "ind" = c("Ind,near rep", "Independent", "Ind,near dem"),
            "dem" = c("Strong democrat","Not str democrat")                      
        )
    ) |>
    count(partyid)

### C. Lump uncommon factor together with `fct_lump*()`:
#`fct_lump_lowfreq(factor)` : lumps together the least frequent levels into "Other"
#`fct_lump_min(factor, min = <input_minimum>)`: lumps levels that appear fewer than minimum times
#`fct_lump_prop(factor, prop = p)`: lumps levels that appear in fewer than prop * n times
?fct_lump_lowfreq

gss_cat |>
    mutate(relig= fct_lump_lowfreq(relig) ) |>
    count(relig)
# 'No answer''Don\'t know''Other party' lumped together