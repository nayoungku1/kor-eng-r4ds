#################################
## Chapter 17: Dates and Times ##
## Presented by Nayoung Ku     ##
#################################
## 0. Introduction

### Objectives
#1. **Create** date and datetime objects
#2. Work with **datetime components**
#3. Perform **arithmetics** on time
#4. Recognize ways to deal with **timezones**


# Import package
library(tidyverse)
library(nycflights13)

flights

### Vocabularies
# types of date/time objects:
#    1. date = `<date>`: Stores the date (year, month, day). No time information.
#    2. time = `<hms>`: Stores the time of day (hours, minutes, seconds). No date information. 
#    3. datetime = `<dttm>` = POSIXct: Includes both date and time information. Time zones can be applied.
# Important to be aware of timezones
today()
class(today()) 

today()
class(today()) 

now() # time with timestamp; local time zone
class(now())

### NOTICE: 
# dttm: date-time class that some R packages (e.g., lubridate) utilize.
# POSIXt: the default date-time class of R, more broadly utilized.

########################

## 1. Creating date and time objects

### `lubridate` functions

#### `ymd_hms()`: By default, it puts everything in UTC
ymd_hms("2025-02-28 15:39:29")

#### `make_date()` &  `make_datetime()`: If your data contains separate columns for year, month, day, hour, minute, etc., these will turn them into a datetime or date

head(flights,3) 
# In the `flights` dataset,
# `year`,  `month`, `day`: Date of departure
# `hour`, `minute`: Scheduled departure time broken into hours and minutes.
flights |>
    select(year, month, day, hour, minute) |>
    mutate(
        departure = make_datetime(year, month, day, hour, minute),
        dep_date = make_date(year, month, day)
    ) |>
    head(3)
#departure = <dttm> & dep_date = <date>

#### `as_date()` & `as_datetime()`
# Standardized way to store times when values are given as large numbers, which represent days or seconds, known as the Unix epoch.
# Unix Epoch: 1970-01-01 00:00:00 UTC
# `as_date()`: stores days
# `as_datetime()`: stores seconds
as_datetime(365)

## Getting component
# `year()`, `month()`, `hour()`, `minute()`, `second()`
# `day()`:
#    * `mday()`: day of the month; same as `day()`
#    * `yday()`: day of the year
#    * `wday()`: day of the week

now()
datetime_example <- ymd_hms("2025-02-28 15:39:29")

year(datetime_example)
mday(datetime_example)
yday(datetime_example) # 31 + 28  = 59

wday(now()) # Order of the levels: ['Sun','Mon','Tue','Wed','Thu','Fri','Sat']
wday(now(), label = TRUE)
wday(now(), label = TRUE, abbr = FALSE)

########################

## 2. Arithmetic on time

### Rounding datetimes
# Types: `round_date()`, `floor_date()`, `ceiling_date()`
# Argument:`*_date(<datetime>, unit = "hour")`  

datetime_example
print("=========")
round_date(datetime_example, unit = "hour")
round_date(datetime_example, unit = "min")
round_date(datetime_example, unit = "day")

### Updating datetimes
hour(datetime_example)

hour(datetime_example) <- hour(datetime_example) + 1
hour(datetime_example)
datetime_example

update(datetime_example, years = 2024, months = 5, mdays = 30)


### Concepts of time spans
# Durations: exact number of seconds elapsed
ny_age <- today() - ymd("2004-09-30")
ny_age 

as.duration(ny_age) #lubridate always stores it in seconds

#Duration days are calculated in number of seconds
ddays(0:3)
dhours(2)
ddays(0:3) + dhours(2)


# Periods: human units (like days)
days(1)

ymd_hms(now(), tz = "Asia/Seoul")
ymd_hms(now(), tz = "Asia/Seoul") + days(1)

#Intervals: start and end datetime

y2023 <- ymd("2023-01-01") %--% ymd("2024-01-01") # %--%  = to
y2023
y2023 / days(1) # Number of days in 2023

y2024 <- ymd("2024-01-01") %--% ymd("2025-01-01")
y2024
y2024 / days(1) 
class(y2024)

y2024 / days(1) 
y2024 / ddays(1)
y2024 / dyears(1)

# Summary
start_time <- ymd("2025-01-01")

# Duration: Fixed in seconds >> Calculates exact difference in "seconds" after 1 year
dmonths(12) 
# `lubridate` calculates based on an average year length of 365.25 days (including leap years)
start_time + dyears(1)

# Period: Calculates the date exactly one year later based on the calendar 
years(1) 

start_time + years(1) 

# Interval: Compares the duration between two dates
time_interval <- start_time %--% (start_time + years(1))
as.duration(time_interval) # Actual calculation in seconds

########################

## 3. Time Zone

# The package `clock` is used as a backend (easier to update)
# The definition of time zones is still changing: Daylight saving, etc.
internship_start_korea <- ymd_hms("2025-01-02 09:00:00", tz = "Asia/Seoul")
internship_start_korea

### Converting Time Zone
# Keep the definition of the time but only change the time zone
force_tz(internship_start_korea, "Europe/Paris")

with_tz(internship_start_korea, "Europe/Paris")
with_tz(internship_start_korea, "America/Chicago")
with_tz(internship_start_korea, "Australia/Sydney")

### Daylight saving time
internship_starts <- internship_start_korea + weeks(9:20)
internship_starts #|> hour()

# Europe/Rome starts DST on the last Sunday of March, moving one hour ahead
internship_starts |> with_tz("Europe/Rome") |> hour()

# America/Chicago starts DST on the second Sunday of March
internship_starts_chicago <- with_tz(internship_start_korea, "America/Chicago") + weeks(9:20)
internship_starts_chicago |> with_tz("Europe/Rome") |> hour()

# UTC is not affected by DST
internship_starts_chicago |> with_tz("UTC") |> hour()

# Australia/Sydney ends DST on the first Sunday of April
internship_starts_chicago |> with_tz("Australia/Sydney") |> hour()