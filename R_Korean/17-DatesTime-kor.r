#################################
## Chapter 17: Dates and Times ##
## Presented by Nayoung Ku     ##
#################################
## 0. Introduction

### Objectives
#1. 날짜(Date)와 날짜-시간(Datetime) 객체 생성
#2. 날짜-시간 구성 요소 다루기
#3. 시간 연산 수행
#4. 시간대를 다루는 방법 인식


# 패키지 불러오기
library(tidyverse)
library(nycflights13)

flights

### 주요 개념
# 날짜/시간 객체의 유형:
#    1. date = `<date>`: 날짜(연도, 월, 일)를 저장. 시간 정보 없음.
#    2. time = `<hms>`: 하루의 시간(시, 분, 초)을 저장. 날짜 정보 없음.
#    3. datetime = `<dttm>` = POSIXct: 날짜 및 시간 정보 포함. 시간대 적용 가능.
# 시간대(Timezone)에 대한 인식이 중요함
today()
class(today()) 

today()
class(today()) 

now() # 타임스탬프와 함께 현재 로컬 시간 출력
class(now())

### 참고: 
# dttm: `lubridate` 등 일부 R 패키지가 활용하는 날짜-시간 클래스.
# POSIXt: R에서 기본적으로 사용하는 날짜-시간 클래스, 보다 광범위하게 활용됨.

########################

## 1. 날짜 및 시간 객체 생성하기

### `lubridate` 함수

#### `ymd_hms()`: 기본적으로 UTC로 변환됨
ymd_hms("2025-02-28 15:39:29")

#### `make_date()` &  `make_datetime()`: 연도, 월, 일, 시, 분 등이 분리된 열로 되어 있을 때 이를 하나의 날짜/시간 객체로 변환

head(flights,3) 
# `flights` 데이터셋에서,
# `year`,  `month`, `day`: 출발 날짜
# `hour`, `minute`: 출발 예정 시간(시, 분 단위)
flights |>
    select(year, month, day, hour, minute) |>
    mutate(
        departure = make_datetime(year, month, day, hour, minute),
        dep_date = make_date(year, month, day)
    ) |>
    head(3)
#departure = <dttm> & dep_date = <date>

#### `as_date()` & `as_datetime()`
# 날짜 및 시간을 표준화하여 저장할 때 사용되며, 값이 Unix epoch 기준으로 변환됨.
# Unix Epoch: 1970-01-01 00:00:00 UTC
# `as_date()`: 일(day) 단위로 변환
# `as_datetime()`: 초(second) 단위로 변환
as_datetime(365)

## 날짜-시간 요소 추출하기
# `year()`, `month()`, `hour()`, `minute()`, `second()`
# `day()`:
#    * `mday()`: 월 기준 일(day of the month), `day()`와 동일
#    * `yday()`: 연 기준 일(day of the year)
#    * `wday()`: 주 기준 일(day of the week)

now()
datetime_example <- ymd_hms("2025-02-28 15:39:29")

year(datetime_example)
mday(datetime_example)
yday(datetime_example) # 31 + 28  = 59

wday(now()) # 요일 순서: ['Sun','Mon','Tue','Wed','Thu','Fri','Sat']
wday(now(), label = TRUE)
wday(now(), label = TRUE, abbr = FALSE)

########################

## 2. 시간 연산

### 날짜-시간 반올림
# 유형:
#    * `round_date()`, `floor_date()`, `ceiling_date()`
# 인수:
#    * `<>_date(<datetime>, unit = "hour")`  

datetime_example
print("=========")
round_date(datetime_example, unit = "hour")
round_date(datetime_example, unit = "min")
round_date(datetime_example, unit = "day")

### 날짜-시간 업데이트
hour(datetime_example)

hour(datetime_example) <- hour(datetime_example) + 1
hour(datetime_example)
datetime_example

update(datetime_example, years = 2024, months = 5, mdays = 30)


### 시간 간격 개념
# Duration: 정확한 초(second) 단위 차이 계산
ny_age <- today() - ymd("2004-09-30")
ny_age 

as.duration(ny_age) # `lubridate`는 항상 초 단위로 저장함

#Duration의 일(day) 단위는 초(second) 단위로 계산됨
ddays(0:3)
dhours(2)
ddays(0:3) + dhours(2)


# Period: 달력(Calendar) 기준의 시간 간격 (예: 1일)
days(1)

ymd_hms(now(), tz = "Asia/Seoul")
ymd_hms(now(), tz = "Asia/Seoul") + days(1)

# Interval: 시작 및 종료 날짜-시간 구간

y2023 <- ymd("2023-01-01") %--% ymd("2024-01-01") # %--%  = to
y2023
y2023 / days(1) # 2023년의 총 일수

y2024 <- ymd("2024-01-01") %--% ymd("2025-01-01")
y2024
y2024 / days(1) 
class(y2024)

y2024 / days(1) 
y2024 / ddays(1)
y2024 / dyears(1)

########################

## 3. 시간대(Time Zone)

# `clock` 패키지는 백엔드로 사용됨 (업데이트 용이)
# 시간대 정의는 계속 변경됨 (예: 서머타임 변동)
internship_start_korea <- ymd_hms("2025-01-02 09:00:00", tz = "Asia/Seoul")
internship_start_korea

### 시간대 변환
# 시간의 정의는 유지하고, 시간대만 변경
force_tz(internship_start_korea, "Europe/Paris")

with_tz(internship_start_korea, "Europe/Paris")
with_tz(internship_start_korea, "America/Chicago")
with_tz(internship_start_korea, "Australia/Sydney")

### 서머타임(Daylight saving time)
internship_starts <- internship_start_korea + weeks(9:20)
internship_starts

# Europe/Rome은 3월 마지막 일요일부터 서머타임 시작
internship_starts |> with_tz("Europe/Rome") |> hour()

# America/Chicago는 3월 둘째 주 일요일부터 서머타임 시작
internship_starts_chicago <- with_tz(internship_start_korea, "America/Chicago") + weeks(9:20)
internship_starts_chicago |> with_tz("Europe/Rome") |> hour()

# UTC는 서머타임 영향을 받지 않음
internship_starts_chicago |> with_tz("UTC") |> hour()

# Australia/Sydney는 4월 첫째 주 일요일에 서머타임 종료
internship_starts_chicago |> with_tz("Australia/Sydney") |> hour()
