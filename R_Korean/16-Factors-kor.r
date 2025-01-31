##############################
## Chapter 16: Factors      ##
## Presented by Nayoung Ku  ##
##############################

## 0. Introduction
### Factor: 범주형 데이터(categorical data)로, R에서는 "factors"로 저장되며, 미리 정해진 값들의 집합을 가진 변수를 나타냄.

### 왜 중요한가?: 데이터 무결성 유지, 정확한 분석, 효과적인 시각화

### 목표:
#1. `factor` 변수 생성
#2. `forcats::gss_cat`을 활용하여 General Social Survey 데이터 탐색
#3. Factor levels 재정렬
#4. Factor levels 수정

library(tidyverse)

########################

## 1. Factor 생성

### `factor()`: 문자 또는 숫자 벡터에서 factor 생성
### 예제: 월(Month)

# 유효한 level 목록 생성
month_levels <- c(
    "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

x1 <- c("Dec", "Apr", "Jan", "Mar")
x2 <- c("Dec", "Apr", "Jam", "Mar") # Issue #1: 철자 오류 발생

print(class(x1))
print(x1) 
sort(x1) # Issue #2: 알파벳순 정렬이 월의 순서와 맞지 않음

# Factor를 사용하여 문자열 관련 문제 해결
# Issue 2 해결: Factor 생성 >> 특정 순서를 따르게 함
y1 <- factor(x1, levels = month_levels)
print(class(y1))
print(y1) # 정렬 전: 문자형과 동일
sort(y1) 

# Issue 1 해결: 
y2 <- factor(x2, levels = month_levels)
print(class(y2))
#**WARNING**: 정의되지 않은 값(철자 오류)은 자동으로 NA로 변환됨
y2 


### NOTICE
print(factor(x1)) # 특정 level을 정의하지 않으면 알파벳순으로 정렬
levels(y1)

### `forcats::fct()`: `factor()`와 유사하지만...
# `fct()`는 모든 값이 `levels` 또는 `na`에 포함되어야 함
y2 <- fct(x2, levels = month_levels) # Error: "Jam"이 levels에 없음

### `col_factor()`: 파일을 읽을 때 특정 컬럼을 factor로 정의
# df <- read_csv(csv, col_types = cols(month = `col_factor(month_levels)`))
csv <- "
month,value
Jan,12
Feb,56
Mar,12
"
df <- read_csv(csv, col_types = cols(month = col_factor(month_levels)))
df#$month

### [Tip] 데이터에서 처음 등장한 순서대로 levels를 정렬하고 싶다면: 1. `unique()`; 2. `fct_inorder()`

#`unique()`
x1 <- c('Dec','Apr','Jan','Mar',"Dec")
f1 <- factor(x1, levels = unique(x1)) 
f1

#`fct_inorder()`
f2 <- x1 |> factor() |> fct_inorder()
sort(f2)

########################

## 2. General Social Survey (`gss_cat`) 데이터셋

### 데이터셋 소개: 21,483개의 관측치와 9개의 열을 포함한 tibble 데이터셋

head(gss_cat) # General Social Survey (미국의 장기 설문 조사)
?gss_cat

# 간단한 탐색적 데이터 분석(EDA)

gss_cat |>
    count(race)
# 이 설문에서 가장 흔한 종교는?
gss_cat |>
    count(relig) |>
    arrange(desc(n)) 
# 가장 흔한 정당 소속은?
gss_cat |>
    count(partyid) |>
    arrange(desc(n)) 
levels(gss_cat$partyid)

# relig 별 평균 tv 시청시간
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
# 미리 정의된 relig level이 정렬을 방해하여 비교가 어려움

levels(gss_cat$relig)

########################

## 3. Factor 재정렬
# Factors는 논리적 혹은 통계적 기준에 따라 재정렬 가능

### A. `fct_reorder()`를 사용하여 다른 변수의 값을 기준으로 정렬
# `fct_reorder(factor_var, numeric_var)`

reordered_reli_tv <- relig_summary |>
    mutate(
        relig = fct_reorder(relig, tvhours) # relig(factor)를 tvhours 기준으로 재정렬
    ) |> arrange(relig)

reordered_reli_tv

levels(reordered_reli_tv$relig)
levels(relig_summary$relig)

ggplot(relig_summary, aes(x = tvhours, y = fct_reorder(relig, tvhours))) +
  geom_point()

### B. `fct_relevel()`을 사용하여 특정 level을 앞쪽으로 이동하거나 순서를 지정
# `fct_relevel(factor, levels)`: 언급되지 않은 level은 언급된 level 다음으로 정렬됨.
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

### C. Bar plot을 그리기 위해 정렬 순서를 바꿀 때,
#1. `fct_infreq()` : 가장 많이 등장한 level이 가장 먼저 정렬 (내림차순 level 정렬)
#2.  `fct_rev()`: 기존 level과 정 반대로 정렬

levels(gss_cat$marital)
gss_cat |>
    mutate(
        marital = marital |> 
            #fct_infreq() |>
            fct_rev()
          ) |>
    ggplot(aes(x = marital)) +
        geom_bar()

########################

## 4. Factor Levels 수정

# Levels의 값을 변경 가능

### A. `fct_recode()`를 사용하여 level 이름 변경
# `fct_recode(factor_var, "new_level" = "old_level" )`
levels(gss_cat$partyid)

gss_cat |>
    mutate(
        partyid = fct_recode(partyid, 
            # 이름 바꾸기: <새 이름> = <기존 이름>
            "Republican, strong" = "Strong republican",
            "Republican, weak" = "Not str republican",
            "Independent, near rep" = "Ind,near rep",
            "Independent, near dem" = "Ind,near dem",
            "Democrat, weak" = "Not str democrat",
            "Democrat, strong" = "Strong democrat",
            # 이름 재사용 하여 하나의 그룹으로 합치기
            "Other" = "No answer",
            "Other" = "Don\'t know",
            "Other" = "Other party"
        )
    ) |>
    count(partyid)

### B. `fct_collapse()`를 사용하여 여러 levels를 하나로 합치기
# `fct_collapse(factor_var, new_level = c("old_level1", "old_level2"))`

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

### C. 드문 factor를 하나로 합치기 (`fct_lump*()` 활용)
# `fct_lump_lowfreq(factor)`: 가장 빈도가 낮은 level을 "Other"로 합침
# `fct_lump_min(factor, min = <input_minimum>)`: 최소 등장 횟수보다 적은 level을 합침
# `fct_lump_prop(factor, prop = p)`: 전체 비율이 `prop * n`보다 적은 level을 합침
?fct_lump_lowfreq

gss_cat |>
    mutate(relig= fct_lump_lowfreq(relig) ) |>
    count(relig)
# 'No answer', 'Don't know', 'Other party'가 하나로 합쳐짐