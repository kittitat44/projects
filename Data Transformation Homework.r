# -*- coding: utf-8 -*-

# -- Sheet --

# **NYC flights 13**  ver 1


library(dplyr)
library(tidyverse)

install.packages("nycflights13")

library(nycflights13)

data(package = "nycflights13")  

glimpse(flights)

glimpse(airlines)

glimpse(airports)

flights <- library("nycflights13")
complete.cases(flights)
sum(complete.cases(flights))











# # 1. What are top 5 airlines in 2013?


flights %>%
left_join(airlines, by = "carrier") %>%
group_by(name) %>% 
summarise(flights = n()) %>%
arrange(desc(flights)) %>%
head(5)

# # 2. What was the most canceled flight in 2013 


flights %>% 
filter(is.na(dep_time)) %>%
left_join(airlines, by = "carrier") %>%
group_by(name) %>% 
summarise(canceled = n()) %>%
arrange(desc(canceled)) %>%
head(5)

# # 3. What was the most aircraft manufacturer in 2013?


flights %>%
count(tailnum) %>%
inner_join(planes, by = "tailnum") %>%
select(tailnum, n , manufacturer)

result <- aggregate(pop_tai$n, by = list(manufacturer = pop_tai$manufacturer), sum) %>% 
rename(count_flights = x) %>%
arrange(desc(count_flights)) %>%
head(5)

view(result)

flights <- read.csv("flights.csv")

library(dplyr)
library(tidyverse)

read_csv("flights.csv")

flights <- read_csv("flights.csv")

complete.cases(flights)
sum(complete.cases(flights))

nrow(flights)

sum(complete.cases(flights))/nrow(flights)

# # Complete Value 97%


drop_na(flights)

df <- drop_na(flights)
head(df, 5)

# # 1. Top 5 flights in April 2013 ?


glimpse(df)

df %>%
    filter(month == 4) %>%
    count(carrier) %>%
    arrange(desc(n))
    head(5)

topFiveApril <- df %>%
    filter(month == 4) %>%
    count(carrier) %>%
    arrange(desc(n)) %>%
    head(5)

df %>%
    select(carrier, distance, origin, dest) %>%
    distinct(carrier , distance, origin, dest) %>%
    arrange(desc(distance)) %>%
    head(10)

topTenLongest <- df %>%
    select(carrier, distance, origin, dest) %>%
    distinct(carrier, distance, origin, dest) %>%
    arrange(desc(distance)) %>%
    head(10)

# ## Q3 : Top 5 destination flights in 2013 ?


df %>%
    count(dest) %>%
    arrange(desc(n)) %>%
    head(5)

topFiveDest <- df %>%
    count(dest) %>%
    arrange(desc(n)) %>%
    head(5)

# # Q4 : Top 5 months arrivals delayed ?


df %>%
    filter(arr_delay > 0) %>%
    group_by(month) %>%
    select(month, arr_delay) %>%
    summarise(num_delay = n()) %>%
    arrange(desc(num_delay)) %>%
    head(5)

topFiveDelay <- df %>%
    filter(arr_delay > 0) %>%
    group_by(month) %>%
    select(month, arr_delay) %>%
    summarise(num_delay = n()) %>%
    arrange(desc(num_delay)) %>%
    head(5)

df %>%
    group_by(carrier) %>%
    select(carrier, air_time) %>%
    summarise(air_time = n()) %>%
    arrange(air_time)
    head(5)

topFiveLow <- df %>%
    group_by(carrier) %>%
    select(carrier, air_time) %>%
    summarise(air_time = n()) %>%
    arrange(air_time) %>%
    head(5)

df %>%
    filter(between(month ,3,4), dep_delay > 0 ) %>%
    group_by(carrier) %>%
    summarise(avg_delay = mean(dep_delay)) %>%
    arrange(avg_delay)

avgDelay <- df %>%
    filter(between(month ,3,4), dep_delay > 0) %>%
    group_by(carrier) %>%
    summarise(avg_delay = mean(dep_delay)) %>%
    arrange(avg_delay)





