# Assignment 3: Incarceration

rm(list = ls())

# Set working directory
setwd("~/Documents/Info201code/a3-ellebasy")

# Load needed packages
library("dplyr")
library(tidyverse)
library(ggplot2)
library(maps)

# Read in the data set
incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# Summary Statistics

# What year was the women prison population the highest?
women_prison_year_totals <- incarceration %>%
  group_by(year) %>%
  summarize(female_prison_pop = sum(female_prison_pop, na.rm = TRUE),
            female_jail_pop = sum(female_jail_pop, na.rm = TRUE)) %>%
  subset(female_prison_pop > 0)

women_prison_year_max <- women_prison_year_totals %>%
  filter(female_prison_pop == max(female_prison_pop)) %>%
  pull(year)

women_prison_year_race <- incarceration %>%
  group_by(year) %>%
  summarize(white_female_prison_pop = sum(white_female_prison_pop, na.rm = TRUE),
            black_female_prison_pop = sum(black_female_prison_pop, na.rm = TRUE),
            aapi_female_prison_pop = sum(aapi_female_prison_pop, na.rm = TRUE),
            latinx_female_prison_pop = sum(latinx_female_prison_pop, na.rm = TRUE),
            native_female_prison_pop = sum(native_female_prison_pop, na.rm = TRUE)) %>%
  subset(white_female_prison_pop > 0)

#   What state hold the most total prison population for women?
women_prison_pop <- incarceration %>%
  group_by(state) %>%
  summarize(female_prison_pop = max(female_prison_pop, na.rm = TRUE)) 

is.na(women_prison_pop) <- sapply(women_prison_pop, is.infinite)

women_prison_pop_max <- women_prison_pop_max %>%
  filter(female_prison_pop == max(female_prison_pop, na.rm = TRUE)) %>%
  pull(state)

women_prison_pop_min <- women_prison_pop_max %>%
  filter(female_prison_pop == min(female_prison_pop, na.rm = TRUE)) %>%
  pull(state)



#   What is the racial breakdown of women within the state?
women_prison_race <- incarceration %>%
  filter(state == women_prison_pop_max) %>%
  select(county_name, white_female_prison_pop, black_female_prison_pop, aapi_female_prison_pop, 
         latinx_female_prison_pop, native_female_prison_pop, other_race_female_prison_pop) %>%
  summarize(white_female_prison_pop = sum(white_female_prison_pop, na.rm = TRUE),
            black_female_prison_pop = sum(black_female_prison_pop, na.rm = TRUE),
            aapi_female_prison_pop = sum(aapi_female_prison_pop, na.rm = TRUE),
            latinx_female_prison_pop = sum(latinx_female_prison_pop, na.rm = TRUE),
            native_female_prison_pop = sum(native_female_prison_pop, na.rm = TRUE))

women_prison_race <- as.character(women_prison_race[1, ])
print(women_prison_race)


# Time Trend Chart 
# Line graph with a line represent different races of women 
time_trend <- ggplot(women_prison_year_race, aes(x = year)) +
  geom_line(aes(y = white_female_prison_pop, color = "White")) +
  geom_line(aes(y = black_female_prison_pop, color = "Black")) +
  geom_line(aes(y = aapi_female_prison_pop, color = "Aapi")) +
  geom_line(aes(y = latinx_female_prison_pop, color = "Lantinx")) +
  geom_line(aes(y = native_female_prison_pop, color = "Native")) +
  labs(title = "Total Women Prison Population By Race Overtime", x = "Year", 
       y = "Total Population", color = "Race") 

time_trend

# Variable Comparison Chart
comparison_chart <- ggplot(incarceration, 
                           aes(x = female_jail_pop, y = female_prison_pop)) +
  geom_point(color = "darkslateblue") +
  geom_point()
  labs(title = "Women's Jail vs. Prison Populations", x = "Women Jail Population", y = "Women Prison Population")

comparison_chart

comparison_chart2 <- ggplot(incarceration, 
                           aes(x = white_female_prison_pop, y = black_female_prison_pop)) +
  geom_point(color = "darkslateblue") +
  labs(title = "White vs. Black Women Populations in Prison", x = "White Women Population", y = "Black Women Population")

comparison_chart2

# Map Visual


# Creates a blank theme
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )



