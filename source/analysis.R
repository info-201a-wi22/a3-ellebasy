# Assignment 3: Incarceration

# Set up
setwd("~/Documents/Info201code/a3-ellebasy")
library("dplyr")
library(tidyverse)
library(ggplot2)
library(maps)

# Load in the data set
df <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# Summary Statistics
# How does the women prison population compare to the men prison population?
men_prison_pop <- df %>% 
  summarize(male_prison_pop = mean(male_prison_pop, na.rm = TRUE)) %>%
  pull(male_prison_pop)
  
women_prison_pop <- df %>%
  summarize(female_prison_pop = mean(female_prison_pop, na.rm = TRUE)) %>%
  pull(female_prison_pop)

men_women_pop_ratio <- men_prison_pop / women_prison_pop

# What year has the highest recorded population of women in prisons?
women_prison_pop_yearly <- df %>%
  group_by(year) %>%
  summarize(female_prison_pop = sum(female_prison_pop, na.rm = TRUE)) %>%
  subset(female_prison_pop > 0)

women_prison_pop_max_total <- women_prison_pop_yearly %>%
  filter(female_prison_pop == max(female_prison_pop)) %>%
  pull(female_prison_pop)

women_prison_year_max <- women_prison_pop_yearly %>%
  filter(female_prison_pop == max(female_prison_pop)) %>%
  pull(year)

# What race of women are the most populated in prison?
women_prison_pop_race_2013 <- df %>%
  filter(year == women_prison_year_max) %>%
  summarize(white_female_prison_pop = sum(white_female_prison_pop, na.rm = TRUE),
            black_female_prison_pop = sum(black_female_prison_pop, na.rm = TRUE),
            aapi_female_prison_pop = sum(aapi_female_prison_pop, na.rm = TRUE),
            latinx_female_prison_pop = sum(latinx_female_prison_pop, na.rm = TRUE),
            native_female_prison_pop = sum(native_female_prison_pop, na.rm = TRUE)) %>%
  subset(white_female_prison_pop > 0) %>%
  gather(key = "race", value = "female_prison_pop_race")

women_prison_race_max <- women_prison_pop_race_2013 %>%
  filter(female_prison_pop_race == max(female_prison_pop_race)) %>%
  pull(race)

women_prison_race_max_total <- women_prison_pop_race_2013 %>%
  filter(female_prison_pop_race == max(female_prison_pop_race)) %>%
  pull(female_prison_pop_race)

# What race of women are the least populated in prison?
women_prison_pop_race_min <- women_prison_pop_race_2013 %>%
  filter(female_prison_pop_race == min(female_prison_pop_race)) %>%
  pull(race)

women_prison_race_min_total <- women_prison_pop_race_2013 %>%
  filter(female_prison_pop_race == min(female_prison_pop_race)) %>%
  pull(female_prison_pop_race)

# Difference between prison race population of the highest and lowest race
pop_race_difference <- women_prison_race_max_total - women_prison_race_min_total

# How populated are black women in prison?
black_women_pop_2013 <- women_prison_pop_race_2013 %>%
  filter(race == "black_female_prison_pop") %>%
  pull(female_prison_pop_race)

# What state holds the highest average prison population for women?
women_prison_pop <- df %>%
  group_by(state) %>%
  summarize(female_prison_pop = max(female_prison_pop, na.rm = TRUE)) 
  
is.na(women_prison_pop) <- sapply(women_prison_pop, is.infinite)

women_pop_max_state <- women_prison_pop %>%
  filter(female_prison_pop == max(female_prison_pop, na.rm = TRUE)) %>%
  pull(state)

women_pop_max_state_total <- women_prison_pop %>%
  filter(female_prison_pop == max(female_prison_pop, na.rm = TRUE)) %>%
  pull(female_prison_pop)

# Time Trend Chart 
# Data frame of total women prison populations for every race for evey year
women_prison_pop_year_race <- df %>%
  group_by(year) %>%
  summarize(white_female_prison_pop = sum(white_female_prison_pop, na.rm = TRUE),
            black_female_prison_pop = sum(black_female_prison_pop, na.rm = TRUE),
            aapi_female_prison_pop = sum(aapi_female_prison_pop, na.rm = TRUE),
            latinx_female_prison_pop = sum(latinx_female_prison_pop, na.rm = TRUE),
            native_female_prison_pop = sum(native_female_prison_pop, na.rm = TRUE)) %>%
  subset(white_female_prison_pop > 0)

# Creates a line chart to show trend over time
time_trend <- ggplot(women_prison_pop_year_race, aes(x = year)) +
  geom_line(aes(y = white_female_prison_pop, color = "White")) +
  geom_line(aes(y = black_female_prison_pop, color = "Black")) +
  geom_line(aes(y = aapi_female_prison_pop, color = "Aapi")) +
  geom_line(aes(y = latinx_female_prison_pop, color = "Lantinx")) +
  geom_line(aes(y = native_female_prison_pop, color = "Native")) +
  labs(title = "Total Women Prison Population By Race Overtime", x = "Year", 
       y = "Total Population", color = "Race") 

# Variable Comparison Chart
# Creates data frame to compare the average white to black women population rates
# in prison for each state
white_black_women_pop <- df %>%
  filter(year == 2013) %>%
  group_by(state) %>%
  summarize(white_female_prison_pop = mean(white_female_prison_pop, na.rm = TRUE),
            black_female_prison_pop = mean(black_female_prison_pop, na.rm = TRUE))

# Creates a scatterplot for the above dataframe
comparison_chart <- ggplot(white_black_women_pop, aes(x = white_female_prison_pop,
                                                      y = black_female_prison_pop)) +
  geom_point(color = "darkslateblue") +
  labs(title = "White vs. Black Women Prison Population in 2013", x = "White Women Population", 
       y = "Black Women Population")

# Map Visual

# Create a blank theme
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

# Finding the year with the highest prison population of black women
black_women_max_year <-  df %>%
  group_by(year) %>%
  summarize(black_female_prison_pop = sum(black_female_prison_pop, na.rm = TRUE)) %>%
  filter(black_female_prison_pop == max(black_female_prison_pop)) %>%
  pull(year)

# Filtering out the data set for only 2009 California results
black_women_ca_counties <- df %>%
  filter(year == 2009 & state == "CA") %>%
  drop_na(black_female_prison_pop) 

# Gather county data, longitudes & latitudes, etc. then merges with the above data frame.
county_shapes <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname") %>%
  left_join(black_women_ca_counties, by = "fips") %>%
  filter(state == "CA")

# Plots out the map chart
map_chart <- ggplot(county_shapes) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = black_female_prison_pop),
               color = "black", size = .1) +
  coord_map() + 
  scale_fill_continuous(low = "skyblue3", high = "purple") +
  labs( title = "Total Black Women Prison Population: California 2009",
        fill = "Black Women Prison Population") +
  blank_theme

