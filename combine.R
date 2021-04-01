library(readr)
library(dplyr)
library(ggplot2)
theme_set(theme_minimal())
library(stringr)

# Data ----
nfl <- read_csv("Data/NFLCombine.csv")

# Exploratory Data Analysis ----
nfl %>% skimr::skim()
nfl %>% count(drafted)

drafted_list <- nfl$drafted %>% str_split(pattern = "/")
drafted_list