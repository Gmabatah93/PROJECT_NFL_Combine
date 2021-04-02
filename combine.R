library(readr)
library(dplyr)
library(ggplot2)
theme_set(theme_minimal())
library(stringr)

# Data ----
nfl <- read_csv("Data/NFLCombine.csv")
# - clean: Draft Variables
# -- Spilt to Columns: Team | Round | Pick | Draft_Year
draft <- nfl$drafted %>% str_split(pattern = "/", simplify = TRUE) %>% as_tibble() %>% 
  select(team = V1, round = V2, pick = V3, draft_year = V4)
# -- draft: team
draft$team <- draft$team %>% str_trim(side = "right")
# -- draft: round
draft$round <- draft$round %>% str_remove_all(pattern = " ")
# -- draft: pick
draft$pick <- draft$pick %>% str_remove(pattern = " ")
draft$pick <- draft$pick %>% str_remove(pattern = "st pick")
draft$pick <- draft$pick %>% str_remove(pattern = "nd pick")
draft$pick <- draft$pick %>% str_remove(pattern = "rd pick")
draft$pick <- draft$pick %>% str_remove(pattern = "th pick")
draft$pick <- draft$pick %>% str_remove(pattern = " ")
# -- draft: year
draft$draft_year <- draft$draft_year %>% str_trim(side = "left")
# -- Draft: NAs
draft <- draft %>% 
  mutate(round = ifelse(round == "", NA, round),
         pick = ifelse(pick == "", NA, pick),
         draft_year = ifelse(draft_year == "", NA, draft_year))

# Add to NFL dataset & change DataTypes
nfl <- nfl %>% 
  bind_cols(draft) %>% 
  mutate(position = position %>% factor,
         year = year %>% factor,
         drafted = ifelse(is.na(drafted), "No","Yes") %>% factor,
         round = round %>% factor,
         draft_year = draft_year %>% factor)
  

# Exploratory Data Analysis ----
nfl %>% skimr::skim()
nfl %>% count(draft_year)

