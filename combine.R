library(readr)
library(dplyr)
library(ggplot2)
theme_set(theme_minimal())
library(stringr)
library(forcats)

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

# Offense Defense
# - 1426
nfl_offense <- nfl %>% 
  filter(position %in% c("WR","OT","RB","OG","TE","C","FB","QB","LS","OL"))
# - 1459
nfl_defense <- nfl %>%
  filter(position %in% c("CB","DE","DT","OLB","ILB","FS","SS","EDGE","S","LB"))

# Exploratory Data Analysis: Offense ----
nfl_offense %>% count(drafted)
# - note: 874 drafted

# Position
gg_offense_Position <- nfl_offense %>%
  filter(drafted == "Yes") %>% 
  group_by(position) %>% count() %>% 
  ggplot(aes(n, fct_reorder(position, n))) +
  geom_col() +
  labs(
    title = "Position: Who got Drafted ?",
    subtitle = "Offense",
    y = "Position") + 
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "red"))

# School
gg_offense_School <- nfl_offense %>%
  filter(drafted == "Yes") %>% 
  group_by(school) %>% count(sort = TRUE) %>% 
  filter(n > 10) %>% 
  ggplot(aes(n, fct_reorder(school, n))) +
  geom_col() +
  labs(
    title = "School: Who got Drafted ?",
    subtitle = "Offense",
    y = "School") +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "red"))

# Team
gg_offense_Team <- nfl_offense %>%
  filter(drafted == "Yes") %>% 
  group_by(team) %>% count(sort = TRUE) %>% 
  ggplot(aes(n, fct_reorder(team, n))) +
  geom_col() +
  labs(
    title = "Team:",
    subtitle = "Offense",
    y = "Team") +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "red"))


# Exploratory Data Analysis: Defense ----
nfl_defense %>% count(drafted)
# - note: 1013 drafted

# Position
gg_defense_Position <- nfl_defense %>%
  filter(drafted == "Yes") %>% 
  group_by(position) %>% count() %>% 
  ggplot(aes(n, fct_reorder(position, n))) +
  geom_col() +
  labs(
    title = "Position: Who got Drafted ?",
    subtitle = "Defense",
    y = "Position") + 
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "blue"))

# School
gg_defense_School <- nfl_defense %>%
  filter(drafted == "Yes") %>% 
  group_by(school) %>% count(sort = TRUE) %>% 
  filter(n > 10) %>% 
  ggplot(aes(n, fct_reorder(school, n))) +
  geom_col() +
  labs(
    title = "School: Who got Drafted ?",
    subtitle = "Defense",
    y = "School") +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "blue"))

# Team
gg_defense_Team <- nfl_defense %>%
  filter(drafted == "Yes") %>% 
  group_by(team) %>% count(sort = TRUE) %>% 
  ggplot(aes(n, fct_reorder(team, n))) +
  geom_col() +
  labs(
    title = "Team:",
    subtitle = "Defense",
    y = "Team") +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "blue"))
