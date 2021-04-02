library(readr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
theme_set(theme_minimal())
library(stringr)
library(forcats)
library(FactoMineR)
library(factoextra)

# Data ----
nfl <- read_csv("Data/NFLCombine.csv")

# More Cleaning
nfl$school <- nfl$school %>% str_replace(pattern = "St",replacement = "ate")
nfl$school %>% str_replace(pattern = "atete",replacement = "State")

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

# Offense-Defense
nfl <- nfl %>% 
  mutate(side = ifelse(position %in% c("WR","OT","RB","OG","TE","C","FB","QB","LS","OL"), "Offense", "Defense"))
# - offense: 1426
nfl %>% 
  filter(side == "Offense") %>% 
  count()
# - 1459
nfl %>% 
  filter(side == "Defense") %>% 
  count()

# Conference
nfl <- nfl %>% 
  mutate(conference = case_when(
    school %in% c("Northwestern", "Iowa", "Wisconsin", "Minnesota","Nebraska","Purdue","Illinois","Ohio State","Indiana","Penn St.","Maryland","Rutgers","Michigan","Michigan St.") ~ "Big 10",
    school %in% c("Iowa State", "Oklahoma","Oklahoma St.","Texas","TCU","West Virginia", "Kansas State", "Texas Tech", "Baylor","Kansas") ~ "Big 12",
    school %in% c("Cincinnati", "Tulsa", "Memphis", "UCF", "SMU", "Houston", "Navy", "Tulane", "East Carolina", "Temple", "South Florida") ~ "American Athletic Conference",
    school %in% c("Notre Dame","Clemson","Miami","North Carolina", "North Carolina St.", "Boston College","Pittsburgh","Virginia Tech","Virginia","Wake Forrest","Georgia Tech","Louisville","Florida State","Duke","Syracuse") ~ "Atlantic Coast",
    school %in% c("Marshall", "Florida Atlantic","Western Kentucky","Charlotte","Old Dominion","Middle Tennesse","Florida International","Texas-San Antonio","North Texas","Rice","Southern Miss","Texas-El Paso") ~ "Conference USA",
    school %in% c("USC","Colorado","Utah","Arizona State","UCLA","Arizona","Washington","Oregon","Stanford","Oregon State","Washington St.","California") ~ "Pac-12",
    school %in% c("Alabama","Texas A&M","Auburn","LSU","Mississippi","Arkansas","MS State","Florida","Georgia","Missouri","Kentucky","Tennessee", "South Carolina","Vanderbilt") ~ "Southeastern",
    school %in% c("Buffalo", "Kent State","Ohio","Miami OH","Akron","Bowling Green","Ball State","Western Michigan", "Toledo","Central Michigan","Eastern Michigan","Northern Illinois") ~ "Mid-American",
    school %in% c("San Jose St.", "Boise St.","Nevada","San Diego St.","Hawaii","Fresno St.", "Air Force","Wyoming","New Mexico","Colorado State","Utah State","UNLV") ~ "Mountain West",
    school %in% c("North Dakota St.") ~ "Missouri Valley"
  ))

# Exploratory Data Analysis: Offense ----
nfl %>% 
  filter(side =="Offense") %>% 
  count(drafted)
# - note: 874 drafted

# Position
gg_offense_Position <- nfl %>%
  filter(side == "Offense",
         drafted == "Yes") %>% 
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
gg_offense_School <- nfl %>%
  filter(side == "Offense",
         drafted == "Yes") %>% 
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
gg_offense_Team <- nfl %>%
  filter(side == "Offense",
         drafted == "Yes") %>% 
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

# Correlation Matrix
gg_offense_cor <- nfl %>% 
  filter(side == "Offense") %>% 
  select(height, weight, forty, vertical, bench, broad_jump, three_cone, shuttle) %>% 
  cor() %>% ggcorrplot::ggcorrplot(type = "lower", lab = TRUE) +
  labs(
    subtitle = "Offense"
  ) + 
  theme(
    plot.subtitle = element_text(hjust = 0.5, color = "red")
  )


#
# Exploratory Data Analysis: Defense ----
nfl %>% 
  filter(side == "Defense") %>% 
  count(drafted)
# - note: 1013 drafted

# Position
gg_defense_Position <- nfl %>%
  filter(side == "Defense",
         drafted == "Yes") %>% 
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
gg_defense_School <- nfl %>%
  filter(side == "Defense",
         drafted == "Yes") %>% 
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
gg_defense_Team <- nfl %>%
  filter(side == "Defense",
         drafted == "Yes") %>% 
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


# Exploratory Data Analysis: Principal Componenet Analysis ----
nfl_PCA <- nfl %>% 
  select(drafted, player, height, weight, forty, vertical, bench, broad_jump, three_cone, shuttle) %>% 
  PCA(graph = FALSE, scale.unit = TRUE,
      # Target: Drafted
      quali.sup = c(1,2) 
      )

# Eigenvalues
nfl_PCA %>% 
  fviz_eig(addlabels = TRUE, ylim = c(0,70)) +
  labs(
    title = "Eigenvalues",
    x = "Principal Components",
    y = element_blank()
  ) +
  theme_minimal() +
  theme(
     axis.text.y = element_blank()
  )

# Principal Component 1
nfl_PCA %>% 
  fviz_contrib(choice = "var", axes = 1) + 
  theme_minimal() +
  theme(
    plot.background = element_blank()
  )
# Principal Component 2
nfl_PCA %>% 
  fviz_contrib(choice = "var", axes = 2) + 
  theme_minimal() +
  theme(
    plot.background = element_blank()
  )

# BiPlot
nfl_PCA %>% fviz_pca_var(alpha.var = "cos2")

# - Side
nfl_PCA %>% 
  fviz_pca_biplot(
  geom = "point", pointshape = 21, fill.ind = "gray", col.ind = nfl$side,
  col.var = "black", alpha.var = "cos2",
  palette = c("blue","red"),
  legend.title = "", mean.point = FALSE
) +
  labs(
    title = "Side",
    x = element_blank(),
    y = element_blank(),
    alpha = "Quality of Representation"
  ) +
  theme(
    plot.background = element_blank(),
    legend.position = "top"
  )

# - Position
nfl_PCA %>% 
  fviz_pca_biplot(
    geom = "point", pointshape = 21, fill.ind = "gray", col.ind = nfl$position,
    col.var = "black", alpha.var = "cos2",
    palette = "Spectral",
    legend.title = "", mean.point = FALSE
  ) +
  labs(
    title = "Side",
    x = element_blank(),
    y = element_blank(),
    alpha = "Quality of Representation"
  ) +
  theme(
    plot.background = element_blank(),
    legend.position = "top"
  )

# - Round
nfl_PCA %>% 
  fviz_pca_biplot(
    geom = "point", pointshape = 21, fill.ind = "gray", col.ind = nfl$round,
    col.var = "black", alpha.var = "cos2",
    palette = "Set1",
    legend.title = "", mean.point = FALSE
  ) +
  labs(
    title = "Round",
    x = element_blank(),
    y = element_blank(),
    alpha = "Quality of Representation"
  ) +
  theme(
    plot.background = element_blank(),
    legend.position = "top"
  )

# - Drafted









