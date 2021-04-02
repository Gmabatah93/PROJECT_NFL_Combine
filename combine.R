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









