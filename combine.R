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

# Clean: "St. & St" to State\
nfl$school <- nfl$school %>% str_replace(pattern = "St.", replacement = "State")
nfl$school <- nfl$school %>% str_replace(pattern = "St", replacement = "State")
nfl$school <- nfl$school %>% str_replace(pattern = "Stateate",replacement = "State")
nfl$school <- nfl$school %>% str_replace(pattern = "Statete",replacement = "State")
nfl$school <- nfl$school %>% str_replace(pattern = "Statenford",replacement = "Stanford")


# clean: Split Draft information "drafted"
# - Spilt to Columns: Team | Round | Pick | Draft_Year
draft <- nfl$drafted %>% str_split(pattern = "/", simplify = TRUE) %>% as_tibble() %>% 
  select(team = V1, round = V2, pick = V3, draft_year = V4)
# - team: remove white space at end
draft$team <- draft$team %>% str_trim(side = "right")
# - round: remove white space
draft$round <- draft$round %>% str_trim(side = "both")
# - pick: remove "pick"
draft$pick <- draft$pick %>% str_remove(pattern = " pick ")
# -- draft_year: remove white space
draft$draft_year <- draft$draft_year %>% str_trim(side = "left")
# - Draft: change "" to NA
draft <- draft %>% 
  mutate(round = ifelse(round == "", NA, round),
         pick = ifelse(pick == "", NA, pick),
         draft_year = ifelse(draft_year == "", NA, draft_year))

# Add Split Draft Info back to Dataframe
nfl <- nfl %>% 
  bind_cols(draft)

# - Change the old drafted variable to a tag
nfl <- nfl %>% 
  mutate(drafted = ifelse(is.na(drafted), "No","Yes") %>% factor)

# - DataType Conversions
nfl <- nfl %>% 
  # Do not need "draft_year" already have "year"
  select(-draft_year) %>% 
  mutate(position = position %>% factor,
         school = school %>% factor,
         year = year %>% factor,
         team = team %>% factor,
         round = round %>% factor,
         pick = pick %>% factor)

# Feature Engineering
# - Offense-Defense
nfl <- nfl %>% 
  mutate(side = ifelse(position %in% c("WR","OT","RB","OG","TE","C","FB","QB","LS","OL"), "Offense", "Defense"))
# - Conference
nfl <- nfl %>% 
  mutate(conference = case_when(
    school %in% c("Northwestern", "Iowa", "Wisconsin", "Minnesota","Nebraska","Purdue","Illinois","Ohio State","Indiana","Penn State","Maryland","Rutgers","Michigan","Michigan State") ~ "Big 10",
    school %in% c("Iowa State", "Oklahoma","Oklahoma State","Texas","TCU","West Virginia", "Kansas State", "Texas Tech", "Baylor","Kansas") ~ "Big 12",
    school %in% c("Cincinnati", "Tulsa", "Memphis", "UCF", "SMU", "Houston", "Navy", "Tulane", "East Carolina", "Temple", "South Florida") ~ "American Athletic Conference",
    school %in% c("Notre Dame","Clemson","Miami (FL)","North Carolina", "North Carolina State", "Boston College","Pittsburgh","Virginia Tech","Virginia","Wake Forrest","Georgia Tech","Louisville","Florida State","Duke","Syracuse") ~ "Atlantic Coast",
    school %in% c("Marshall", "Florida Atlantic","Western Kentucky","Charlotte","Old Dominion","Middle Tennesse","Florida International","Texas-San Antonio","North Texas","Rice","Southern Miss","Texas-El Paso") ~ "Conference USA",
    school %in% c("USC","Colorado","Utah","Arizona State","UCLA","Arizona","Washington","Oregon","Stanford","Oregon State","Washington State","California") ~ "Pac-12",
    school %in% c("Alabama","Texas A&M","Auburn","LSU","Mississippi","Arkansas","MS State","Florida","Georgia","Missouri","Kentucky","Tennessee", "South Carolina","Vanderbilt") ~ "Southeastern",
    school %in% c("Buffalo", "Kent State","Ohio","Miami OH","Akron","Bowling Green","Ball State","Western Michigan", "Toledo","Central Michigan","Eastern Michigan","Northern Illinois") ~ "Mid-American",
    school %in% c("San Jose State", "Boise State","Nevada","San Diego State","Hawaii","Fresno State", "Air Force","Wyoming","New Mexico","Colorado State","Utah State","UNLV") ~ "Mountain West",
    school %in% c("Fordham") ~ "Atlantic 10",
    school %in% c("Maine") ~ " America East Conference",
    school %in% c("North Dakota State") ~ "Missouri Valley"
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









