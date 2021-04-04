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
nfl$school <- nfl$school %>% str_replace(pattern = "Statephen", replacement = "Stephen")
nfl$school <- nfl$school %>% str_replace(pattern = "Statellman", replacement = "Stillman")

#
nfl$school <- nfl$school %>% str_replace(pattern = "Southern California", replacement = "USC")
nfl$school <- nfl$school %>% str_replace(pattern = "Texas Christian", replacement = "TCU")
nfl$school <- nfl$school %>% str_replace(pattern = "Louisiana State", replacement = "LSU")
nfl$school <- nfl$school %>% str_replace(pattern = "Brigham Young", replacement = "BYU")
nfl$school <- nfl$school %>% str_replace(pattern = "Southern Methodist", replacement = "SMU")

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



# Data: Feature Engineering ----

# - Change the old drafted variable to a tag
nfl <- nfl %>% 
  mutate(drafted = ifelse(is.na(drafted), "No","Yes") %>% factor)

# - Offense-Defense
nfl <- nfl %>% 
  mutate(side = ifelse(position %in% c("WR","OT","RB","OG","TE","C","FB","QB","LS","OL"), "Offense", "Defense"))

# - Conference
nfl <- nfl %>% 
  mutate(conference = case_when(
    # Division I-A
    school %in% c("Alabama","Texas A&M","Auburn","LSU","Mississippi","Arkansas","Mississippi State","Florida","Georgia","Missouri","Kentucky","Tennessee", "South Carolina","Vanderbilt") ~ "Division I-A (SEC)",
    school %in% c("Notre Dame","Clemson","Miami (FL)","Miami","North Carolina", "North Carolina State", "Boston College","Pittsburgh","Virginia Tech","Virginia","Wake Forest","Georgia Tech","Louisville","Florida State","Duke","Syracuse") ~ "Division I-A (ACC)",
    school %in% c("Northwestern", "Iowa", "Wisconsin", "Minnesota","Nebraska","Purdue","Illinois","Ohio State","Indiana","Penn State","Maryland","Rutgers","Michigan","Michigan State") ~ "Division I-A (Big 10)",
    school %in% c("Iowa State", "Oklahoma","Oklahoma State","Texas","TCU","West Virginia", "Kansas State", "Texas Tech", "Baylor","Kansas") ~ "Division I-A (Big 12)",
    school %in% c("USC","Colorado","Utah","Arizona State","UCLA","Arizona","Washington","Oregon","Stanford","Oregon State","Washington State","California") ~ "Division I-A (Pac-12)",
    school %in% c("Cincinnati", "Tulsa", "Memphis", "Central Florida", "SMU", "Houston", "Navy", "Tulane", "East Carolina", "Temple", "South Florida","Connecticut") ~ "Division I-A (American)",
    school %in% c("Appalachian State","Louisiana-Lafayette","Georgia State","South Alabama","Troy","Louisiana","Texas State", "Arkansas State","Coastal Carolina","Georgia Southern","Louisiana-Monroe") ~ "Division I-A (Sunbelt)",
    school %in% c("San Jose State", "Boise State","Nevada","San Diego State","Hawaii","Fresno State", "Air Force","Wyoming","New Mexico","Colorado State","Utah State","UNLV") ~ "Division I-A (Mountain West)",
    school %in% c("Buffalo", "Kent State","Ohio","Miami (OH)","Akron","Bowling Green","Ball State","Western Michigan", "Toledo","Central Michigan","Eastern Michigan","Northern Illinois") ~ "Division I-A (MAC)",
    school %in% c("Marshall", "Florida Atlantic","Western Kentucky","Charlotte","Old Dominion","Middle Tennessee State","Florida International","Texas-San Antonio","North Texas","Rice","Southern Miss","Texas-El Paso","Louisiana Tech","Alabama-Birmingham") ~ "Division I-A (Conference USA)",
    # Division I-AA
    school %in% c("North Dakota State", "South Dakota State", "Illinois State", "Youngstown State","Northern Iowa","Southern Illinois", "South Dakota","Missouri State","North Dakota","Western Illinois","Stephen F. Austin","McNeese State",
                  "Abilene Christian","Weber State","Eastern Washington", "Southern Utah","Montana", "Montana State","Cal Poly","Portland State","Idaho","	California-Davis","Northern Colorado","California-Davis","Idaho State","Sacramento State",
                  "Jacksonville State", "Eastern Kentucky","Tennessee State","Tennessee-Martin", "Tennessee Tech","Southeast Missouri","New Mexico State","BYU","Massachusetts","Wagner","Charleston Southern","Presbyterian","Hampton","Monmouth (NJ)",
                  "Bucknell", "Lafayette","Lehigh","Fordham","Villanova","William & Mary","Delaware","James Madison","Towson","Elon", "New Hampshire","Richmond","Hofstra","Northeastern (MA)","Maine","Chattanooga","Samford","Furman","Citadel",
                  "Stillman","Harvard","Princeton","Cornell","Brown","South Carolina State", "North Carolina Central", "Norfolk State","Florida A&M","Howard","Bethune-Cookman","North Carolina A&T","Morgan State","Savannah State","Central Arkansas",
                  "Southeastern Louisiana", "Northwestern State","Nicholls State","Sam Houston State","Arkansas-Pine Bluff","Alabama A&M","Alcorn State","Jackson State","Alabama State","Grambling State") ~ "Division I-AA",
    # Divsion II & III
    school %in% c("Humboldt State","Indiana (PA)","West Georgia","Lindenwood","Kutztown","Ashland","Wayne State (MI)","Grand Valley State","Harding","Hobart & William Smith","Newberry","Northwest Missouri State","Bloomsburg",
                  "Saginaw Valley State","Shepherd","Missouri Western","Concordia (MN)","West Texas A&M","Chadron State","Glenville State","Valdosta State","Missouri Southern","California (PA)","Midwestern State",
                  "Slippery Rock","Hillsdale","Mount Union","Southern Arkansas","Hartwick","Liberty","West Liberty","Whitworth","Nebraska-Omaha","Tarleton State","Michigan Tech","Bowie State","Fort Hays State","Central Missouri",
                  "Minnesota-Morris","Mars Hill","Augustana (SD)","Clarion","Winston-Salem State","Texas A&M-Kingsville","Northeastern State","Azusa Pacific") ~ "Division II & III"
  ))


nfl <- nfl %>% 
  select(player, side, position, school, conference, year, everything())

#
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

# Conference
gg_offense_Conference <- nfl %>%
  filter(side == "Offense",
         drafted == "Yes") %>% 
  group_by(conference) %>% count(sort = TRUE) %>% 
  ggplot(aes(n, fct_reorder(conference, n))) +
  geom_col() +
  labs(
    title = "Conference",
    subtitle = "Offense",
    y = "School") +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "red"))
# - first round
nfl %>% 
  filter(side == "Offense",round == "1st") %>%
  group_by(conference) %>% 
  count() %>% 
  ggplot(aes(n, fct_reorder(conference,n))) + 
  geom_col()

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









