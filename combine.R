library(tidymodels)
library(stringr)
library(forcats)
library(RColorBrewer)
theme_set(theme_minimal())
library(ggmosaic)
library(ggpubr)
library(FactoMineR)
library(factoextra)
library(workflowsets)
library(doParallel)

# Data ----
nfl_raw <- readr::read_csv("Data/NFLCombine.csv")

# Clean: "St. & St" to State\
nfl_raw$school <- nfl_raw$school %>% str_replace(pattern = "St.", replacement = "State")
nfl_raw$school <- nfl_raw$school %>% str_replace(pattern = "St", replacement = "State")
nfl_raw$school <- nfl_raw$school %>% str_replace(pattern = "Stateate",replacement = "State")
nfl_raw$school <- nfl_raw$school %>% str_replace(pattern = "Statete",replacement = "State")
nfl_raw$school <- nfl_raw$school %>% str_replace(pattern = "Statenford",replacement = "Stanford")
nfl_raw$school <- nfl_raw$school %>% str_replace(pattern = "Statephen", replacement = "Stephen")
nfl_raw$school <- nfl_raw$school %>% str_replace(pattern = "Statellman", replacement = "Stillman")

#
nfl_raw$school <- nfl_raw$school %>% str_replace(pattern = "Southern California", replacement = "USC")
nfl_raw$school <- nfl_raw$school %>% str_replace(pattern = "Texas Christian", replacement = "TCU")
nfl_raw$school <- nfl_raw$school %>% str_replace(pattern = "Louisiana State", replacement = "LSU")
nfl_raw$school <- nfl_raw$school %>% str_replace(pattern = "Brigham Young", replacement = "BYU")
nfl_raw$school <- nfl_raw$school %>% str_replace(pattern = "Southern Methodist", replacement = "SMU")

# clean: Split Draft information "drafted"
# - Spilt to Columns: Team | Round | Pick | Draft_Year
draft <- nfl_raw$drafted %>% str_split(pattern = "/", simplify = TRUE) %>% as_tibble() %>% 
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
nfl_draft <- nfl_raw %>% 
  bind_cols(draft)

# - DataType Conversions
nfl_draft <- nfl_draft %>% 
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
nfl_draft <- nfl_draft %>% 
  mutate(drafted = ifelse(is.na(drafted), "No","Yes") %>% factor)

# - Offense-Defense
nfl_draft <- nfl_draft %>% 
  mutate(side = ifelse(position %in% c("WR","OT","RB","OG","TE","C","FB","QB","LS","OL"), "Offense", "Defense") %>% factor())

# - Conference
nfl_draft <- nfl_draft %>% 
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
  ) %>% factor())


nfl_draft <- nfl_draft %>% 
  select(player, side, position, school, conference, year, everything()) %>% 
  select(-school, -pick) %>% 
  mutate(round = case_when(
                  round == "1st" ~ "1st",
                  TRUE ~ "Not 1st"
                  ) %>% factor
  )


nfl_draft %>% skimr::skim()

#

# Exploratory Data Analysis: Draft Summary ----

# Target: 
nfl_draft %>% 
  count(drafted) %>% 
  ggplot(aes(drafted, n)) +
  geom_col(aes(fill = drafted)) +
  geom_label(aes(label = n)) +
  labs(title = "Drafted ?") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "bottom"
  ) + 
  scale_fill_manual(values = c("grey80", "forestgreen"))
# - By Side
nfl_draft %>%
  group_by(drafted) %>% 
  count(side) %>% 
  mutate(side = fct_reorder(side, n)) %>% 
  ggplot(aes(side, n, fill = drafted)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("grey80","forestgreen"))
nfl_draft %>%
  group_by(drafted) %>% 
  count(side) %>% 
  mutate(side = fct_reorder(side, n)) %>% 
  ggplot(aes(side, n, fill = drafted)) +
  geom_col(position = "fill") +
  geom_hline(yintercept = 0.66, color = "red") +
  scale_fill_manual(values = c("grey80","forestgreen"))

# - By Position
nfl_draft %>%
  group_by(drafted, side) %>% 
  count(position) %>% 
  mutate(position = fct_reorder(position, n)) %>% 
  ggplot(aes(position, n, fill = drafted)) +
  geom_col(position = "dodge") +
  facet_wrap(~ side, nrow = 2) +
  scale_fill_manual(values = c("grey80","forestgreen"))
nfl_draft %>%
  group_by(drafted, side) %>% 
  count(position) %>% 
  mutate(position = fct_reorder(position, n)) %>% 
  ggplot(aes(position, n, fill = drafted)) +
  geom_col(position = "fill") +
  geom_hline(yintercept = 0.66, color = "red") +
  facet_wrap(~ side, nrow = 2) +
  scale_fill_manual(values = c("grey80","forestgreen"))
# - By Conference
nfl_draft %>%
  group_by(drafted) %>% 
  count(conference) %>% 
  mutate(conference = fct_reorder(conference, n)) %>% 
  ggplot(aes(conference, n, fill = drafted)) +
  geom_col(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("grey80","forestgreen"))
nfl_draft %>%
  group_by(drafted) %>% 
  count(conference) %>% 
  mutate(conference = fct_reorder(conference, n)) %>% 
  ggplot(aes(conference, n, fill = drafted)) +
  geom_col(position = "fill") +
  geom_hline(yintercept = 0.66, color = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("grey80","forestgreen"))

# CAtegorical
nfl_draft %>% 
  ggplot() +
  geom_mosaic(aes(x = product(position), fill=conference)) +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
  
# Corrplot: Combine Stats
nfl_draft %>% 
  select(height:shuttle) %>% cor %>% 
  corrplot::corrplot(method = "number", type = "upper")
# - Weight ~ Forty: 89
gg_Weight_Forty <- nfl_draft %>% 
  ggplot(aes(weight, forty, color = drafted)) +
  geom_point(alpha = 0.3) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    legend.position = "bottom"
  ) +
scale_color_manual(values = c("grey80","forestgreen"))
# - Three Cone ~ Shuttle: 85
gg_3cone_Shuttle <- nfl_draft %>% 
  ggplot(aes(three_cone, shuttle, color = drafted)) +
  geom_point(alpha = 0.3) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    legend.position = "none"
  ) +
  scale_color_manual(values = c("grey80","forestgreen"))
# - Forty ~ Broad Jump: -84
gg_Forty_BJump <- nfl_draft %>% 
  ggplot(aes(forty, broad_jump, color = drafted)) +
  geom_point(alpha = 0.3) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    legend.position = "none"
  ) +
  scale_color_manual(values = c("grey80","forestgreen"))
# - Forty ~ Three Cone: 83
gg_Forty_3cone <- nfl_draft %>% 
  ggplot(aes(forty, three_cone, color = drafted)) +
  geom_point(alpha = 0.3) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    legend.position = "none"
  ) +
  scale_color_manual(values = c("grey80","forestgreen"))
# - Vertical ~ Broad Jump: 82
gg_Vertical_BJump <- nfl_draft %>% 
  ggplot(aes(vertical, broad_jump, color = drafted)) +
  geom_point(alpha = 0.3) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    legend.position = "bottom"
  ) +
  scale_color_manual(values = c("grey80","forestgreen"))
# - Weight ~ Three Cone: 81
gg_Weight_3cone <- nfl_draft %>% 
  ggplot(aes(weight, three_cone, color = drafted)) +
  geom_point(alpha = 0.3) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    legend.position = "none"
  ) +
  scale_color_manual(values = c("grey80","forestgreen"))

# Multiplot
ggarrange(gg_Weight_Forty, gg_3cone_Shuttle, gg_Forty_BJump, 
          gg_Forty_3cone, gg_Vertical_BJump, gg_Weight_3cone, 
          ncol = 3, nrow = 2)
#
# Exploratory Data Analysis: Round Summary ----

# Target
nfl_draft %>% 
  count(round) %>% 
  ggplot(aes(round, n)) +
  geom_col(aes(fill = round)) +
  geom_label(aes(label = n)) +
  labs(title = "1st Round ?") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  ) + 
  scale_fill_manual(values = c("forestgreen","grey80"))
# - By Side
nfl_draft %>%
  group_by(round) %>% 
  count(side) %>% 
  mutate(side = fct_reorder(side, n)) %>% 
  ggplot(aes(side, n, fill = round)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("forestgreen","grey80"))
# - By Position
nfl_draft %>%
  group_by(round, side) %>% 
  count(position) %>% 
  mutate(position = fct_reorder(position, n)) %>% 
  ggplot(aes(position, n, fill = round)) +
  geom_col(position = "dodge") +
  facet_wrap(~ side, nrow = 2) +
  scale_fill_manual(values = c("forestgreen","grey80"))
# - By Conference
nfl_draft %>%
  group_by(round) %>% 
  count(conference) %>% 
  mutate(conference = fct_reorder(conference, n)) %>% 
  ggplot(aes(conference, n, fill = round)) +
  geom_col(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("forestgreen", "grey80"))

# Corrplot: Combine Stats
nfl_draft %>% 
  select(height:shuttle) %>% cor %>% 
  corrplot::corrplot(method = "number", type = "upper")
# - Weight ~ Forty: 89
gg_Weight_Forty <- nfl_draft %>% 
  ggplot(aes(weight, forty, color = round)) +
  geom_point(alpha = 0.3) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    legend.position = "bottom"
  ) +
  scale_color_manual(values = c("forestgreen","grey80"))
# - Three Cone ~ Shuttle: 85
gg_3cone_Shuttle <- nfl_draft %>% 
  ggplot(aes(three_cone, shuttle, color = round)) +
  geom_point(alpha = 0.3) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    legend.position = "none"
  ) +
  scale_color_manual(values = c("forestgreen","grey80"))
# - Forty ~ Broad Jump: -84
gg_Forty_BJump <- nfl_draft %>% 
  ggplot(aes(forty, broad_jump, color = round)) +
  geom_point(alpha = 0.3) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    legend.position = "none"
  ) +
  scale_color_manual(values = c("forestgreen","grey80"))
# - Forty ~ Three Cone: 83
gg_Forty_3cone <- nfl_draft %>% 
  ggplot(aes(forty, three_cone, color = round)) +
  geom_point(alpha = 0.3) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    legend.position = "none"
  ) +
  scale_color_manual(values = c("forestgreen","grey80"))
# - Vertical ~ Broad Jump: 82
gg_Vertical_BJump <- nfl_draft %>% 
  ggplot(aes(vertical, broad_jump, color = round)) +
  geom_point(alpha = 0.3) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    legend.position = "bottom"
  ) +
  scale_color_manual(values = c("forestgreen","grey80"))
# - Weight ~ Three Cone: 81
gg_Weight_3cone <- nfl_draft %>% 
  ggplot(aes(weight, three_cone, color = round)) +
  geom_point(alpha = 0.3) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    legend.position = "none"
  ) +
  scale_color_manual(values = c("forestgreen","grey80"))

# Multiplot
ggarrange(gg_Weight_Forty, gg_3cone_Shuttle, gg_Forty_BJump, 
          gg_Forty_3cone, gg_Vertical_BJump, gg_Weight_3cone, 
          ncol = 3, nrow = 2)

# Categorical
nfl_draft %>%
  filter(side == "Offense") %>% 
  ggplot() +
  geom_mosaic(aes(x = product(position), fill = conference)) +
  theme(axis.text.y = element_blank()) +
  facet_wrap(~round, nrow = 2) +
  theme_bw() +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
    ) 

#
# Exploratory Data Analysis: PCA ----

# PCA Object
nfl_PCA <- nfl_draft %>% 
  select(height:shuttle) %>% 
  PCA(graph = FALSE)
# - Graph
nfl_PCA %>% 
  fviz_pca_biplot(repel = TRUE,
                  arrowsize = 1,
                  col.var = "grey25", alpha.var = "cos2",
                  geom.ind = "point", pointsize = 0.5,
                  col.ind = "grey85", 
                  legend.title = list(alpha = "Quality of Representation"))

# Eigen: 70.1% Explained by PC1
nfl_PCA %>% fviz_eig(addlabels = TRUE)
# - variables:
nfl_PCA$var$coord[,c(1,2)]
nfl_PCA$var$cos2[,c(1,2)]
nfl_PCA$var$contrib[,c(1,2)]
nfl_PCA %>% fviz_contrib(choice = "var", axes = 1)
nfl_PCA %>% fviz_contrib(choice = "var", axes = 2)
nfl_PCA %>% fviz_contrib(choice = "var", axes = c(1,2))


# - PCA: Drafted
nfl_PCA %>% 
  fviz_pca_biplot(repel = TRUE,
                  arrowsize = 1, 
                  col.var = "grey25", alpha.var = "cos2",
                  geom.ind = "point", pointsize = 0.5,
                  col.ind = nfl_draft$drafted, 
                  addEllipses = TRUE, ellispe.type = "norm",
                  palette = c("tomato", "forestgreen"),
                  legend.title = "Drafted")

# - PCA: Round
nfl_PCA %>% 
  fviz_pca_biplot(repel = TRUE,
                  arrowsize = 1, 
                  col.var = "grey25", alpha.var = "cos2",
                  geom.ind = "point", pointsize = 0.5,
                  col.ind = nfl_draft$round, 
                  addEllipses = TRUE, ellispe.type = "norm",
                  legend.title = "Round")

# - PCA: Conference
nfl_PCA %>% 
  fviz_pca_biplot(repel = TRUE,
                  arrowsize = 1, 
                  col.var = "grey25", alpha.var = "cos2",
                  geom.ind = "point", pointsize = 0.5,
                  col.ind = nfl_draft$conference, 
                  addEllipses = TRUE, ellispe.type = "norm",
                  legend.title = "Conference",
                  legend.position = "bottom")

# - PCA: Side
nfl_PCA %>% 
  fviz_pca_biplot(repel = TRUE,
                  arrowsize = 1, 
                  col.var = "grey25", alpha.var = "cos2",
                  geom.ind = "point", pointsize = 0.5,
                  col.ind = nfl_draft$side, 
                  addEllipses = TRUE, ellispe.type = "norm",
                  palette = c("blue","red"),
                  legend.title = "Side")

# - PCA: Position
nfl_PCA%>% 
  fviz_pca_biplot(repel = TRUE,
                  arrowsize = 1, 
                  col.var = "grey25", alpha.var = "cos2",
                  geom.ind = "point", pointsize = 0.5,
                  col.ind = nfl_draft$position, 
                  addEllipses = TRUE, ellispe.type = "norm",
                  legend.title = "Position")


# New Dataset
nfl_df <- nfl_draft %>% 
  select(side, position, conference, weight, forty, broad_jump, bench, drafted, round)

#
# Exploratory Data Analysis: Combine ----
# Combine Data: Weight
nfl_mean_weight <- nfl_df$weight %>% mean
nfl_df %>% 
  ggplot(aes(weight)) +
  geom_histogram() + 
  geom_vline(xintercept = nfl_mean_weight, color = "red")
# - By poisition
nfl_df %>% 
  ggplot(aes(weight, fill = side)) +
  geom_density(alpha = 0.3)
nfl_df %>% 
  ggplot(aes(position, weight, fill = drafted)) +
  geom_boxplot() + geom_hline(yintercept = nfl_mean_weight, color = "red") +
  geom_jitter(alpha = 0.09) +
  facet_wrap(~ side)
# - By Conference
nfl_df %>%
  mutate(cond = case_when(
    conference %in% c("Division I-A (SEC)", "Division I-A (ACC)", "Division I-A (Big 10)", "Division I-A (Big 12)", "Division I-A (Pac-12)") ~ "Elite",
    conference %in% c("Division I-A (American)", "Division I-A (Sunbelt)", "Division I-A (Mountain West)", "Division I-A (MAC)","Division I-A (Conference USA)") ~ "Division I-A",
    conference %in% c("Division I-AA") ~ "Division I-AA",
    conference %in% c("Division II & III") ~ "Division II & III"
  )) %>% 
  ggplot(aes(conference, weight, fill = cond)) +
  geom_boxplot() + geom_hline(yintercept = nfl_mean_weight, color = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combine Data: Forty
nfl_mean_forty <- nfl_draft$forty %>% mean
nfl_df %>% 
  ggplot(aes(forty)) +
  geom_histogram() + 
  geom_vline(xintercept = nfl_mean_forty, color = "red")
# - By poisition
nfl_df %>% 
  ggplot(aes(forty, fill = side)) +
  geom_density(alpha = 0.3)
nfl_df %>% 
  ggplot(aes(position, forty, fill = drafted)) +
  geom_boxplot() + geom_hline(yintercept = nfl_mean_forty, color = "red") +
  geom_jitter(alpha = 0.09) +
  facet_wrap(~ side)
# - By Conference
nfl_df %>%
  mutate(cond = case_when(
    conference %in% c("Division I-A (SEC)", "Division I-A (ACC)", "Division I-A (Big 10)", "Division I-A (Big 12)", "Division I-A (Pac-12)") ~ "Elite",
    conference %in% c("Division I-A (American)", "Division I-A (Sunbelt)", "Division I-A (Mountain West)", "Division I-A (MAC)","Division I-A (Conference USA)") ~ "Division I-A",
    conference %in% c("Division I-AA") ~ "Division I-AA",
    conference %in% c("Division II & III") ~ "Division II & III"
  )) %>% 
  ggplot(aes(conference, forty, fill = cond)) +
  geom_boxplot() + geom_hline(yintercept = nfl_mean_forty, color = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combine Data: Bench
nfl_mean_bench <- nfl_draft$bench %>% mean
nfl_df %>% 
  ggplot(aes(bench)) +
  geom_histogram() + 
  geom_vline(xintercept = nfl_mean_bench, color = "red")
# - By poisition
nfl_df %>% 
  ggplot(aes(bench, fill = side)) +
  geom_density(alpha = 0.3)
nfl_df %>% 
  ggplot(aes(position, bench, fill = side)) +
  geom_boxplot() + geom_hline(yintercept = nfl_mean_bench, color = "red") +
  geom_jitter(alpha = 0.09) 
# - By Conference
nfl_df %>%
  mutate(cond = case_when(
    conference %in% c("Division I-A (SEC)", "Division I-A (ACC)", "Division I-A (Big 10)", "Division I-A (Big 12)", "Division I-A (Pac-12)") ~ "Elite",
    conference %in% c("Division I-A (American)", "Division I-A (Sunbelt)", "Division I-A (Mountain West)", "Division I-A (MAC)","Division I-A (Conference USA)") ~ "Division I-A",
    conference %in% c("Division I-AA") ~ "Division I-AA",
    conference %in% c("Division II & III") ~ "Division II & III"
  )) %>% 
  ggplot(aes(conference, bench, fill = cond)) +
  geom_boxplot() + geom_hline(yintercept = nfl_mean_bench, color = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#

# Exploratory Data Analysis: Offense ----
nfl_df %>% 
  filter(side =="Offense") %>% 
  count(drafted)
# - note: 874 drafted

# STATS: 

# Summary
nfl_df %>% 
  filter(side == "Offense") %>% 
  summarise(Avg_Weight = mean(weight),
            Avg_Forty = mean(forty),
            Avg_BroadJump = mean(broad_jump),
            Avg_Bench = mean(bench))

# By Conference
nfl_offense_stat_Conference <- nfl_df %>% 
  filter(side == "Offense") %>% 
  group_by(conference, drafted, round) %>% 
  summarise(Weight = mean(weight),
            Forty = mean(forty),
            BroadJump = mean(broad_jump),
            Bench = mean(bench)) %>% 
  gather(Weight, Forty, BroadJump, Bench,
         key = "Measure", value = "Stat")
# - drafted
nfl_offense_stat_Conference %>% 
  ggplot(aes(conference, Stat, fill = drafted)) +
  geom_col(position = "dodge") +
  facet_wrap(~Measure, scales = "free_y") +
  labs(title = "Offense-Conference: Stats") +
  theme_bw() + theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 90, hjust = 1)
  ) +
  scale_fill_manual(values = c("grey70","forestgreen"))
# - round
nfl_offense_stat_Conference %>% 
  ggplot(aes(conference, Stat, fill = round)) +
  geom_col(position = "dodge") +
  facet_wrap(~Measure, scales = "free_y") +
  labs(title = "Offense-Conference: Stats") +
  theme_bw() + theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 90, hjust = 1)
  ) +
  scale_fill_manual(values = c("midnightblue","grey70"))

# By Position
nfl_offense_stat_Position <- nfl_df %>% 
  filter(side == "Offense") %>% 
  group_by(position, drafted, round) %>% 
  summarise(Weight = mean(weight),
            Forty = mean(forty),
            BroadJump = mean(broad_jump),
            Bench = mean(bench)) %>% 
  gather(Weight, Forty, BroadJump, Bench,
         key = "Measure", value = "Stat")
# - drafted
nfl_offense_stat_Position %>% 
  ggplot(aes(position, Stat, fill = drafted)) +
  geom_col(position = "dodge") +
  facet_wrap(~Measure, scales = "free_y") +
  labs(title = "Offense-Position: Stats") +
  theme_bw() + theme(
    plot.title = element_text(hjust = 0.5, size = 12)
  ) +
  scale_fill_manual(values = c("grey70","forestgreen"))
# - round
nfl_offense_stat_Position %>% 
  ggplot(aes(position, Stat, fill = round)) +
  geom_col(position = "dodge") +
  facet_wrap(~Measure, scales = "free_y") +
  labs(title = "Offense-Position: Stats") +
  theme_bw() + theme(
    plot.title = element_text(hjust = 0.5, size = 12)
  ) +
  scale_fill_manual(values = c("midnightblue","grey70"))


# Statistical Test
nfl_df %>% 
  filter(side == "Offense") %>% 
  mutate(drafted = ifelse(drafted == "Yes", 1, 0)) %>% 
  glm(drafted ~ weight + forty + broad_jump + bench, family = "binomial", data = .) %>% 
  tidy() %>% 
  mutate(estimate = plogis(estimate),
         p.value = round(p.value,3))

nfl_df %>% 
  filter(side == "Offense") %>% 
  mutate(drafted = ifelse(drafted == "Yes", 1, 0)) %>% 
  glm(drafted ~ conference + weight + forty + broad_jump + bench, family = "binomial", data = .) %>% 
  tidy() %>% 
  mutate(estimate = plogis(estimate),
         p.value = round(p.value,3))

nfl_df %>% 
  filter(side == "Offense") %>% 
  mutate(drafted = ifelse(drafted == "Yes", 1, 0)) %>%
  glm(drafted ~ position + weight + forty + broad_jump + bench, family = "binomial", data = .) %>% 
  tidy() %>% 
  mutate(estimate = plogis(estimate),
         p.value = round(p.value,3))

nfl_df %>% 
  filter(side == "Offense") %>% 
  mutate(drafted = ifelse(drafted == "Yes", 1, 0)) %>%
  glm(drafted ~ position + conference + weight + forty + broad_jump + bench, family = "binomial", data = .) %>% 
  tidy() %>% 
  mutate(estimate = plogis(estimate),
         p.value = round(p.value,3))


#
# Exploratory Data Analysis: Defense ----
nfl_df %>% 
  filter(side == "Defense") %>% 
  count(drafted)
# - note: 874 drafted

# STATS: 

# Summary
nfl_df %>% 
  filter(side == "Defense") %>% 
  summarise(Avg_Weight = mean(weight),
            Avg_Forty = mean(forty),
            Avg_BroadJump = mean(broad_jump),
            Avg_Bench = mean(bench))

# By Conference
nfl_defense_stat_Conference <- nfl_df %>% 
  filter(side == "Defense") %>% 
  group_by(conference, drafted, round) %>% 
  summarise(Weight = mean(weight),
            Forty = mean(forty),
            BroadJump = mean(broad_jump),
            Bench = mean(bench)) %>% 
  gather(Weight, Forty, BroadJump, Bench,
         key = "Measure", value = "Stat")
# - drafted
nfl_defense_stat_Conference %>% 
  ggplot(aes(conference, Stat, fill = drafted)) +
  geom_col(position = "dodge") +
  facet_wrap(~Measure, scales = "free_y") +
  labs(title = "Offense-Conference: Stats") +
  theme_bw() + theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 90, hjust = 1)
  ) +
  scale_fill_manual(values = c("grey70","forestgreen"))
# - round
nfl_defense_stat_Conference %>% 
  ggplot(aes(conference, Stat, fill = round)) +
  geom_col(position = "dodge") +
  facet_wrap(~Measure, scales = "free_y") +
  labs(title = "Offense-Conference: Stats") +
  theme_bw() + theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 90, hjust = 1)
  ) +
  scale_fill_manual(values = c("midnightblue","grey70"))

# By Position
nfl_defense_stat_Position <- nfl_df %>% 
  filter(side == "Defense") %>% 
  group_by(position, drafted, round) %>% 
  summarise(Weight = mean(weight),
            Forty = mean(forty),
            BroadJump = mean(broad_jump),
            Bench = mean(bench)) %>% 
  gather(Weight, Forty, BroadJump, Bench,
         key = "Measure", value = "Stat")
# - drafted
nfl_defense_stat_Position %>% 
  ggplot(aes(position, Stat, fill = drafted)) +
  geom_col(position = "dodge") +
  facet_wrap(~Measure, scales = "free_y") +
  labs(title = "Offense-Position: Stats") +
  theme_bw() + theme(
    plot.title = element_text(hjust = 0.5, size = 12)
  ) +
  scale_fill_manual(values = c("grey70","forestgreen"))
# - round
nfl_defense_stat_Position %>% 
  ggplot(aes(position, Stat, fill = round)) +
  geom_col(position = "dodge") +
  facet_wrap(~Measure, scales = "free_y") +
  labs(title = "Offense-Position: Stats") +
  theme_bw() + theme(
    plot.title = element_text(hjust = 0.5, size = 12)
  ) +
  scale_fill_manual(values = c("midnightblue","grey70"))


# Statistical Test
nfl_df %>% 
  filter(side == "Defense") %>% 
  mutate(drafted = ifelse(drafted == "Yes", 1, 0)) %>% 
  glm(drafted ~ weight + forty + broad_jump + bench, family = "binomial", data = .) %>% 
  tidy() %>% 
  mutate(estimate = plogis(estimate),
         p.value = round(p.value,3))

nfl_df %>% 
  filter(side == "Defense") %>% 
  mutate(drafted = ifelse(drafted == "Yes", 1, 0)) %>% 
  glm(drafted ~ conference + weight + forty + broad_jump + bench, family = "binomial", data = .) %>% 
  tidy() %>% 
  mutate(estimate = plogis(estimate),
         p.value = round(p.value,3))

nfl_df %>% 
  filter(side == "Defense") %>% 
  mutate(drafted = ifelse(drafted == "Yes", 1, 0)) %>%
  glm(drafted ~ position + weight + forty + broad_jump + bench, family = "binomial", data = .) %>% 
  tidy() %>% 
  mutate(estimate = plogis(estimate),
         p.value = round(p.value,3))

nfl_df %>% 
  filter(side == "Defense") %>% 
  mutate(drafted = ifelse(drafted == "Yes", 1, 0)) %>%
  glm(drafted ~ position + conference + weight + forty + broad_jump + bench, family = "binomial", data = .) %>% 
  tidy() %>% 
  mutate(estimate = plogis(estimate),
         p.value = round(p.value,3))


#

# Modeling: Prepocess ----
# Split
nfl_split <- initial_split(nfl_df, prop = 0.80, strata = drafted)
nfl_train <- training(nfl_split)
nfl_test <- testing(nfl_split)
# - K-Folds
nfl_10fold <- vfold_cv(nfl_train, v = 10)

# Preprocess
nfl_recipe <- 
  recipe(drafted ~ position + conference + weight + forty + broad_jump + bench,
       data = nfl_train) %>% 
  step_other(position, threshold = 0.01) %>% 
  step_normalize(all_numeric()) %>% 
  step_dummy(position, conference)

nfl_recipe_simple <- 
  recipe(drafted ~ position + conference + weight + forty + broad_jump + bench,
         data = nfl_train) 

# Model Data
nfl_recipe %>% 
  prep() %>% juice() %>% 
  glimpse()

# Control
nfl_ctrl <- 
  control_grid(event_level = "second",
               allow_par = TRUE,
               save_pred = TRUE,
               verbose = TRUE)

# Metrics
nfl_metrics <- metric_set(roc_auc, sens, spec, recall, precision, f_meas)

#
# Modeling: Fit ----

# Start Parallel Processing
cl_3 <- parallel::makeCluster(2)
doParallel::registerDoParallel(cl_3)

# Logistic Regression
# - spec
log_spec <- 
  logistic_reg(
    penalty = tune(),
    mixture = tune()
  ) %>% 
  set_engine("glmnet") %>% 
  set_mode("classification")
# - workflow
log_wflow <- 
  workflow() %>% 
  add_model(log_spec) %>% 
  add_recipe(nfl_recipe) 
# - hyperparameters
log_grid <- 
  log_spec %>% 
  parameters() %>% 
  grid_latin_hypercube(size = 20)
# - tune
log_tune <-
  log_wflow %>% 
  tune_grid(
    resamples = nfl_10fold,
    grid = log_grid,
    metrics = nfl_metrics,
    control = nfl_ctrl
  )
# - visual
log_tune %>% autoplot()
log_best <- log_tune %>% select_best("f_meas")
# - final fit
log_wflow_FIANL <- 
  log_wflow %>% 
  finalize_workflow(log_best)

log_FINAL <- 
  log_wflow_FIANL %>% 
  fit(nfl_train)

# Random Forrest
# - spec
rf_spec <- 
  rand_forest(
    mtry = tune(),
    trees = 1000,
    min_n = tune()
) %>% 
  set_engine("ranger") %>% 
  set_mode("classification")
# - workflow
rf_wflow <- 
  workflow() %>% 
  add_model(rf_spec) %>% 
  add_recipe(nfl_recipe_simple)
# - tune
rf_tune <- 
  rf_wflow %>% 
  tune_grid(
    resamples = nfl_10fold,
    grid = 10,
    metrics = nfl_metrics,
    control = nfl_ctrl
  )
# - results
rf_tune %>% autoplot() + theme_bw()
rf_best <- rf_tune %>% select_best("f_meas")
# - final fit
rf_wflow_FINAL <- 
  rf_wflow %>% 
  finalize_workflow(rf_best)

rf_FINAL <- 
  rf_wflow_FINAL %>% 
  fit(nfl_train)

# Stop Parallell Processing
parallel::stopCluster(cl_3)

# Modeling: Diagnostics ----

# Logistic Regression
# - Predictions
log_Results <- 
  tibble(LOG_Pred = predict(log_FINAL, new_data = nfl_test) %>% pull(),
         LOG_Prob = predict(log_FINAL, new_data = nfl_test, type = "prob") %>% pull())
# - Conf Matrix
log_Results %>% 
  mutate(Actual = factor(nfl_test$drafted)) %>% 
  conf_mat(truth = Actual, estimate = LOG_Pred) %>% 
  autoplot(type = "heatmap") +
  labs(title = " Logistic Regression")
# - Metrics
log_FINAL_Metrics <- 
  tibble(Model = "Logistic_Regression",
         Accuracy = accuracy(log_Results, truth = factor(nfl_test$drafted), estimate = LOG_Pred) %>% pull(.estimate) %>% round(3),
         Detection_Rate = detection_prevalence(log_Results, truth = factor(nfl_test$drafted), estimate = LOG_Pred, event_level = "second") %>% pull(.estimate) %>% round(3),
         Sensitivity = sens(log_Results, truth = factor(nfl_test$drafted), estimate = LOG_Pred, event_level = "second") %>% pull(.estimate) %>% round(3),
         Specificity = spec(log_Results, truth = factor(nfl_test$drafted), estimate = LOG_Pred, event_level = "second") %>% pull(.estimate) %>% round(3),
         Precision = precision(log_Results, truth = factor(nfl_test$drafted), estimate = LOG_Pred, event_level = "second") %>% pull(.estimate) %>% round(3),
         Recall = recall(log_Results, truth = factor(nfl_test$drafted), estimate = LOG_Pred, event_level = "second") %>% pull(.estimate) %>% round(3),
         F1 = f_meas(log_Results, truth = factor(nfl_test$drafted), estimate = LOG_Pred, event_level = "second") %>% pull(.estimate) %>% round(3),
         PPV = ppv(log_Results, truth = factor(nfl_test$drafted), estimate = LOG_Pred, event_level = "second") %>% pull(.estimate) %>% round(3),
         NPV = npv(log_Results, truth = factor(nfl_test$drafted), estimate = LOG_Pred, event_level = "second") %>% pull(.estimate) %>% round(3),
         AUC = roc_auc(log_Results, factor(nfl_test$drafted), LOG_Prob, event_level = "second") %>% pull(.estimate) %>% round(3))


# Random Forrest

# - Predictions
rf_Results <- 
  data_frame(RF_Pred = predict(rf_FINAL, new_data = nfl_test) %>% pull(),
             RF_Prob = predict(rf_FINAL, new_data = nfl_test, type = "prob") %>% pull())
# - Conf Matrix
rf_Results %>% 
  mutate(Actual = factor(nfl_test$drafted)) %>% 
  conf_mat(truth = Actual, estimate = RF_Pred) %>% 
  autoplot(type = "heatmap") +
  labs(title = "Random Forrest")
# - Metrics
rf_FINAL_Metrics <- 
  tibble(Model = "Random_Forest",
         Accuracy = accuracy(rf_Results, truth = factor(nfl_test$drafted), estimate = RF_Pred) %>% pull(.estimate) %>% round(3),
         Detection_Rate = detection_prevalence(rf_Results, truth = factor(nfl_test$drafted), estimate = RF_Pred, event_level = "second") %>% pull(.estimate) %>% round(3),
         Sensitivity = sens(rf_Results, truth = factor(nfl_test$drafted), estimate = RF_Pred, event_level = "second") %>% pull(.estimate) %>% round(3),
         Specificity = spec(rf_Results, truth = factor(nfl_test$drafted), estimate = RF_Pred, event_level = "second") %>% pull(.estimate) %>% round(3),
         Precision = precision(rf_Results, truth = factor(nfl_test$drafted), estimate = RF_Pred, event_level = "second") %>% pull(.estimate) %>% round(3),
         Recall = recall(rf_Results, truth = factor(nfl_test$drafted), estimate = RF_Pred, event_level = "second") %>% pull(.estimate) %>% round(3),
         F1 = f_meas(rf_Results, truth = factor(nfl_test$drafted), estimate = RF_Pred, event_level = "second") %>% pull(.estimate) %>% round(3),
         PPV = ppv(rf_Results, truth = factor(nfl_test$drafted), estimate = RF_Pred, event_level = "second") %>% pull(.estimate) %>% round(3),
         NPV = npv(rf_Results, truth = factor(nfl_test$drafted), estimate = RF_Pred, event_level = "second") %>% pull(.estimate) %>% round(3),
         AUC = roc_auc(rf_Results, factor(nfl_test$drafted), RF_Prob, event_level = "second") %>% pull(.estimate) %>% round(3))


# Summary
log_FINAL_Metrics %>% 
  bind_rows(rf_FINAL_Metrics)

# ROC Curve
# - Logistic Regression
log_ROC <- log_Results %>%
  mutate(Actual = factor(nfl_test$drafted)) %>% 
  roc_curve(Actual, LOG_Prob, event_level = "second") %>% 
  mutate(Model = "Logisitic Regression")
# - Random Forest
rf_ROC <- rf_Results %>%
  mutate(Actual = factor(nfl_test$drafted)) %>% 
  roc_curve(Actual, RF_Prob, event_level = "second") %>% 
  mutate(Model = "Random Forrest")
# - Plot
log_ROC %>% 
  bind_rows(rf_ROC) %>% 
  mutate(specificity = 1 - specificity) %>% 
  ggplot(aes(specificity, sensitivity, color = Model)) +
  geom_line() + geom_abline(slope = 1, linetype = 2, alpha = 0.2) +
  labs(
    x = "1 - specificity"
  ) + 
  theme_bw() + 
  theme(legend.position = "bottom")


