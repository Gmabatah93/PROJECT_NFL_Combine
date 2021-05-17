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
nfl <- nfl_raw %>% 
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
  mutate(side = ifelse(position %in% c("WR","OT","RB","OG","TE","C","FB","QB","LS","OL"), "Offense", "Defense") %>% factor())

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
  ) %>% factor())


# Target Drafted
nfl_draft <- nfl %>% 
  select(player, side, position, school, conference, year, everything()) %>% 
  select(-school, -pick, -round)

# Target Round
nfl_round <- nfl %>% 
  select(player, side, position, school, conference, year, everything()) %>% 
  select(-school, -pick, -drafted) %>% 
  mutate(round = case_when(
                  round == "1st" ~ "1st",
                  TRUE ~ "Not 1st"
                  ) %>% factor
  )
#

# "DRAFT": Exploratory Data Analysis - Summary ----

# Target: 
nfl_draft %>% 
  count(drafted) %>% 
  ggplot(aes(drafted, n)) +
  geom_col(aes(fill = drafted)) +
  geom_label(aes(label = n)) +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 15)
  ) + 
  scale_fill_manual(values = c("grey80", "forestgreen"))

# By Side
gg_draft_Side <- nfl_draft %>%
  group_by(drafted) %>% 
  count(side) %>% 
  mutate(side = fct_reorder(side, n)) %>% 
  ggplot(aes(side, n, fill = drafted)) +
  geom_col(position = "dodge") +
  geom_label(aes(label = n),
             position = position_dodge(width = 1),
             color = "white", alpha = 0.2
            ) +
  labs(title = "Side") +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "none"
  ) +
  scale_fill_manual(values = c("grey80","forestgreen"))
gg_draft_Side_prop <- nfl_draft %>%
  group_by(drafted) %>% 
  count(side) %>% 
  mutate(side = fct_reorder(side, n)) %>% 
  ggplot(aes(side, n, fill = drafted)) +
  geom_col(position = "fill") + geom_hline(yintercept = 0.66, color = "red") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_blank(),
    axis.text.x = element_text(face = "italic", size = 15, hjust = 1, angle = 45),
    legend.position = "none"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("grey80","forestgreen"))
# - visual
ggarrange(gg_draft_Side, gg_draft_Side_prop, 
          ncol = 1)

# By Position
gg_draft_Position <- nfl_draft %>%
  group_by(drafted, side) %>% 
  count(position) %>% 
  ungroup() %>% 
  mutate(position = fct_reorder(position, n)) %>% 
  ggplot(aes(position, n, fill = drafted)) +
  geom_col(position = "dodge") +
  labs(title = "Position") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "none"
  ) +
  scale_fill_manual(values = c("grey80","forestgreen"))
gg_draft_position_Offense_prop <- nfl_draft %>%
  filter(side == "Offense") %>% 
  group_by(drafted, side) %>% 
  count(position) %>% 
  mutate(position = fct_reorder(position, n)) %>% 
  ggplot(aes(position, n, fill = drafted)) +
  geom_col(position = "fill") + geom_hline(yintercept = 0.66, color = "red") +
  labs(title = "Offense") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_blank(),
    axis.text.x = element_text(),
    legend.position = "none"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("grey80","forestgreen"))
gg_draft_position_Defense_prop <- nfl_draft %>%
  filter(side == "Defense") %>% 
  group_by(drafted, side) %>% 
  count(position) %>% 
  mutate(position = fct_reorder(position, n)) %>% 
  ggplot(aes(position, n, fill = drafted)) +
  geom_col(position = "fill") + geom_hline(yintercept = 0.66, color = "red") +
  labs(title = "Defense") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(),
    legend.position = "none"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("grey80","forestgreen"))
gg_draft_Position_prop <- ggarrange(gg_draft_position_Offense_prop, gg_draft_position_Defense_prop,
          nrow = 1)
# - visual
ggarrange(gg_draft_Position, gg_draft_Position_prop, 
          ncol = 1)

# By Conference
gg_draft_conference <- nfl_draft %>%
  group_by(drafted) %>% 
  count(conference) %>% 
  mutate(conference = fct_reorder(conference, n)) %>% 
  ggplot(aes(conference, n, fill = drafted)) +
  geom_col(position = "dodge") +
  labs(title = "Conference") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "none"
  ) +
  scale_fill_manual(values = c("grey80","forestgreen"))
gg_draft_conference_prop <- nfl_draft %>%
  group_by(drafted) %>% 
  count(conference) %>% 
  mutate(conference = fct_reorder(conference, n)) %>% 
  ggplot(aes(conference, n, fill = drafted)) +
  geom_col(position = "fill") +
  geom_hline(yintercept = 0.66, color = "red") +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    legend.position = "none"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("grey80","forestgreen"))
# - visual
ggarrange(gg_draft_conference, gg_draft_conference_prop,
          ncol = 1)

# Combine Stats
nfl_draft %>% 
  select(height:shuttle) %>% cor %>% 
  ggcorrplot::ggcorrplot(type = "lower", 
                         lab = TRUE, lab_col = "gray10",
                         colors = c("darksalmon","white","royalblue4"),
                         tl.cex = 10,
                         title = "Combine Stats: Correlation") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20))

# Drafted
# - Weight ~ Forty: 89
gg_drafted_Weight_Forty <- nfl_draft %>% 
  ggplot(aes(weight, forty, color = drafted)) +
  geom_point(alpha = 0.3) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    axis.title = element_text(face = "bold", size = 14),
    legend.position = "none"
  ) +
  scale_color_manual(values = c("grey80","forestgreen"))
# - Three Cone ~ Shuttle: 85
gg_drafted_3cone_Shuttle <- nfl_draft %>% 
  ggplot(aes(three_cone, shuttle, color = drafted)) +
  geom_point(alpha = 0.3) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    axis.title = element_text(face = "bold", size = 14),
    legend.position = "none"
  ) +
  scale_color_manual(values = c("grey80","forestgreen"))
# - Forty ~ Broad Jump: -84
gg_drafted_Forty_BJump <- nfl_draft %>% 
  ggplot(aes(forty, broad_jump, color = drafted)) +
  geom_point(alpha = 0.3) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    axis.title = element_text(face = "bold", size = 14),
    legend.position = "none"
  ) +
  scale_color_manual(values = c("grey80","forestgreen"))
# - Forty ~ Three Cone: 83
gg_drafted_Forty_3cone <- nfl_draft %>% 
  ggplot(aes(forty, three_cone, color = drafted)) +
  geom_point(alpha = 0.3) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    axis.title = element_text(face = "bold", size = 14),
    legend.position = "none"
  ) +
  scale_color_manual(values = c("grey80","forestgreen"))
# - Vertical ~ Broad Jump: 82
gg_drafted_Vertical_BJump <- nfl_draft %>% 
  ggplot(aes(vertical, broad_jump, color = drafted)) +
  geom_point(alpha = 0.3) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    axis.title = element_text(face = "bold", size = 14),
    legend.position = "none"
  ) +
  scale_color_manual(values = c("grey80","forestgreen"))
# - Weight ~ Three Cone: 81
gg_drafted_Weight_3cone <- nfl_draft %>% 
  ggplot(aes(weight, three_cone, color = drafted)) +
  geom_point(alpha = 0.3) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    axis.title = element_text(face = "bold", size = 14),
    legend.position = "none"
  ) +
  scale_color_manual(values = c("grey80","forestgreen"))

# Multiplot
ggarrange(gg_drafted_Weight_Forty, gg_drafted_3cone_Shuttle, gg_drafted_Forty_BJump, 
          gg_drafted_Forty_3cone, gg_drafted_Vertical_BJump, gg_drafted_Weight_3cone, 
          ncol = 3, nrow = 2)


# "DRAFT": Exploratory Data Analysis - PCA ----

# PCA Object
nfl_draft_PCA <- nfl_draft %>% 
  select(height:shuttle) %>% 
  PCA(graph = FALSE)
# - Graph
nfl_draft_PCA %>% 
  fviz_pca_biplot(repel = TRUE,
                  arrowsize = 1,
                  col.var = "grey25", alpha.var = "cos2",
                  geom.ind = "point", pointsize = 0.5,
                  col.ind = "grey85", 
                  title = "Combine PCA Biplot",
                  legend.title = list(alpha = "Quality of Representation")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        legend.position = "bottom")

# Eigen: 70.1% Explained by PC1
nfl_draft_PCA %>% fviz_eig(addlabels = TRUE, 
                           barfill = "azure4", barcolor = "black") +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank())
# - variables:
nfl_draft_PCA$var$coord[,c(1,2)]
nfl_draft_PCA$var$cos2[,c(1,2)]
nfl_draft_PCA$var$contrib[,c(1,2)]

gg_PC1 <- nfl_draft_PCA %>% 
  fviz_contrib(choice = "var", axes = 1,
               fill = "azure3", color = "black") + 
  labs(title = "Contributions: PC 1") +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        axis.title = element_blank())

gg_PC2 <- nfl_draft_PCA %>% 
  fviz_contrib(choice = "var", axes = 2,
               fill = "azure3", color = "black") +
  labs(title = "Contributions: PC 2") +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        axis.title = element_blank())
# - visaul
ggarrange(gg_PC1, gg_PC2, ncol = 1)

# - PCA: Drafted
nfl_draft_PCA %>% 
  fviz_pca_biplot(repel = TRUE,
                  arrowsize = 1, 
                  col.var = "grey25", alpha.var = "cos2",
                  geom.ind = "point", pointsize = 0.5,
                  col.ind = nfl_draft$drafted, 
                  addEllipses = TRUE, ellispe.type = "norm",
                  palette = c("gray80", "forestgreen"),
                  legend.title = "Drafted") +
  guides(alpha = FALSE) +
  theme(plot.title = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "top",
        legend.title = element_text(face = "bold", size = 15))

# New Dataset
nfl_draft_simple <- nfl_draft %>% 
  select(side, position, conference, weight, forty, broad_jump, bench, drafted)

#
# "DRAFT": Exploratory Data Analysis - Combine ----

# Combine Data: Weight
# - mean
nfl_mean_weight <- nfl_draft_simple$weight %>% mean
# - plot
gg_Combine_Weight_draft <- nfl_draft_simple %>% 
  ggplot(aes(weight)) +
  geom_histogram(bins = 50, alpha = 0.3) + 
  geom_vline(xintercept = nfl_mean_weight, color = "red") +
  labs(title = "Combine: Weight") +
  theme(plot.title = element_text(hjust = 0.3, face = "bold", size = 20),
        axis.title = element_blank())
gg_Combine_Weight_Position_draft <- nfl_draft_simple %>% 
  ggplot(aes(weight, fill = side)) +
  geom_density(alpha = 0.1) +
  scale_fill_manual(values = c("blue","red")) +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "bottom")
# - visual
ggarrange(gg_Combine_Weight_draft, gg_Combine_Weight_Position_draft, ncol = 1)

# - By Side
gg_Combine_Weight_Offense_draft <- nfl_draft_simple %>% 
  filter(side == "Offense") %>% 
  ggplot(aes(position, weight, fill = drafted)) +
  geom_boxplot() + geom_hline(yintercept = nfl_mean_weight, color = "red") +
  geom_jitter(alpha = 0.09) +
  labs(title = "Offense") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        axis.title.y = element_text(color = "red"),
        axis.title.x = element_blank(),
        legend.position = "none") +
  scale_fill_manual(values = c("gray80", "forestgreen"))
gg_Combine_Weight_Defense_draft <- nfl_draft_simple %>% 
  filter(side == "Defense") %>% 
  ggplot(aes(position, weight, fill = drafted)) +
  geom_boxplot() + geom_hline(yintercept = nfl_mean_weight, color = "darkred") +
  geom_jitter(alpha = 0.09) +
  labs(title = "Defense") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        axis.title = element_blank(),
        axis.text.y = element_blank()) +
  scale_fill_manual(values = c("gray80", "forestgreen"))
# -- visual
ggarrange(gg_Combine_Weight_Offense_draft, gg_Combine_Weight_Defense_draft, nrow = 1)  

# - By Conference
gg_Combine_Weight_Conference_draft <- nfl_draft_simple %>%
  mutate(cond = case_when(
    conference %in% c("Division I-A (SEC)", "Division I-A (ACC)", "Division I-A (Big 10)", "Division I-A (Big 12)", "Division I-A (Pac-12)") ~ "Elite",
    conference %in% c("Division I-A (American)", "Division I-A (Sunbelt)", "Division I-A (Mountain West)", "Division I-A (MAC)","Division I-A (Conference USA)") ~ "Division I-A",
    conference %in% c("Division I-AA") ~ "Division I-AA",
    conference %in% c("Division II & III") ~ "Division II & III"
  )) %>% 
  ggplot(aes(conference, weight, fill = cond)) +
  geom_boxplot() + geom_hline(yintercept = nfl_mean_weight, color = "red") +
  labs(title = "Conference") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(colour = "red"),
        axis.title.x = element_blank()) +
  scale_fill_brewer(palette = "YlGnBu")
# -- visual
gg_Combine_Weight_Conference_draft


# Combine Data: Forty
# - mean
nfl_mean_forty <- nfl_draft_simple$forty %>% mean
# - plot
gg_Combine_40_draft <- nfl_draft_simple %>% 
  ggplot(aes(forty)) +
  geom_histogram(bins = 50, alpha = 0.3) + 
  geom_vline(xintercept = nfl_mean_forty, color = "red") +
  labs(title = "Combine: 40") +
  theme(plot.title = element_text(hjust = 0.3, face = "bold", size = 20),
        axis.title = element_blank())
gg_Combine_40_Position_draft <- nfl_draft_simple %>% 
  ggplot(aes(forty, fill = side)) +
  geom_density(alpha = 0.1) +
  scale_fill_manual(values = c("blue","red")) +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "bottom")
# - visual
ggarrange(gg_Combine_40_draft, gg_Combine_40_Position_draft, ncol = 1)

# - By Side
gg_Combine_40_Offense_draft <- nfl_draft_simple %>% 
  filter(side == "Offense") %>% 
  ggplot(aes(position, forty, fill = drafted)) +
  geom_boxplot() + geom_hline(yintercept = nfl_mean_forty, color = "red") +
  geom_jitter(alpha = 0.09) +
  labs(title = "Offense") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        axis.title.y = element_text(color = "red"),
        axis.title.x = element_blank(),
        legend.position = "none") +
  ylim(c(4,6)) +
  scale_fill_manual(values = c("gray80", "forestgreen")) 
gg_Combine_40_Defense_draft <- nfl_draft_simple %>% 
  filter(side == "Defense") %>% 
  ggplot(aes(position, forty, fill = drafted)) +
  geom_boxplot() + geom_hline(yintercept = nfl_mean_forty, color = "darkred") +
  geom_jitter(alpha = 0.09) +
  labs(title = "Defense") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        axis.title = element_blank(),
        axis.text.y = element_blank()) +
  ylim(c(4,6)) +
  scale_fill_manual(values = c("gray80", "forestgreen"))
# -- visual
ggarrange(gg_Combine_40_Offense_draft, gg_Combine_40_Defense_draft, nrow = 1)  


# - By Conference
gg_Combine_40_Conference_draft <- nfl_draft_simple %>%
  mutate(cond = case_when(
    conference %in% c("Division I-A (SEC)", "Division I-A (ACC)", "Division I-A (Big 10)", "Division I-A (Big 12)", "Division I-A (Pac-12)") ~ "Elite",
    conference %in% c("Division I-A (American)", "Division I-A (Sunbelt)", "Division I-A (Mountain West)", "Division I-A (MAC)","Division I-A (Conference USA)") ~ "Division I-A",
    conference %in% c("Division I-AA") ~ "Division I-AA",
    conference %in% c("Division II & III") ~ "Division II & III"
  )) %>% 
  ggplot(aes(conference, forty, fill = cond)) +
  geom_boxplot() + geom_hline(yintercept = nfl_mean_forty, color = "red") +
  labs(title = "Conference") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(colour = "red"),
        axis.title.x = element_blank()) +
  scale_fill_brewer(palette = "YlGnBu")
# -- visual
gg_Combine_40_Conference_draft


# Combine Data: Bench
# - mean
nfl_mean_bench <- nfl_draft_simple$bench %>% mean
# - plot
gg_Combine_Bench_draft <- nfl_draft_simple %>% 
  ggplot(aes(bench)) +
  geom_histogram(bins = 50, alpha = 0.3) + 
  geom_vline(xintercept = nfl_mean_bench, color = "red") +
  labs(title = "Combine: Bench") +
  theme(plot.title = element_text(hjust = 0.3, face = "bold", size = 20),
        axis.title = element_blank())
gg_Combine_Bench_Position_draft <- nfl_draft_simple %>% 
  ggplot(aes(bench, fill = side)) +
  geom_density(alpha = 0.1) +
  scale_fill_manual(values = c("blue","red")) +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "bottom")
# - visual
ggarrange(gg_Combine_Bench_draft, gg_Combine_Bench_Position_draft, ncol = 1)

# - By Side
gg_Combine_Bench_Offense_draft <- nfl_draft_simple %>% 
  filter(side == "Offense") %>% 
  ggplot(aes(position, bench, fill = drafted)) +
  geom_boxplot() + geom_hline(yintercept = nfl_mean_bench, color = "red") +
  geom_jitter(alpha = 0.09) +
  labs(title = "Offense") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        axis.title.y = element_text(color = "red"),
        axis.title.x = element_blank(),
        legend.position = "none") +
  scale_fill_manual(values = c("gray80", "forestgreen"))
gg_Combine_Bench_Defense_draft <- nfl_draft_simple %>% 
  filter(side == "Defense") %>% 
  ggplot(aes(position, bench, fill = drafted)) +
  geom_boxplot() + geom_hline(yintercept = nfl_mean_bench, color = "darkred") +
  geom_jitter(alpha = 0.09) +
  labs(title = "Defense") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        axis.title = element_blank(),
        axis.text.y = element_blank()) +
  scale_fill_manual(values = c("gray80", "forestgreen"))
# -- visual
ggarrange(gg_Combine_Bench_Offense_draft, gg_Combine_Bench_Defense_draft, nrow = 1)  

# - By Conference
gg_Combine_Bench_Conference_draft <- nfl_draft_simple %>%
  mutate(cond = case_when(
    conference %in% c("Division I-A (SEC)", "Division I-A (ACC)", "Division I-A (Big 10)", "Division I-A (Big 12)", "Division I-A (Pac-12)") ~ "Elite",
    conference %in% c("Division I-A (American)", "Division I-A (Sunbelt)", "Division I-A (Mountain West)", "Division I-A (MAC)","Division I-A (Conference USA)") ~ "Division I-A",
    conference %in% c("Division I-AA") ~ "Division I-AA",
    conference %in% c("Division II & III") ~ "Division II & III"
  )) %>% 
  ggplot(aes(conference, bench, fill = cond)) +
  geom_boxplot() + geom_hline(yintercept = nfl_mean_bench, color = "red") +
  labs(title = "Conference") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(colour = "red"),
        axis.title.x = element_blank()) +
  scale_fill_brewer(palette = "YlGnBu")
# -- visual
gg_Combine_Bench_Conference_draft

# Combine Data: Broad Jump
# - mean
nfl_mean_jump <- nfl_draft_simple$broad_jump %>% mean
# - plot
gg_Combine_Jump_draft <- nfl_draft_simple %>% 
  ggplot(aes(broad_jump)) +
  geom_histogram(bins = 50, alpha = 0.3) + 
  geom_vline(xintercept = nfl_mean_jump, color = "red") +
  labs(title = "Combine: Broad Jump") +
  theme(plot.title = element_text(hjust = 0.3, face = "bold", size = 20),
        axis.title = element_blank())
gg_Combine_Jump_Position_draft <- nfl_draft_simple %>% 
  ggplot(aes(broad_jump, fill = side)) +
  geom_density(alpha = 0.1) +
  scale_fill_manual(values = c("blue","red")) +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "bottom")
# - visual
ggarrange(gg_Combine_Jump_draft, gg_Combine_Jump_Position_draft, ncol = 1)

# - By Side
gg_Combine_Jump_Offense_draft <- nfl_draft_simple %>% 
  filter(side == "Offense") %>% 
  ggplot(aes(position, broad_jump, fill = drafted)) +
  geom_boxplot() + geom_hline(yintercept = nfl_mean_jump, color = "red") +
  geom_jitter(alpha = 0.09) +
  labs(title = "Offense") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        axis.title.y = element_text(color = "red"),
        axis.title.x = element_blank(),
        legend.position = "none") +
  ylim(c(80,140)) +
  scale_fill_manual(values = c("gray80", "forestgreen"))
gg_Combine_Jump_Defense_draft <- nfl_draft_simple %>% 
  filter(side == "Defense") %>% 
  ggplot(aes(position, broad_jump, fill = drafted)) +
  geom_boxplot() + geom_hline(yintercept = nfl_mean_jump, color = "darkred") +
  geom_jitter(alpha = 0.09) +
  labs(title = "Defense") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        axis.title = element_blank(),
        axis.text.y = element_blank()) +
  ylim(c(80,140)) +
  scale_fill_manual(values = c("gray80", "forestgreen"))
# -- visual
ggarrange(gg_Combine_Jump_Offense_draft, gg_Combine_Jump_Defense_draft, nrow = 1)  

# - By Conference
gg_Combine_Jump_Conference_draft <- nfl_draft_simple %>%
  mutate(cond = case_when(
    conference %in% c("Division I-A (SEC)", "Division I-A (ACC)", "Division I-A (Big 10)", "Division I-A (Big 12)", "Division I-A (Pac-12)") ~ "Elite",
    conference %in% c("Division I-A (American)", "Division I-A (Sunbelt)", "Division I-A (Mountain West)", "Division I-A (MAC)","Division I-A (Conference USA)") ~ "Division I-A",
    conference %in% c("Division I-AA") ~ "Division I-AA",
    conference %in% c("Division II & III") ~ "Division II & III"
  )) %>% 
  ggplot(aes(conference, broad_jump, fill = cond)) +
  geom_boxplot() + geom_hline(yintercept = nfl_mean_jump, color = "red") +
  labs(title = "Conference") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(colour = "red"),
        axis.title.x = element_blank()) +
  scale_fill_brewer(palette = "YlGnBu")
# -- visual
gg_Combine_Jump_Conference_draft



# "DRAFT": Exploratory Data Analysis - Offense ----
nfl_draft_simple %>% 
  filter(side =="Offense") %>% 
  count(drafted)
# - note: 874 drafted

# STATS: 

# Summary
nfl_draft_simple %>% 
  filter(side == "Offense") %>% 
  summarise(Avg_Weight = mean(weight),
            Avg_Forty = mean(forty),
            Avg_BroadJump = mean(broad_jump),
            Avg_Bench = mean(bench))

# By Conference
nfl_offense_stat_Conference <- nfl_draft_simple %>% 
  filter(side == "Offense") %>% 
  group_by(conference, drafted) %>% 
  summarise(Weight = mean(weight),
            Forty = mean(forty),
            BroadJump = mean(broad_jump),
            Bench = mean(bench)) %>% 
  gather(Weight, Forty, BroadJump, Bench,
         key = "Measure", value = "Stat")
# - plot
gg_Offense_Conference_drafted <- nfl_offense_stat_Conference %>% 
  ggplot(aes(conference, Stat, fill = drafted)) +
  geom_col(position = "dodge") +
  facet_wrap(~Measure, scales = "free_y") +
  labs(title = "Offense-Conference: Stats") +
  theme_bw() + theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "none"
  ) +
  scale_fill_manual(values = c("grey80","forestgreen"))
# - visual
gg_Offense_Conference_drafted

# By Position
nfl_offense_stat_Position <- nfl_draft_simple %>% 
  filter(side == "Offense") %>% 
  group_by(position, drafted) %>% 
  summarise(Weight = mean(weight),
            Forty = mean(forty),
            BroadJump = mean(broad_jump),
            Bench = mean(bench)) %>% 
  gather(Weight, Forty, BroadJump, Bench,
         key = "Measure", value = "Stat")
# - plot
gg_Offense_Position_drafted <-  nfl_offense_stat_Position %>% 
  ggplot(aes(position, Stat, fill = drafted)) +
  geom_col(position = "dodge") +
  facet_wrap(~Measure, scales = "free_y") +
  labs(title = "Offense-Position: Stats") +
  theme_bw() + theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.title = element_blank(),
    legend.position = "none"
  ) +
  scale_fill_manual(values = c("grey80","forestgreen"))
# - visual
gg_Offense_Position_drafted

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
# "DRAFT": Exploratory Data Analysis - Defense ----
nfl_draft_simple %>% 
  filter(side == "Defense") %>% 
  count(drafted)
# - note: 874 drafted

# STATS: 

# Summary
nfl_draft_simple %>% 
  filter(side == "Defense") %>% 
  summarise(Avg_Weight = mean(weight),
            Avg_Forty = mean(forty),
            Avg_BroadJump = mean(broad_jump),
            Avg_Bench = mean(bench))

# By Conference
nfl_defense_stat_Conference <- nfl_draft_simple %>% 
  filter(side == "Defense") %>% 
  group_by(conference, drafted) %>% 
  summarise(Weight = mean(weight),
            Forty = mean(forty),
            BroadJump = mean(broad_jump),
            Bench = mean(bench)) %>% 
  gather(Weight, Forty, BroadJump, Bench,
         key = "Measure", value = "Stat")
# - drafted
gg_Defense_Conference_drafted <- nfl_defense_stat_Conference %>% 
  ggplot(aes(conference, Stat, fill = drafted)) +
  geom_col(position = "dodge") +
  facet_wrap(~Measure, scales = "free_y") +
  labs(title = "Defense-Conference: Stats") +
  theme_bw() + theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "none"
  ) +
  scale_fill_manual(values = c("grey80","forestgreen"))
# - visual
gg_Defense_Conference_drafted

# By Position
nfl_defense_stat_Position <- nfl_draft_simple %>% 
  filter(side == "Defense") %>% 
  group_by(position, drafted) %>% 
  summarise(Weight = mean(weight),
            Forty = mean(forty),
            BroadJump = mean(broad_jump),
            Bench = mean(bench)) %>% 
  gather(Weight, Forty, BroadJump, Bench,
         key = "Measure", value = "Stat")
# - drafted
gg_Defense_Position_drafted <- nfl_defense_stat_Position %>% 
  ggplot(aes(position, Stat, fill = drafted)) +
  geom_col(position = "dodge") +
  facet_wrap(~Measure, scales = "free_y") +
  labs(title = "Defense-Position: Stats") +
  theme_bw() + theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.title = element_blank(),
    legend.position = "none"
  ) +
  scale_fill_manual(values = c("grey80","forestgreen"))
# - visual
gg_Defense_Position_drafted

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
nfl_split <- initial_split(nfl_draft_simple, prop = 0.80, strata = drafted)
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
# - model data
nfl_recipe %>% 
  prep() %>% juice() %>% 
  glimpse()

nfl_recipe_simple <- 
  recipe(drafted ~ position + conference + weight + forty + broad_jump + bench,
         data = nfl_train) 
# - model data
nfl_recipe_simple %>% 
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

# LOGISTIC REGRESSION
# Spec
log_spec <- 
  logistic_reg(
    penalty = tune(),
    mixture = tune()
  ) %>% 
  set_engine("glmnet") %>% 
  set_mode("classification")
# Workflow
log_wflow <- 
  workflow() %>% 
  add_model(log_spec) %>% 
  add_recipe(nfl_recipe) 


# Hyperparameters
log_spec %>%  parameters() %>% pull_dials_object("penalty")
log_spec %>%  parameters() %>% pull_dials_object("mixture")
# - Random Grid
set.seed(101)
log_grid_random <- 
  log_spec %>% 
  parameters(penalty(trans = NULL)) %>% 
  grid_random(size = 50)
# -- plot
gg_Log_Grid_random <- log_grid_random %>% 
  ggplot(aes(penalty, mixture)) +
  geom_point(shape = 1, size = 4, color = "grey60") +
  labs(title = "Random Grid",
       subtitle = "Logistic Regression") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.4, color = "darkolivegreen"),
        axis.title = element_text(face = "bold", color = "cyan4"))
# - Latin Hypercube
set.seed(101)
log_grid_latin <- 
  log_spec %>% 
  parameters(penalty(trans = NULL)) %>% 
  grid_latin_hypercube(size = 50)
# -- plot
gg_Log_Grid_latin <- log_grid_latin %>% 
  ggplot(aes(penalty, mixture)) +
  geom_point(shape = 1, size = 4, color = "grey60") +
  labs(title = "Latin Grid",
       subtitle = "Logistic Regression") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.4, color = "darkolivegreen"),
        axis.title = element_text(face = "bold", color = "cyan4"))
# - Custom Grid
log_grid_custom <- 
  crossing(
    penalty = seq(0.001, 0.1, 0.005),
    mixture = c(0, 0.5, 1)
  )
# -- plot
gg_Log_Grid_custom <- log_grid_custom %>% 
  ggplot(aes(penalty, mixture)) +
  geom_point(shape = 1, size = 4, color = "grey60") +
  labs(title = "Custom Grid",
       subtitle = "Logistic Regression") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.4, color = "darkolivegreen"),
        axis.title = element_text(face = "bold", color = "cyan4"))

# Visual
ggarrange(gg_Log_Grid_random, gg_Log_Grid_latin, gg_Log_Grid_custom, nrow = 1)

# Tune
# - Random Grid
log_tune_random <-
  log_wflow %>% 
  tune_grid(
    resamples = nfl_10fold,
    grid = log_grid_random,
    metrics = nfl_metrics,
    control = nfl_ctrl
  )
# -- plot
gg_Log_tune_random <- log_tune_random %>% 
  collect_metrics() %>% 
  filter(.metric == "f_meas") %>%
  mutate(mixture = case_when(
    mixture >= 0   & mixture < 0.2 ~ "[0 - 0.2)",
    mixture >= 0.2 & mixture < 0.4 ~ "[0.2 - 0.4)",
    mixture >= 0.4 & mixture < 0.6 ~ "[0.4 - 0.6)",
    mixture >= 0.6 & mixture < 0.8 ~ "[0.6 - 0.8)",
    mixture >= 0.8 & mixture <= 1  ~ "[0.8 - 1]",
  ) %>% factor) %>% 
  ggplot(aes(penalty, mean)) +
  geom_line(aes(color = mixture)) + geom_point() +
  labs(title = "Evaluation: Random Grid",
       subtitle = "Logistic Regression",
       y = "F Score") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.title = element_text(face = "bold", color = "cyan4"),
        legend.text = element_text(size = 7),
        legend.position = "bottom")

# - Latin Grid
log_tune_latin <-
  log_wflow %>% 
  tune_grid(
    resamples = nfl_10fold,
    grid = log_grid_latin,
    metrics = nfl_metrics,
    control = nfl_ctrl
  )
# -- plot
gg_Log_tune_latin <- log_tune_latin %>% 
  collect_metrics() %>% 
  filter(.metric == "f_meas") %>%
  mutate(mixture = case_when(
    mixture >= 0   & mixture < 0.2 ~ "[0 - 0.2)",
    mixture >= 0.2 & mixture < 0.4 ~ "[0.2 - 0.4)",
    mixture >= 0.4 & mixture < 0.6 ~ "[0.4 - 0.6)",
    mixture >= 0.6 & mixture < 0.8 ~ "[0.6 - 0.8)",
    mixture >= 0.8 & mixture <= 1  ~ "[0.8 - 1]",
  ) %>% factor) %>% 
  ggplot(aes(penalty, mean)) +
  geom_line(aes(color = mixture)) + geom_point() +
  labs(title = "Evaluation: Latin Grid",
       subtitle = "Logistic Regression",
       y = "F Score") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.title = element_text(face = "bold", color = "cyan4"),
        legend.text = element_text(size = 7),
        legend.position = "bottom")


# - Custom Grid
log_tune_custom <-
  log_wflow %>% 
  tune_grid(
    resamples = nfl_10fold,
    grid = log_grid_custom,
    metrics = nfl_metrics,
    control = nfl_ctrl
  )
# -- plot
gg_Log_tune_custom <- log_tune_custom %>% 
  collect_metrics() %>% 
  filter(.metric == "f_meas") %>%
  mutate(mixture = factor(mixture)) %>% 
  ggplot(aes(penalty, mean)) +
  geom_line(aes(color = mixture)) + geom_point() +
  labs(title = "Evaluation: Custom Grid",
       subtitle = "Logistic Regression",
       y = "F Score") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.title = element_text(face = "bold", color = "cyan4"),
        legend.position = "bottom")



# Visual
ggarrange(gg_Log_tune_random, gg_Log_tune_latin, gg_Log_tune_custom, nrow = 1)







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
# - hyperparameters
rf_spec %>%  parameters() %>% pull_dials_object("min_n")
rf_spec %>%  parameters() %>% pull_dials_object("mtry")
# -- Regular Custom Grid
rf_grid_custom <- 
  crossing(
    min_n = seq(1,40,2),
    mtry = c(1,2,5,8)
  )
# --tune
rf_tune_custom <- 
  rf_wflow %>% 
  tune_grid(
    resamples = nfl_10fold,
    grid = rf_grid_custom,
    metrics = nfl_metrics,
    control = nfl_ctrl
  )
# - results
rf_tune_custom %>% 
  collect_metrics() %>% 
  filter(.metric == "f_meas") %>% 
  mutate(mtry = factor(mtry)) %>% 
  ggplot(aes(min_n, mean)) +
  geom_line(aes(color = mtry)) + geom_point() +
  labs(title = "Random Forrest: Custom Grid",
       y = "F Score") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold"),
        axis.title.y = element_text(color = "tomato"))

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


