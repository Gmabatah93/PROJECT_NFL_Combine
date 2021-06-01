library(tidymodels)
library(stringr)
library(forcats)
library(RColorBrewer)
theme_set(theme_minimal())
library(ggmosaic)
library(ggpubr)
library(FactoMineR)
library(factoextra)
library(probably)
library(vip)
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
  mutate(drafted = ifelse(is.na(drafted), "No","Yes") %>% factor(levels = c("Yes","No")))

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
  select(-pick,-round) %>% 
  mutate(drafted = ifelse(drafted == "Yes", 1,0) %>% factor(levels = c(1,0), labels = c("Yes","No")))

# Target Round
nfl_round <- nfl %>% 
  select(player, side, position, school, conference, year, everything()) %>% 
  select(-pick, -drafted) %>% 
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

# Modeling: Split & Control ----
# Split
set.seed(101)
nfl_split <- initial_split(nfl_draft, prop = 0.80, strata = drafted)
nfl_other <- training(nfl_split)
nfl_test <- testing(nfl_split)
nfl_split_2 <- initial_split(nfl_other, prop = 0.80, strata = drafted)
nfl_train <- training(nfl_split_2)
nfl_val <- testing(nfl_split_2)
# - K-Folds
nfl_10fold <- vfold_cv(nfl_train, v = 10)

# Control
nfl_ctrl <- 
  control_grid(allow_par = TRUE, 
               parallel_over = "resamples",
               save_pred = TRUE,
               verbose = TRUE)

# Metrics
nfl_metrics <- metric_set(roc_auc, accuracy, sens, spec, precision, f_meas)



#
# Modeling: Preprocess - Logistic Regression ----

# Normal
nfl_log_recipe_normal <- 
  recipe(drafted ~ .,
         data = nfl_train) %>% 
  update_role(player, school, new_role = "id") %>% 
  update_role(team, new_role = "team") %>% 
  step_normalize(all_numeric()) %>% 
  step_dummy(side, position, conference, year)
# - model data
nfl_log_recipe_normal %>% 
  prep() %>% juice() %>% 
  glimpse()


# PCA
nfl_log_recipe_pca <- 
  recipe(drafted ~ .,
         data = nfl_train) %>% 
  update_role(player, school, new_role = "id") %>% 
  update_role(team, new_role = "team") %>% 
  step_other(position, threshold = 0.01) %>% 
  step_normalize(all_numeric()) %>% 
  step_pca(all_numeric(), num_comp = 2, id = "pca") %>% 
  step_dummy(all_predictors(),-all_numeric(), -all_outcomes())

# Eigen
nfl_log_recipe_pca %>% 
  prep() %>% 
  tidy(id = "pca", type = "variance") %>%  
  filter(terms == "percent variance") %>% 
  ggplot(aes(component, value)) +
  geom_col(fill = "azure4") +
  geom_label(aes(label = round(value,1))) +
  labs(
    title = "Eigenvalues",
    y = "% of Total Variance"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.2, color = "grey30"),
    axis.text.y = element_blank()
  )

# Prinipal Components
nfl_log_recipe_pca %>% 
  prep() %>% 
  tidy(id = "pca") %>%
  filter(component %in% c("PC1", "PC2")) %>% 
  mutate(terms = tidytext::reorder_within(terms, 
                                          abs(value),
                                          component)) %>% 
  ggplot(aes(abs(value), terms, fill = value > 0)) +
  geom_col() +
  facet_wrap(~ component, scales = "free") +
  tidytext::scale_y_reordered() +
  scale_fill_manual(values = c("antiquewhite4","antiquewhite")) +
  labs(
    title = "Principal Components",
    x = "Absolute value of contribution",
    y = NULL,
    fill = "Positive ?"
  ) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, color = "grey30"))

# Data
nfl_log_recipe_pca %>% 
  prep() %>% juice() %>% glimpse()


nfl_log_recipe_pca %>% 
  prep() %>% 
  tidy(id = "pca") %>% 
  pivot_wider(names_from = component, id_cols = terms) %>% 
  select(terms:PC4)

# Simple
nfl_log_recipe_simple <- 
  recipe(drafted ~ player + school + team +
           position + conference + year + 
           height + weight + forty + broad_jump + bench,
         data = nfl_train) %>% 
  update_role(player, school, new_role = "id") %>% 
  update_role(team, new_role = "team") %>% 
  step_other(position, threshold = 0.01) %>% 
  step_normalize(all_numeric()) %>% 
  step_dummy(position, conference, year)
# - model data
nfl_log_recipe_simple %>% 
  prep() %>% juice() %>% 
  glimpse()

#
# Modeling: Preprocess - Random Forrest ----

# Normal
nfl_rf_recipe_normal <- 
  recipe(drafted ~ .,
         data = nfl_train) %>% 
  update_role(player, school, new_role = "id") %>% 
  update_role(team, new_role = "team")
# - model data
nfl_rf_recipe_normal %>% 
  prep() %>% juice() %>% 
  glimpse()

# PCA
nfl_rf_recipe_pca <- 
  recipe(drafted ~ .,
         data = nfl_train) %>% 
  update_role(player, school, new_role = "id") %>% 
  update_role(team, new_role = "team") %>% 
  step_other(position, threshold = 0.01) %>% 
  step_normalize(all_numeric()) %>% 
  step_pca(all_numeric(), num_comp = 2, id = "pca") 
# - model data
nfl_rf_recipe_pca %>% 
  prep() %>% juice() %>% 
  glimpse()

# Simple
nfl_rf_recipe_simple <- 
  recipe(drafted ~ player + school + team +
           position + conference + year + 
           height + weight + forty + broad_jump + bench,
         data = nfl_train) %>% 
  update_role(player, school, new_role = "id") %>% 
  update_role(team, new_role = "team")
# - model data
nfl_rf_recipe_simple %>% 
  prep() %>% juice() %>% 
  glimpse()

#
# Modeling: Spec - Logistic Regression ----

# Spec
log_spec <- 
  logistic_reg(
    penalty = tune(),
    mixture = tune()
  ) %>% 
  set_engine("glmnet", seed = 101, importance = "impurity") %>% 
  set_mode("classification")
# Workflow
log_wflow <- 
  workflow() %>% 
  add_model(log_spec) %>% 
  add_recipe(nfl_log_recipe_normal) 

# Hyperparameters
log_spec %>%  parameters() %>% pull_dials_object("penalty")
log_spec %>%  parameters() %>% pull_dials_object("mixture")

# Latin Hypercube
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
ggarrange(gg_Log_Grid_latin, gg_Log_Grid_custom, nrow = 1)

# Modeling: Spec - Random Forrest ----

# Spec
rf_spec <- 
  rand_forest(
    mtry = tune(),
    trees = 1000,
    min_n = tune()
  ) %>% 
  set_engine("ranger", seed = 101, importance = "impurity") %>% # or permutation
  set_mode("classification")

# Workflow
rf_wflow <- 
  workflow() %>% 
  add_model(rf_spec) %>% 
  add_recipe(nfl_rf_recipe_normal)

# Hyperparameters
rf_spec %>%  parameters() %>% pull_dials_object("min_n")
rf_spec %>%  parameters() %>% update(mtry = mtry(c(1,8))) %>%  pull_dials_object("mtry")

# - Latin Hypercube
rf_grid_latin <- 
  rf_spec %>% 
  parameters(penalty(trans = NULL)) %>% 
  update(mtry = mtry(c(1,8))) %>% 
  grid_latin_hypercube(size = 50)
# -- plot
gg_RF_Grid_latin <- rf_grid_latin %>% 
  ggplot(aes(min_n, mtry)) +
  geom_point(shape = 1, size = 4, color = "grey60") +
  labs(title = "Latin Grid",
       subtitle = "Random Forrest") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.4, color = "darkolivegreen"),
        axis.title = element_text(face = "bold", color = "cyan4"))
# - Custom Grid
rf_grid_custom <- 
  crossing(
    min_n = seq(1, 10, 2),
    mtry = 1:8
  )
# -- plot
gg_RF_Grid_custom <- rf_grid_custom %>% 
  ggplot(aes(min_n, mtry)) +
  geom_point(shape = 1, size = 4, color = "grey60") +
  labs(title = "Custom Grid",
       subtitle = "Random Forrest") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.4, color = "darkolivegreen"),
        axis.title = element_text(face = "bold", color = "cyan4"))

# Visual
ggarrange(gg_RF_Grid_latin, gg_RF_Grid_custom, nrow = 1)


#
# Modeling: Fit - Logistic Regression ----
options(tidymodels.dark = TRUE) 

# Latin Grid
# - normal
log_tune_latin_normal <-
  log_wflow %>% 
  tune_grid(
    resamples = nfl_10fold,
    grid = log_grid_latin,
    metrics = nfl_metrics,
    control = nfl_ctrl
  )
# - pca 
log_tune_latin_pca <-
  log_wflow %>% 
  update_recipe(nfl_log_recipe_pca) %>% 
  tune_grid(
    resamples = nfl_10fold,
    grid = log_grid_latin,
    metrics = nfl_metrics,
    control = nfl_ctrl
  )
# - simple
log_tune_latin_simple <-
  log_wflow %>% 
  update_recipe(nfl_log_recipe_simple) %>% 
  tune_grid(
    resamples = nfl_10fold,
    grid = log_grid_latin,
    metrics = nfl_metrics,
    control = nfl_ctrl
  )

# PLOT: Accuracy
# - normal
gg_Log_tune_latin_normal_Acc <- log_tune_latin_normal %>% 
  collect_metrics() %>% 
  filter(.metric == "accuracy") %>%
  mutate(mixture = case_when(
    mixture >= 0   & mixture < 0.2 ~ "[0 - 0.2)",
    mixture >= 0.2 & mixture < 0.4 ~ "[0.2 - 0.4)",
    mixture >= 0.4 & mixture < 0.6 ~ "[0.4 - 0.6)",
    mixture >= 0.6 & mixture < 0.8 ~ "[0.6 - 0.8)",
    mixture >= 0.8 & mixture <= 1  ~ "[0.8 - 1]",
  ) %>% factor) %>% 
  ggplot(aes(penalty, mean)) +
  geom_line(aes(color = mixture)) + geom_point() +
  labs(title = "Latin Grid",
       subtitle = "Logistic Regression - Normal",
       y = "Accuracy") +
  ylim(c(0.65, 0.72)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.4, color = "darkolivegreen"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.title = element_text(face = "bold", color = "cyan4"),
        legend.text = element_text(size = 3),
        legend.position = "bottom")
# - pca 
gg_Log_tune_latin_pca_Acc <- log_tune_latin_pca %>% 
  collect_metrics() %>% 
  filter(.metric == "accuracy") %>%
  mutate(mixture = case_when(
    mixture >= 0   & mixture < 0.2 ~ "[0 - 0.2)",
    mixture >= 0.2 & mixture < 0.4 ~ "[0.2 - 0.4)",
    mixture >= 0.4 & mixture < 0.6 ~ "[0.4 - 0.6)",
    mixture >= 0.6 & mixture < 0.8 ~ "[0.6 - 0.8)",
    mixture >= 0.8 & mixture <= 1  ~ "[0.8 - 1]",
  ) %>% factor) %>% 
  ggplot(aes(penalty, mean)) +
  geom_line(aes(color = mixture)) + geom_point() +
  labs(title = "Latin Grid",
       subtitle = "Logistic Regression - PCA",
       y = "Accuracy") +
  ylim(c(0.65, 0.72)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.4, color = "darkolivegreen"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.title = element_text(face = "bold", color = "cyan4"),
        legend.text = element_text(size = 3),
        legend.position = "bottom")
# - simple
gg_Log_tune_latin_simple_Acc <- log_tune_latin_simple %>% 
  collect_metrics() %>% 
  filter(.metric == "accuracy") %>%
  mutate(mixture = case_when(
    mixture >= 0   & mixture < 0.2 ~ "[0 - 0.2)",
    mixture >= 0.2 & mixture < 0.4 ~ "[0.2 - 0.4)",
    mixture >= 0.4 & mixture < 0.6 ~ "[0.4 - 0.6)",
    mixture >= 0.6 & mixture < 0.8 ~ "[0.6 - 0.8)",
    mixture >= 0.8 & mixture <= 1  ~ "[0.8 - 1]",
  ) %>% factor) %>% 
  ggplot(aes(penalty, mean)) +
  geom_line(aes(color = mixture)) + geom_point() +
  labs(title = "Latin Grid",
       subtitle = "Logistic Regression - Simple",
       y = "Accuracy") +
  ylim(c(0.65, 0.72)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.4, color = "darkolivegreen"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.title = element_text(face = "bold", color = "cyan4"),
        legend.text = element_text(size = 3),
        legend.position = "bottom")
# - VISUAL
ggarrange(gg_Log_tune_latin_normal_Acc, gg_Log_tune_latin_pca_Acc, gg_Log_tune_latin_simple_Acc, nrow = 1)

# PLOT: F Score
# - noraml
gg_Log_tune_latin_normal_F <- log_tune_latin_normal %>% 
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
  labs(title = "Latin Grid",
       subtitle = "Logistic Regression - Normal",
       y = "F Score") +
  ylim(c(0.78, 0.805)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.4, color = "darkolivegreen"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.title = element_text(face = "bold", color = "cyan4"),
        legend.text = element_text(size = 3),
        legend.position = "bottom")
# - pca
gg_Log_tune_latin_pca_F <- log_tune_latin_pca %>% 
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
  labs(title = "Latin Grid",
       subtitle = "Logistic Regression - PCA",
       y = "F Score") +
  ylim(c(0.78, 0.805)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.4, color = "darkolivegreen"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.title = element_text(face = "bold", color = "cyan4"),
        legend.text = element_text(size = 3),
        legend.position = "bottom")
# - simple
gg_Log_tune_latin_simple_F <- log_tune_latin_simple %>% 
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
  labs(title = "Latin Grid",
       subtitle = "Logistic Regression - Simple",
       y = "F Score") +
  ylim(c(0.78, 0.805)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.4, color = "darkolivegreen"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.title = element_text(face = "bold", color = "cyan4"),
        legend.text = element_text(size = 3),
        legend.position = "bottom")
# - VISUAL
ggarrange(gg_Log_tune_latin_normal_F, gg_Log_tune_latin_pca_F, gg_Log_tune_latin_simple_F, nrow = 1)



# Custom Grid
# - normal
log_tune_custom_normal <-
  log_wflow %>% 
  tune_grid(
    resamples = nfl_10fold,
    grid = log_grid_custom,
    metrics = nfl_metrics,
    control = nfl_ctrl
  )
# - pca
log_tune_custom_pca <- 
  log_wflow %>% 
  update_recipe(nfl_log_recipe_pca) %>%
  tune_grid(
    resamples = nfl_10fold,
    grid = log_grid_custom,
    metrics = nfl_metrics,
    control = nfl_ctrl
  )
# - simple
log_tune_custom_simple <- 
  log_wflow %>% 
  update_recipe(nfl_log_recipe_simple) %>%
  tune_grid(
    resamples = nfl_10fold,
    grid = log_grid_custom,
    metrics = nfl_metrics,
    control = nfl_ctrl
  )


# PLOT: Accuracy
# - normal
gg_Log_tune_custom_normal_Acc <- log_tune_custom_normal %>% 
  collect_metrics() %>% 
  filter(.metric == "accuracy") %>%
  mutate(mixture = factor(mixture)) %>% 
  ggplot(aes(penalty, mean)) +
  geom_line(aes(color = mixture)) + geom_point() +
  labs(title = "Custom Grid",
       subtitle = "Logistic Regression - Normal",
       y = "Accuracy") +
  ylim(c(0.65, 0.715)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.4, color = "darkolivegreen"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.title = element_text(face = "bold", color = "cyan4"),
        legend.position = "bottom")
# - pca
gg_Log_tune_custom_pca_Acc <- log_tune_custom_pca %>% 
  collect_metrics() %>% 
  filter(.metric == "accuracy") %>%
  mutate(mixture = factor(mixture)) %>% 
  ggplot(aes(penalty, mean)) +
  geom_line(aes(color = mixture)) + geom_point() +
  labs(title = "Custom Grid",
       subtitle = "Logistic Regression - PCA",
       y = "Accuracy") +
  ylim(c(0.65, 0.715)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.4, color = "darkolivegreen"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.title = element_text(face = "bold", color = "cyan4"),
        legend.position = "bottom")
# - normal
gg_Log_tune_custom_simple_Acc <- log_tune_custom_simple %>% 
  collect_metrics() %>% 
  filter(.metric == "accuracy") %>%
  mutate(mixture = factor(mixture)) %>% 
  ggplot(aes(penalty, mean)) +
  geom_line(aes(color = mixture)) + geom_point() +
  labs(title = "Custom Grid",
       subtitle = "Logistic Regression - Simple",
       y = "Accuracy") +
  ylim(c(0.65, 0.715)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.4, color = "darkolivegreen"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.title = element_text(face = "bold", color = "cyan4"),
        legend.position = "bottom")
# - VISUAL
ggarrange(gg_Log_tune_custom_normal_Acc, gg_Log_tune_custom_pca_Acc, gg_Log_tune_custom_simple_Acc, nrow = 1)

# PLOT: F Score
# - normal
gg_Log_tune_custom_normal_F <- log_tune_custom_normal %>% 
  collect_metrics() %>% 
  filter(.metric == "f_meas") %>%
  mutate(mixture = factor(mixture)) %>% 
  ggplot(aes(penalty, mean)) +
  geom_line(aes(color = mixture)) + geom_point() +
  labs(title = "Custom Grid",
       subtitle = "Logistic Regression - Normal",
       y = "F Score") +
  ylim(c(0.78, 0.81)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.4, color = "darkolivegreen"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.title = element_text(face = "bold", color = "cyan4"),
        legend.position = "bottom")
# - pca
gg_Log_tune_custom_pca_F <- log_tune_custom_pca %>% 
  collect_metrics() %>% 
  filter(.metric == "f_meas") %>%
  mutate(mixture = factor(mixture)) %>% 
  ggplot(aes(penalty, mean)) +
  geom_line(aes(color = mixture)) + geom_point() +
  labs(title = "Custom Grid",
       subtitle = "Logistic Regression - PCA",
       y = "F Score") +
  ylim(c(0.78, 0.81)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.4, color = "darkolivegreen"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.title = element_text(face = "bold", color = "cyan4"),
        legend.position = "bottom")
# - simple
gg_Log_tune_custom_simple_F <- log_tune_custom_simple %>% 
  collect_metrics() %>% 
  filter(.metric == "f_meas") %>%
  mutate(mixture = factor(mixture)) %>% 
  ggplot(aes(penalty, mean)) +
  geom_line(aes(color = mixture)) + geom_point() +
  labs(title = "Custom Grid",
       subtitle = "Logistic Regression - Simple",
       y = "F Score") +
  ylim(c(0.78, 0.81)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.4, color = "darkolivegreen"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.title = element_text(face = "bold", color = "cyan4"),
        legend.position = "bottom")
# - VISUAL
ggarrange(gg_Log_tune_custom_normal_F, gg_Log_tune_custom_pca_F, gg_Log_tune_custom_simple_F, nrow = 1)





# METRICS: Best
# - best: Accuracy
log_tune_latin_normal %>% show_best(metric = "accuracy", n = 1)  # 003 | 989 | 71.3
log_tune_latin_pca %>% show_best(metric = "accuracy", n = 1)     # 000 | 101 | 68.8
log_tune_latin_simple %>% show_best(metric = "accuracy", n = 1)  # 000 | 0.925 | 70.3
log_tune_custom_normal %>% show_best(metric = "accuracy", n = 1) # 001 | 1 | 71.2
log_tune_custom_pca %>% show_best(metric = "accuracy", n = 1)    # 026 | 0.5 | 69
log_tune_custom_simple %>% show_best(metric = "accuracy", n = 1) # 001 1 | 70.1

# - best: F Score
log_tune_latin_normal %>% show_best(metric = "f_meas", n = 1)  # 014 | 559 | 80.4
log_tune_latin_pca %>% show_best(metric = "f_meas", n = 1)     # 086 | 204 | 79.6
log_tune_latin_simple %>% show_best(metric = "f_meas", n = 1)  # 014 | 559 | 79.7
log_tune_custom_normal %>% show_best(metric = "f_meas", n = 1) # 001 | 1 | 80.6
log_tune_custom_pca %>% show_best(metric = "f_meas", n = 1)    # 026 | 0.5 | 79.9
log_tune_custom_simple %>% show_best(metric = "f_meas", n = 1) # 011 | 1 | 80



# FINAL Fit

# Accuracy 71.3%: penaly = 0.003, mix = 0.989
# - hyperparameter
log_best_Acc_1 <- log_tune_latin_normal %>% select_best(metric = "accuracy")
# - fit
log_fit_Acc_1 <- 
  log_wflow %>% 
  finalize_workflow(log_best_Acc_1) %>% 
  fit(nfl_train)


# Accuracy 71.2%: penaly = 0.001, mix = 1
# - hyperparameter
log_best_Acc_2 <- log_tune_custom_normal %>% select_best(metric = "accuracy")
# - fit
log_fit_Acc_2 <- 
  log_wflow %>% 
  finalize_workflow(log_best_Acc_2) %>% 
  fit(nfl_train)


# F Score 80.4%: penaly = 0.014, mix = 0.559
# - hyperparameter
log_best_F_1 <- log_tune_latin_normal %>% select_best(metric = "f_meas")
# - fit
log_fit_F_1 <- 
  log_wflow %>% 
  finalize_workflow(log_best_F_1) %>% 
  fit(nfl_train)

# F Score 80.6%: penaly = 0.011, mix = 1
# - hyperparameter
log_best_F_2 <- log_tune_custom_normal %>% select_best(metric = "f_meas")
# - fit
log_fit_F_2 <- 
  log_wflow %>% 
  finalize_workflow(log_best_F_2) %>% 
  fit(nfl_train)


#
# Modeling: Fit - Random Forest ----

# Start Parallel Processing
cl_4 <- makeCluster(4)
registerDoParallel(cl_4)

# LATIN Fit
# - normal
rf_tune_latin_normal <-
  rf_wflow %>% 
  tune_grid(
    resamples = nfl_10fold,
    grid = rf_grid_latin,
    metrics = nfl_metrics,
    control = nfl_ctrl
  )
# - pca 
rf_tune_latin_pca <-
  rf_wflow %>% 
  update_recipe(nfl_rf_recipe_pca) %>% 
  tune_grid(
    resamples = nfl_10fold,
    grid = rf_grid_latin,
    metrics = nfl_metrics,
    control = nfl_ctrl
  )
# - simple
rf_tune_latin_simple <-
  rf_wflow %>% 
  update_recipe(nfl_rf_recipe_simple) %>% 
  tune_grid(
    resamples = nfl_10fold,
    grid = rf_grid_latin,
    metrics = nfl_metrics,
    control = nfl_ctrl
  )

# PLOT: Accuracy
# - normal
gg_RF_tune_latin_normal_Acc <- rf_tune_latin_normal %>% 
  collect_metrics() %>% 
  filter(.metric == "accuracy") %>%
  mutate(mtry = factor(mtry)) %>% 
  ggplot(aes(min_n, mean)) +
  geom_line(aes(color = mtry)) + geom_point() +
  labs(title = "Latin Grid",
       subtitle = "Random Forrest - None",
       y = "Accuracy") +
  ylim(c(0.66, 0.71)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.4, color = "darkolivegreen"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.title = element_text(face = "bold", color = "cyan4"),
        legend.text = element_text(size = 7),
        legend.position = "bottom")
# - PCA
gg_RF_tune_latin_pca_Acc <- rf_tune_latin_pca %>% 
  collect_metrics() %>% 
  filter(.metric == "accuracy") %>%
  mutate(mtry = factor(mtry)) %>% 
  ggplot(aes(min_n, mean)) +
  geom_line(aes(color = mtry)) + geom_point() +
  labs(title = "Latin Grid",
       subtitle = "Random Forrest - PCA",
       y = "Accuracy") +
  ylim(c(0.66, 0.71)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.4, color = "darkolivegreen"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.title = element_text(face = "bold", color = "cyan4"),
        legend.text = element_text(size = 7),
        legend.position = "bottom")
# - Simple
gg_RF_tune_latin_simple_Acc <- rf_tune_latin_simple %>% 
  collect_metrics() %>% 
  filter(.metric == "accuracy") %>%
  mutate(mtry = factor(mtry)) %>% 
  ggplot(aes(min_n, mean)) +
  geom_line(aes(color = mtry)) + geom_point() +
  labs(title = "Latin Grid",
       subtitle = "Random Forrest - Simple",
       y = "Accuracy") +
  ylim(c(0.66, 0.71)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.4, color = "darkolivegreen"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.title = element_text(face = "bold", color = "cyan4"),
        legend.text = element_text(size = 7),
        legend.position = "bottom")
# - Visual
ggarrange(gg_RF_tune_latin_normal_Acc, gg_RF_tune_latin_pca_Acc, gg_RF_tune_latin_simple_Acc, nrow = 1)


# PLOT: F Score
# - normal
gg_RF_tune_latin_normal_F <- rf_tune_latin_normal %>% 
  collect_metrics() %>% 
  filter(.metric == "f_meas") %>%
  mutate(mtry = factor(mtry)) %>% 
  ggplot(aes(min_n, mean)) +
  geom_line(aes(color = mtry)) + geom_point() +
  labs(title = "Latin Grid",
       subtitle = "Random Forrest - None",
       y = "F Score") +
  ylim(c(0.75, 0.80)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.4, color = "darkolivegreen"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.title = element_text(face = "bold", color = "cyan4"),
        legend.text = element_text(size = 7),
        legend.position = "bottom")
# - PCA
gg_RF_tune_latin_pca_F <- rf_tune_latin_pca %>% 
  collect_metrics() %>% 
  filter(.metric == "f_meas") %>%
  mutate(mtry = factor(mtry)) %>% 
  ggplot(aes(min_n, mean)) +
  geom_line(aes(color = mtry)) + geom_point() +
  labs(title = "Latin Grid",
       subtitle = "Random Forrest - PCA",
       y = "F Score") +
  ylim(c(0.75, 0.80)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.4, color = "darkolivegreen"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.title = element_text(face = "bold", color = "cyan4"),
        legend.text = element_text(size = 7),
        legend.position = "bottom")
# - Simple
gg_RF_tune_latin_simple_F <- rf_tune_latin_simple %>% 
  collect_metrics() %>% 
  filter(.metric == "f_meas") %>%
  mutate(mtry = factor(mtry)) %>% 
  ggplot(aes(min_n, mean)) +
  geom_line(aes(color = mtry)) + geom_point() +
  labs(title = "Latin Grid",
       subtitle = "Random Forrest - Simple",
       y = "F Score") +
  ylim(c(0.75, 0.80)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.4, color = "darkolivegreen"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.title = element_text(face = "bold", color = "cyan4"),
        legend.text = element_text(size = 7),
        legend.position = "bottom")

# - Visual
ggarrange(gg_RF_tune_latin_normal_F, gg_RF_tune_latin_pca_F, gg_RF_tune_latin_simple_F, nrow = 1)



# CUSTOM
# - normal
rf_tune_custom_normal <-
  rf_wflow %>% 
  tune_grid(
    resamples = nfl_10fold,
    grid = rf_grid_custom,
    metrics = nfl_metrics,
    control = nfl_ctrl
  )
# - PCA
rf_tune_custom_pca <- 
  rf_wflow %>% 
  update_recipe(nfl_rf_recipe_pca) %>%
  tune_grid(
    resamples = nfl_10fold,
    grid = rf_grid_custom,
    metrics = nfl_metrics,
    control = nfl_ctrl
  )
# - Simple
rf_tune_custom_simple <- 
  rf_wflow %>% 
  update_recipe(nfl_rf_recipe_simple) %>%
  tune_grid(
    resamples = nfl_10fold,
    grid = rf_grid_custom,
    metrics = nfl_metrics,
    control = nfl_ctrl
  )

# - plot: Accuracy
# - normal
gg_RF_tune_custom_normal_Acc <- rf_tune_custom_normal %>% 
  collect_metrics() %>% 
  filter(.metric == "accuracy") %>%
  mutate(mtry = factor(mtry)) %>% 
  ggplot(aes(min_n, mean)) +
  geom_line(aes(color = mtry)) + geom_point() +
  labs(title = "Custom Grid",
       subtitle = "Random Forrest - None",
       y = "Accuracy") +
  ylim(c(0.66, 0.71)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.4, color = "darkolivegreen"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.title = element_text(face = "bold", color = "cyan4"),
        legend.text = element_text(size = 7),
        legend.position = "bottom")
# - PCA
gg_RF_tune_custom_pca_Acc <- rf_tune_custom_pca %>% 
  collect_metrics() %>% 
  filter(.metric == "accuracy") %>%
  mutate(mtry = factor(mtry)) %>% 
  ggplot(aes(min_n, mean)) +
  geom_line(aes(color = mtry)) + geom_point() +
  labs(title = "Custom Grid",
       subtitle = "Random Forrest - PCA",
       y = "Accuracy") +
  ylim(c(0.66, 0.71)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.4, color = "darkolivegreen"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.title = element_text(face = "bold", color = "cyan4"),
        legend.text = element_text(size = 7),
        legend.position = "bottom")
# - Simple
gg_RF_tune_custom_simple_Acc <- rf_tune_custom_simple %>% 
  collect_metrics() %>% 
  filter(.metric == "accuracy") %>%
  mutate(mtry = factor(mtry)) %>% 
  ggplot(aes(min_n, mean)) +
  geom_line(aes(color = mtry)) + geom_point() +
  labs(title = "Custom Grid",
       subtitle = "Random Forrest - Simple",
       y = "Accuracy") +
  ylim(c(0.66, 0.71)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.4, color = "darkolivegreen"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.title = element_text(face = "bold", color = "cyan4"),
        legend.text = element_text(size = 7),
        legend.position = "bottom")
# - Visuals
ggarrange(gg_RF_tune_custom_normal_Acc, gg_RF_tune_custom_pca_Acc, gg_RF_tune_custom_simple_Acc, nrow = 1)

# - PLOT: F Score
# - normal
gg_RF_tune_custom_normal_F <- rf_tune_custom_normal %>% 
  collect_metrics() %>% 
  filter(.metric == "f_meas") %>%
  mutate(mtry = factor(mtry)) %>% 
  ggplot(aes(min_n, mean)) +
  geom_line(aes(color = mtry)) + geom_point() +
  labs(title = "Custom Grid",
       subtitle = "Random Forrest - None",
       y = "F Score") +
  ylim(c(0.75, 0.80)) + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.4, color = "darkolivegreen"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.title = element_text(face = "bold", color = "cyan4"),
        legend.text = element_text(size = 7),
        legend.position = "bottom")
# - PCA
gg_RF_tune_custom_pca_F <- rf_tune_custom_pca %>% 
  collect_metrics() %>% 
  filter(.metric == "f_meas") %>%
  mutate(mtry = factor(mtry)) %>% 
  ggplot(aes(min_n, mean)) +
  geom_line(aes(color = mtry)) + geom_point() +
  labs(title = "Custom Grid",
       subtitle = "Random Forrest - PCA",
       y = "F Score") +
  ylim(c(0.75, 0.80)) + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.4, color = "darkolivegreen"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.title = element_text(face = "bold", color = "cyan4"),
        legend.text = element_text(size = 7),
        legend.position = "bottom")
# - Simple
gg_RF_tune_custom_simple_F <- rf_tune_custom_simple %>% 
  collect_metrics() %>% 
  filter(.metric == "f_meas") %>%
  mutate(mtry = factor(mtry)) %>% 
  ggplot(aes(min_n, mean)) +
  geom_line(aes(color = mtry)) + geom_point() +
  labs(title = "Custom Grid",
       subtitle = "Random Forrest - Simple",
       y = "F Score") +
  ylim(c(0.75, 0.80)) + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.4, color = "darkolivegreen"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.title = element_text(face = "bold", color = "cyan4"),
        legend.text = element_text(size = 7),
        legend.position = "bottom")
# - Visual
ggarrange(gg_RF_tune_custom_normal_F, gg_RF_tune_custom_pca_F, gg_RF_tune_custom_simple_F, nrow = 1)


# METRICS
# - best: Accuracy
rf_tune_latin_normal %>% show_best("accuracy", n = 1)  # 70.3 | 4 | 6
rf_tune_latin_pca %>% show_best("accuracy", n = 1)     # 68.5 | 1 | 11
rf_tune_latin_simple %>% show_best("accuracy", n = 1)  # 70.4 | 2 | 36
rf_tune_custom_normal %>% show_best("accuracy", n = 1) # 69.8 | 3 | 9
rf_tune_custom_pca %>% show_best("accuracy", n = 1)    # 68.4 | 1 | 7
rf_tune_custom_simple %>% show_best("accuracy", n = 1) # 70.3 | 1 | 1

# - best: F Score
rf_tune_latin_normal %>% show_best("f_meas", n = 1)    # 79.8 | 1 | 40
rf_tune_latin_pca %>% show_best("f_meas", n = 1)       # 79.4 | 1 | 34
rf_tune_latin_simple %>% show_best("f_meas", n = 1)    # 79.8 | 1 | 11` 
rf_tune_custom_normal %>% show_best("f_meas", n = 1)   # 79.5 | 1 | 3
rf_tune_custom_pca %>% show_best("f_meas", n = 1)      # 79.3 | 1 | 7
rf_tune_custom_simple %>% show_best("f_meas", n = 1)   # 80.0 | 1 | 1





# FINAL Fit

# Accuracy 70.4%: Mtry = 2, Min_N = 36
# - hyperparamter
rf_best_Acc_1 <- rf_tune_latin_simple %>% select_best(metric = "accuracy")
# - fit
rf_fit_Acc_1 <- 
  rf_wflow %>% 
  finalize_workflow(rf_best_Acc_1) %>% 
  fit(nfl_train)


# Accuracy 70.3%: Mtry = 1, Min_N = 1
# - hyperparamter
rf_best_Acc_2 <- rf_tune_custom_simple %>% select_best("accuracy")
# - fit
rf_fit_Acc_2 <- 
  rf_wflow %>% 
  finalize_workflow(rf_best_Acc_2) %>% 
  fit(nfl_train)


# F Score 80%: Mtry = 1, Min_N = 1
# - hyperparameter
rf_best_F_1 <- rf_tune_custom_simple %>% select_best(metric = "f_meas")
# - fit
rf_fit_F_1 <- 
  rf_wflow %>% 
  finalize_workflow(rf_best_F_1) %>% 
  fit(nfl_train)

# F Score 79.8%: Mtry = 1, Min_N = 11
# - hyperparameter
rf_best_F_2 <- rf_tune_latin_simple %>% select_best(metric = "f_meas")
# - fit
rf_fit_F_2 <- 
  rf_wflow %>% 
  finalize_workflow(rf_best_F_2) %>% 
  fit(nfl_train)

# Stop Parallell Processing
stopCluster(cl_4)

# Modeling: Validation Diagnostics - Logistic Regression ----

# PREDICTIONS
log_Results <- 
  tibble(Drafted = nfl_val$drafted,
         LOG_Acc_1_Pred = predict(log_fit_Acc_1, new_data = nfl_val) %>% pull(),
         LOG_Acc_1_Prob = predict(log_fit_Acc_1, new_data = nfl_val, type = "prob") %>% pull(.pred_Yes),
         LOG_Acc_2_Pred = predict(log_fit_Acc_2, new_data = nfl_val) %>% pull(),
         LOG_Acc_2_Prob = predict(log_fit_Acc_2, new_data = nfl_val, type = "prob") %>% pull(.pred_Yes),
         LOG_F_1_Pred = predict(log_fit_F_1, new_data = nfl_val) %>% pull(),
         LOG_F_1_Prob = predict(log_fit_F_1, new_data = nfl_val, type = "prob") %>% pull(.pred_Yes),
         LOG_F_2_Pred = predict(log_fit_F_2, new_data = nfl_val) %>% pull(),
         LOG_F_2_Prob = predict(log_fit_F_2, new_data = nfl_val, type = "prob") %>% pull(.pred_Yes))

# CONF MATRIX

# - Accuracy
log_CM_Acc_1 <- log_Results %>% 
  conf_mat(truth = Drafted, estimate = LOG_Acc_1_Pred) %>% 
  autoplot(type = "heatmap") +
  labs(title = "Accuracy", subtitle = "Penalty = 0.003 | Mix = 0.989") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, color = "darkolivegreen", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "skyblue"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.position = "none")

log_CM_Acc_2 <- log_Results %>% 
  conf_mat(truth = Drafted, estimate = LOG_Acc_2_Pred) %>% 
  autoplot(type = "heatmap") +
  labs(title = "Accuracy", subtitle = "Penalty = 0.001 | Mix = 1") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, color = "darkolivegreen", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "skyblue"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.position = "none")
# - F Score
log_CM_F_1 <- log_Results %>% 
  conf_mat(truth = Drafted, estimate = LOG_F_1_Pred) %>% 
  autoplot(type = "heatmap") +
  labs(title = "F Score", subtitle = "Penalty = 0.014 | Mix = 0.559") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, color = "darkolivegreen", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "skyblue"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.position = "none")

log_CM_F_2 <- log_Results %>% 
  conf_mat(truth = Drafted, estimate = LOG_F_2_Pred) %>% 
  autoplot(type = "heatmap") +
  labs(title = "F Score", subtitle = "Penalty = 0.011 | Mix = 1") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, color = "darkolivegreen", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "skyblue"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.position = "none")

# - Visual
ggarrange(log_CM_Acc_1, log_CM_Acc_2, log_CM_F_1, log_CM_F_2, 
          nrow = 2, ncol = 2)

# ROC Curve
# - Accuracy
log_ROC_Acc_1 <- log_Results %>%
  roc_curve(Drafted, LOG_Acc_1_Prob) %>% 
  mutate(Model = "Acc_p003_m989")

log_ROC_Acc_2 <- log_Results %>%
  roc_curve(Drafted, LOG_Acc_2_Prob) %>% 
  mutate(Model = "Acc_p001_m1")

# - F Score
log_ROC_F_1 <- log_Results %>%
  roc_curve(Drafted, LOG_F_1_Prob) %>% 
  mutate(Model = "F_p014_m559")

log_ROC_F_2 <- log_Results %>%
  roc_curve(Drafted, LOG_F_2_Prob) %>% 
  mutate(Model = "F_p011_m1")

# - Visual
log_ROC_Acc_1 %>% 
  bind_rows(log_ROC_Acc_2, log_ROC_F_1, log_ROC_F_2) %>% 
  mutate(specificity = 1 - specificity) %>% 
  ggplot(aes(specificity,sensitivity, color = Model)) +
  geom_line() + geom_abline(slope = 1, linetype = 2, alpha = 0.2) +
  labs(
    title = "Logistic Regression",
    x = "1 - specificity"
  ) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "tomato"), 
        legend.position = "bottom")


# Metrics
log_Metrics <-
# - Accuracy
  tibble(Model = "Logistic_Regression_Acc_1",
         AUC = roc_auc(log_Results, Drafted, LOG_Acc_1_Prob) %>% pull(.estimate) %>% round(3),
         Accuracy = accuracy(log_Results, truth = Drafted, estimate = LOG_Acc_1_Pred) %>% pull(.estimate) %>% round(3),
         Sensitivity = sens(log_Results, truth = Drafted, estimate = LOG_Acc_1_Pred) %>% pull(.estimate) %>% round(3),
         Specificity = spec(log_Results, truth = Drafted, estimate = LOG_Acc_1_Pred) %>% pull(.estimate) %>% round(3),
         Precision = precision(log_Results, truth = Drafted, estimate = LOG_Acc_1_Pred) %>% pull(.estimate) %>% round(3),
         Recall = recall(log_Results, truth = Drafted, estimate = LOG_Acc_1_Pred) %>% pull(.estimate) %>% round(3),
         F1 = f_meas(log_Results, truth = Drafted, estimate = LOG_Acc_1_Pred) %>% pull(.estimate) %>% round(3))


log_Metrics <- log_Metrics %>% 
  bind_rows(
    tibble(Model = "Logistic_Regression_Acc_2",
           AUC = roc_auc(log_Results, Drafted, LOG_Acc_2_Prob) %>% pull(.estimate) %>% round(3),
           Accuracy = accuracy(log_Results, truth = Drafted, estimate = LOG_Acc_2_Pred) %>% pull(.estimate) %>% round(3),
           Sensitivity = sens(log_Results, truth = Drafted, estimate = LOG_Acc_2_Pred) %>% pull(.estimate) %>% round(3),
           Specificity = spec(log_Results, truth = Drafted, estimate = LOG_Acc_2_Pred) %>% pull(.estimate) %>% round(3),
           Precision = precision(log_Results, truth = Drafted, estimate = LOG_Acc_2_Pred) %>% pull(.estimate) %>% round(3),
           Recall = recall(log_Results, truth = Drafted, estimate = LOG_Acc_2_Pred) %>% pull(.estimate) %>% round(3),
           F1 = f_meas(log_Results, truth = Drafted, estimate = LOG_Acc_2_Pred) %>% pull(.estimate) %>% round(3))
    )

# - F Score    
log_Metrics <- log_Metrics %>% 
  bind_rows(
    tibble(Model = "Logistic_Regression_F_1",
           AUC = roc_auc(log_Results, Drafted, LOG_F_1_Prob) %>% pull(.estimate) %>% round(3),
           Accuracy = accuracy(log_Results, truth = Drafted, estimate = LOG_F_1_Pred) %>% pull(.estimate) %>% round(3),
           Sensitivity = sens(log_Results, truth = Drafted, estimate = LOG_F_1_Pred) %>% pull(.estimate) %>% round(3),
           Specificity = spec(log_Results, truth = Drafted, estimate = LOG_F_1_Pred) %>% pull(.estimate) %>% round(3),
           Precision = precision(log_Results, truth = Drafted, estimate = LOG_F_1_Pred) %>% pull(.estimate) %>% round(3),
           Recall = recall(log_Results, truth = Drafted, estimate = LOG_F_1_Pred) %>% pull(.estimate) %>% round(3),
           F1 = f_meas(log_Results, truth = Drafted, estimate = LOG_F_1_Pred) %>% pull(.estimate) %>% round(3))
  )

log_Metrics <- log_Metrics %>% 
  bind_rows(
    tibble(Model = "Logistic_Regression_F_2",
           AUC = roc_auc(log_Results, Drafted, LOG_F_2_Prob) %>% pull(.estimate) %>% round(3),
           Accuracy = accuracy(log_Results, truth = Drafted, estimate = LOG_F_2_Pred) %>% pull(.estimate) %>% round(3),
           Sensitivity = sens(log_Results, truth = Drafted, estimate = LOG_F_2_Pred) %>% pull(.estimate) %>% round(3),
           Specificity = spec(log_Results, truth = Drafted, estimate = LOG_F_2_Pred) %>% pull(.estimate) %>% round(3),
           Precision = precision(log_Results, truth = Drafted, estimate = LOG_F_2_Pred) %>% pull(.estimate) %>% round(3),
           Recall = recall(log_Results, truth = Drafted, estimate = LOG_F_2_Pred) %>% pull(.estimate) %>% round(3),
           F1 = f_meas(log_Results, truth = Drafted, estimate = LOG_F_2_Pred) %>% pull(.estimate) %>% round(3))
  )

#
# Modeling: Validation Diagnostics - Random Forrest ----

# PREDICTIONS
rf_Results <- 
  tibble(Drafted = nfl_val$drafted,
         RF_Acc_1_Pred = predict(rf_fit_Acc_1, new_data = nfl_val) %>% pull(),
         RF_Acc_1_Prob = predict(rf_fit_Acc_1, new_data = nfl_val, type = "prob") %>% pull(.pred_Yes),
         RF_Acc_2_Pred = predict(rf_fit_Acc_2, new_data = nfl_val) %>% pull(),
         RF_Acc_2_Prob = predict(rf_fit_Acc_2, new_data = nfl_val, type = "prob") %>% pull(.pred_Yes),
         RF_F_1_Pred = predict(rf_fit_F_1, new_data = nfl_val) %>% pull(),
         RF_F_1_Prob = predict(rf_fit_F_1, new_data = nfl_val, type = "prob") %>% pull(.pred_Yes),
         RF_F_2_Pred = predict(rf_fit_F_2, new_data = nfl_val) %>% pull(),
         RF_F_2_Prob = predict(rf_fit_F_2, new_data = nfl_val, type = "prob") %>% pull(.pred_Yes))


# CONF MATRIX
# - Accuracy
rf_CM_Acc_1 <- rf_Results %>% 
  conf_mat(truth = Drafted, estimate = RF_Acc_1_Pred) %>% 
  autoplot(type = "heatmap") +
  labs(title = "Accuracy", subtitle = "Mtry = 2 | Min_n = 36") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, color = "darkolivegreen", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "skyblue"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.position = "none")
rf_CM_Acc_2 <- rf_Results %>% 
  conf_mat(truth = Drafted, estimate = RF_Acc_2_Pred) %>% 
  autoplot(type = "heatmap") +
  labs(title = "Accuracy", subtitle = "Mtry = 1 | Min_n = 1") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, color = "darkolivegreen", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "skyblue"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.position = "none")

# - F Score
rf_CM_F_1 <- rf_Results %>% 
  conf_mat(truth = Drafted, estimate = RF_F_1_Pred) %>% 
  autoplot(type = "heatmap") +
  labs(title = "F Score", subtitle = "Mtry = 1 | Min_n = 1") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, color = "darkolivegreen", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "skyblue"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.position = "none")
rf_CM_F_2 <- rf_Results %>% 
  conf_mat(truth = Drafted, estimate = RF_F_2_Pred) %>% 
  autoplot(type = "heatmap") +
  labs(title = "F Score", subtitle = "Mtry = 1 | Min_n = 11") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, color = "darkolivegreen", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "skyblue"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.position = "none")
# - Visual
ggarrange(rf_CM_Acc_1, rf_CM_Acc_2, rf_CM_F_1, rf_CM_F_2, 
          nrow = 2, ncol = 2)

# ROC Curve
# - Accuracy
rf_ROC_Acc_1 <- rf_Results %>%
  roc_curve(Drafted, RF_Acc_1_Prob) %>% 
  mutate(Model = "Acc_mtry2_min36")

rf_ROC_Acc_2 <- rf_Results %>%
  roc_curve(Drafted, RF_Acc_2_Prob) %>% 
  mutate(Model = "Acc_mtry1_min1")

# - F Score
rf_ROC_F_1 <- rf_Results %>%
  roc_curve(Drafted, RF_F_1_Prob) %>% 
  mutate(Model = "F_mtry1_min1")

rf_ROC_F_2 <- rf_Results %>%
  roc_curve(Drafted, RF_F_2_Prob) %>% 
  mutate(Model = "F_mtry1_min11")

# - Visual
rf_ROC_Acc_1 %>% 
  bind_rows(rf_ROC_Acc_2, rf_ROC_F_1, rf_ROC_F_2) %>% 
  mutate(specificity = 1 - specificity) %>% 
  ggplot(aes(specificity,sensitivity, color = Model)) +
  geom_line() + geom_abline(slope = 1, linetype = 2, alpha = 0.2) +
  labs(
    title = "Random Forrest",
    x = "1 - specificity"
  ) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "tomato"), 
        legend.position = "bottom")


# Metrics
rf_Metrics <-
  # - Accuracy
  tibble(Model = "Random_Forrest_Acc_1",
         AUC = roc_auc(rf_Results, Drafted, RF_Acc_1_Prob) %>% pull(.estimate) %>% round(3),
         Accuracy = accuracy(rf_Results, truth = Drafted, estimate = RF_Acc_1_Pred) %>% pull(.estimate) %>% round(3),
         Sensitivity = sens(rf_Results, truth = Drafted, estimate = RF_Acc_1_Pred) %>% pull(.estimate) %>% round(3),
         Specificity = spec(rf_Results, truth = Drafted, estimate = RF_Acc_1_Pred) %>% pull(.estimate) %>% round(3),
         Precision = precision(rf_Results, truth = Drafted, estimate = RF_Acc_1_Pred) %>% pull(.estimate) %>% round(3),
         Recall = recall(rf_Results, truth = Drafted, estimate = RF_Acc_1_Pred) %>% pull(.estimate) %>% round(3),
         F1 = f_meas(rf_Results, truth = Drafted, estimate = RF_Acc_1_Pred) %>% pull(.estimate) %>% round(3))


rf_Metrics <- rf_Metrics %>% 
  bind_rows(
    tibble(Model = "Random_Forrest_Acc_2",
           AUC = roc_auc(rf_Results, Drafted, RF_Acc_2_Prob) %>% pull(.estimate) %>% round(3),
           Accuracy = accuracy(rf_Results, truth = Drafted, estimate = RF_Acc_2_Pred) %>% pull(.estimate) %>% round(3),
           Sensitivity = sens(rf_Results, truth = Drafted, estimate = RF_Acc_2_Pred) %>% pull(.estimate) %>% round(3),
           Specificity = spec(rf_Results, truth = Drafted, estimate = RF_Acc_2_Pred) %>% pull(.estimate) %>% round(3),
           Precision = precision(rf_Results, truth = Drafted, estimate = RF_Acc_2_Pred) %>% pull(.estimate) %>% round(3),
           Recall = recall(rf_Results, truth = Drafted, estimate = RF_Acc_2_Pred) %>% pull(.estimate) %>% round(3),
           F1 = f_meas(rf_Results, truth = Drafted, estimate = RF_Acc_2_Pred) %>% pull(.estimate) %>% round(3))
  )

# - F Score    
rf_Metrics <- rf_Metrics %>% 
  bind_rows(
    tibble(Model = "Random_Forrest_F_1",
           AUC = roc_auc(rf_Results, Drafted, RF_F_1_Prob) %>% pull(.estimate) %>% round(3),
           Accuracy = accuracy(rf_Results, truth = Drafted, estimate = RF_F_1_Pred) %>% pull(.estimate) %>% round(3),
           Sensitivity = sens(rf_Results, truth = Drafted, estimate = RF_F_1_Pred) %>% pull(.estimate) %>% round(3),
           Specificity = spec(rf_Results, truth = Drafted, estimate = RF_F_1_Pred) %>% pull(.estimate) %>% round(3),
           Precision = precision(rf_Results, truth = Drafted, estimate = RF_F_1_Pred) %>% pull(.estimate) %>% round(3),
           Recall = recall(rf_Results, truth = Drafted, estimate = RF_F_1_Pred) %>% pull(.estimate) %>% round(3),
           F1 = f_meas(rf_Results, truth = Drafted, estimate = RF_F_1_Pred) %>% pull(.estimate) %>% round(3))
  )

rf_Metrics <- rf_Metrics %>% 
  bind_rows(
    tibble(Model = "Random_Forrest_F_2",
           AUC = roc_auc(rf_Results, Drafted, RF_F_2_Prob) %>% pull(.estimate) %>% round(3),
           Accuracy = accuracy(rf_Results, truth = Drafted, estimate = RF_F_2_Pred) %>% pull(.estimate) %>% round(3),
           Sensitivity = sens(rf_Results, truth = Drafted, estimate = RF_F_2_Pred) %>% pull(.estimate) %>% round(3),
           Specificity = spec(rf_Results, truth = Drafted, estimate = RF_F_2_Pred) %>% pull(.estimate) %>% round(3),
           Precision = precision(rf_Results, truth = Drafted, estimate = RF_F_2_Pred) %>% pull(.estimate) %>% round(3),
           Recall = recall(rf_Results, truth = Drafted, estimate = RF_F_2_Pred) %>% pull(.estimate) %>% round(3),
           F1 = f_meas(rf_Results, truth = Drafted, estimate = RF_F_2_Pred) %>% pull(.estimate) %>% round(3))
  )

#
# Modeling: Refit: Probabiliy Threshold - Logistic Regression ----


# Accuracy 71.3%: penaly = 0.003, mix = 0.989

# - thresholds
log_Thres_1 <- log_Results %>% 
  threshold_perf(Drafted, LOG_Acc_1_Prob, threshold = seq(0.2, 1, by = 0.05))
  
log_Thres_1 <- log_Thres_1 %>% 
  filter(.metric != "distance") %>% 
  mutate(group = case_when(
                   .metric == "sens" | .metric == "spec" ~ "1",
                    TRUE ~ "2")
  )
# - max j
max_j_log_1 <- log_Thres_1 %>% 
  filter(.metric == "j_index") %>% 
  filter(.estimate == max(.estimate)) %>% 
  pull(.threshold)
# - VISUAL
gg_Log_Thres_1 <- log_Thres_1 %>% 
  ggplot(aes(.threshold, .estimate, color = .metric, alpha = group)) +
  geom_line(linetype = 2, size = 1) +
  geom_vline(xintercept = max_j_log_1, alpha = 0.6, size = 2, color = "palegreen4") +
  geom_vline(xintercept = 0.5, alpha = 0.6, linetype = 1, size = 1, color = "grey20") +
  labs(title = "Logistic Regression - Optimal Threshold",
       subtitle = "Penalty = 0.003 | Mix = 0.989",
       color = "Metric",
       x = "Threshold") +
  scale_alpha_manual(values = c(.08, 1), guide = "none") +
  scale_color_manual(values = c("palegreen4","red","blue"),
                     labels = c("J_Index", "Sensitivity", "Specificity")) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "skyblue"),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "none"
  )

# Accuracy 71.2%: penaly = 0.001, mix = 1

# - thresholds
log_Thres_2 <- log_Results %>% 
  threshold_perf(Drafted, LOG_Acc_2_Prob, threshold = seq(0.2, 1, by = 0.05))

log_Thres_2 <- log_Thres_2 %>% 
  filter(.metric != "distance") %>% 
  mutate(group = case_when(
    .metric == "sens" | .metric == "spec" ~ "1",
    TRUE ~ "2")
  )
# - max j
max_j_log_2 <- log_Thres_2 %>% 
  filter(.metric == "j_index") %>% 
  filter(.estimate == max(.estimate)) %>% 
  pull(.threshold)
# - VISUAL
gg_Log_Thres_2 <- log_Thres_2 %>% 
  ggplot(aes(.threshold, .estimate, color = .metric, alpha = group)) +
  geom_line(linetype = 2, size = 1) +
  geom_vline(xintercept = max_j_log_2, alpha = 0.6, size = 2, color = "palegreen4") +
  geom_vline(xintercept = 0.5, alpha = 0.6, linetype = 1, size = 1, color = "grey20") +
  labs(title = "Logistic Regression - Optimal Threshold",
       subtitle = "Penalty = 0.001 | Mix = 1",
       color = "Metric",
       x = "Threshold") +
  scale_alpha_manual(values = c(.08, 1), guide = "none") +
  scale_color_manual(values = c("palegreen4","red","blue"),
                     labels = c("J_Index", "Sensitivity", "Specificity")) +
  theme_bw() +
  theme(
    plot.title = element_blank(),
    plot.subtitle = element_text(hjust = 0.5, color = "skyblue"),
    axis.title.y = element_blank(),
    legend.position = "bottom"
  )

# - VISUAL
ggarrange(gg_Log_Thres_1, gg_Log_Thres_2, ncol = 1)

# PREDICTIONS
log_Results_Thres <- 
  tibble(Drafted = nfl_val$drafted,
         LOG_Acc_1_Prob = predict(log_fit_Acc_1, new_data = nfl_val, type = "prob") %>% pull(.pred_Yes),
         LOG_Acc_2_Prob = predict(log_fit_Acc_2, new_data = nfl_val, type = "prob") %>% pull(.pred_Yes)) %>% 
  mutate(LOG_Acc_1_Pred7 = ifelse(LOG_Acc_1_Prob > 0.7, "Yes","No") %>% factor(levels = c("Yes","No")),
         LOG_Acc_2_Pred6 = ifelse(LOG_Acc_2_Prob > 0.6, "Yes","No") %>% factor(levels = c("Yes","No")))

# CM
log_CM_Thres_Acc_1 <- log_Results_Thres %>% 
  conf_mat(truth = Drafted, estimate = LOG_Acc_1_Pred7) %>% 
  autoplot(type = "heatmap") +
  labs(title = "Logistic Regression (Thres 70%)",
       subtitle = "Penalty = 0.003 | Mix = 0.989") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "darkolivegreen"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.position = "none")

log_CM_Thres_Acc_2 <- log_Results_Thres %>% 
  conf_mat(truth = Drafted, estimate = LOG_Acc_2_Pred6) %>% 
  autoplot(type = "heatmap") +
  labs(title = "Logistic Regression (Thres 60%)",
       subtitle = "Penalty = 0.001 | Mix = 1") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "darkolivegreen"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.position = "none")
# - VISUAL
ggarrange(log_CM_Thres_Acc_1, log_CM_Thres_Acc_2)

# Metrics
log_Thres_Metrics <-
  # - Accuracy: Penalty = 0.003 | Mix = 0.989
  tibble(Model = "Logistic_Regression_Acc_1_70",
         AUC = roc_auc(log_Results_Thres, Drafted, LOG_Acc_1_Prob) %>% pull(.estimate) %>% round(3),
         Accuracy = accuracy(log_Results_Thres, truth = Drafted, estimate = LOG_Acc_1_Pred7) %>% pull(.estimate) %>% round(3),
         Sensitivity = sens(log_Results_Thres, truth = Drafted, estimate = LOG_Acc_1_Pred7) %>% pull(.estimate) %>% round(3),
         Specificity = spec(log_Results_Thres, truth = Drafted, estimate = LOG_Acc_1_Pred7) %>% pull(.estimate) %>% round(3),
         Precision = precision(log_Results_Thres, truth = Drafted, estimate = LOG_Acc_1_Pred7) %>% pull(.estimate) %>% round(3),
         Recall = recall(log_Results_Thres, truth = Drafted, estimate = LOG_Acc_1_Pred7) %>% pull(.estimate) %>% round(3),
         F1 = f_meas(log_Results_Thres, truth = Drafted, estimate = LOG_Acc_1_Pred7) %>% pull(.estimate) %>% round(3)) 
  # - Accuracy: Penalty = 0.001 | Mix = 1
log_Thres_Metrics <- log_Thres_Metrics %>% 
  bind_rows(
    tibble(Model = "Logistic_Regression_Acc_2_60",
           AUC = roc_auc(log_Results_Thres, Drafted, LOG_Acc_2_Prob) %>% pull(.estimate) %>% round(3),
           Accuracy = accuracy(log_Results_Thres, truth = Drafted, estimate = LOG_Acc_2_Pred6) %>% pull(.estimate) %>% round(3),
           Sensitivity = sens(log_Results_Thres, truth = Drafted, estimate = LOG_Acc_2_Pred6) %>% pull(.estimate) %>% round(3),
           Specificity = spec(log_Results_Thres, truth = Drafted, estimate = LOG_Acc_2_Pred6) %>% pull(.estimate) %>% round(3),
           Precision = precision(log_Results_Thres, truth = Drafted, estimate = LOG_Acc_2_Pred6) %>% pull(.estimate) %>% round(3),
           Recall = recall(log_Results_Thres, truth = Drafted, estimate = LOG_Acc_2_Pred6) %>% pull(.estimate) %>% round(3),
           F1 = f_meas(log_Results_Thres, truth = Drafted, estimate = LOG_Acc_2_Pred6) %>% pull(.estimate) %>% round(3))
  )
  
#
# Modeling: Refit: Probabiliy Threshold - Random Forrest ----


# Accuracy 70.4%: Mtry = 2, Min_N = 36
# - thresholds
rf_Thres_1 <- rf_Results %>% 
  threshold_perf(Drafted, RF_Acc_1_Prob, threshold = seq(0.2, 1, by = 0.05))

rf_Thres_1 <- log_Thres_1 %>% 
  filter(.metric != "distance") %>% 
  mutate(group = case_when(
    .metric == "sens" | .metric == "spec" ~ "1",
    TRUE ~ "2")
  )
# - max j
max_j_rf_1 <- rf_Thres_1 %>% 
  filter(.metric == "j_index") %>% 
  filter(.estimate == max(.estimate)) %>% 
  pull(.threshold)
# - VISUAL
gg_RF_Thres_1 <- rf_Thres_1 %>% 
  ggplot(aes(.threshold, .estimate, color = .metric, alpha = group)) +
  geom_line(linetype = 2, size = 1) +
  geom_vline(xintercept = max_j_rf_1, alpha = 0.6, size = 2, color = "palegreen4") +
  geom_vline(xintercept = 0.5, alpha = 0.6, linetype = 1, size = 1, color = "grey20") +
  labs(title = "Random Forrest - Optimal Threshold",
       subtitle = "Mtry = 2 | Min = 36",
       color = "Metric",
       x = "Threshold") +
  scale_alpha_manual(values = c(.08, 1), guide = "none") +
  scale_color_manual(values = c("palegreen4","red","blue"),
                     labels = c("J_Index", "Sensitivity", "Specificity")) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "skyblue"),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "none"
  )

# Accuracy 70.3%: Mtry = 1, Min_N = 1
# - thresholds
rf_Thres_2 <- rf_Results %>% 
  threshold_perf(Drafted, RF_Acc_2_Prob, threshold = seq(0.2, 1, by = 0.05))

rf_Thres_2 <- rf_Thres_2 %>% 
  filter(.metric != "distance") %>% 
  mutate(group = case_when(
    .metric == "sens" | .metric == "spec" ~ "1",
    TRUE ~ "2")
  )
# - max j
max_j_rf_2 <- rf_Thres_2 %>% 
  filter(.metric == "j_index") %>% 
  filter(.estimate == max(.estimate)) %>% 
  pull(.threshold)
# - VISUAL
gg_RF_Thres_2 <- rf_Thres_2 %>% 
  ggplot(aes(.threshold, .estimate, color = .metric, alpha = group)) +
  geom_line(linetype = 2, size = 1) +
  geom_vline(xintercept = max_j_rf_2, alpha = 0.6, size = 2, color = "palegreen4") +
  geom_vline(xintercept = 0.5, alpha = 0.6, linetype = 1, size = 1, color = "grey20") +
  labs(title = "Random Forrest - Optimal Threshold",
       subtitle = "Mtry = 1 | Min = 1",
       color = "Metric",
       x = "Threshold") +
  scale_alpha_manual(values = c(.08, 1), guide = "none") +
  scale_color_manual(values = c("palegreen4","red","blue"),
                     labels = c("J_Index", "Sensitivity", "Specificity")) +
  theme_bw() +
  theme(
    plot.title = element_blank(),
    plot.subtitle = element_text(hjust = 0.5, color = "skyblue"),
    axis.title.y = element_blank(),
    legend.position = "bottom"
  )

# - VISUAL
ggarrange(gg_RF_Thres_1, gg_RF_Thres_2, ncol = 1)

# PREDICTIONS
rf_Results_Thres <- 
  tibble(Drafted = nfl_val$drafted,
         RF_Acc_1_Prob = predict(rf_fit_Acc_1, new_data = nfl_val, type = "prob") %>% pull(.pred_Yes),
         RF_Acc_2_Prob = predict(rf_fit_Acc_2, new_data = nfl_val, type = "prob") %>% pull(.pred_Yes)) %>% 
  mutate(RF_Acc_1_Pred7 = ifelse(RF_Acc_1_Prob > 0.7, "Yes","No") %>% factor(levels = c("Yes","No")),
         RF_Acc_2_Pred65 = ifelse(RF_Acc_2_Prob > 0.6, "Yes","No") %>% factor(levels = c("Yes","No")))

# CM
rf_CM_Thres_Acc_1 <- rf_Results_Thres %>% 
  conf_mat(truth = Drafted, estimate = RF_Acc_1_Pred7) %>% 
  autoplot(type = "heatmap") +
  labs(title = "Random Forrest (Thres 70%)",
       subtitle = "Mtry = 2 | Mix = 36") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "darkolivegreen"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.position = "none")

rf_CM_Thres_Acc_2 <- rf_Results_Thres %>% 
  conf_mat(truth = Drafted, estimate = RF_Acc_2_Pred65) %>% 
  autoplot(type = "heatmap") +
  labs(title = "Random Forrest (Thres 65%)",
       subtitle = "Mtry = 1 | Min = 1") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "darkolivegreen"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.position = "none")
# - VISUAL
ggarrange(rf_CM_Thres_Acc_1, rf_CM_Thres_Acc_2)

# Metrics
rf_Thres_Metrics <-
  # - Accuracy: Mtry = 2 | Min = 36
  tibble(Model = "Random_Forrest_Acc_1_70",
         AUC = roc_auc(rf_Results_Thres, Drafted, RF_Acc_1_Prob) %>% pull(.estimate) %>% round(3),
         Accuracy = accuracy(rf_Results_Thres, truth = Drafted, estimate = RF_Acc_1_Pred7) %>% pull(.estimate) %>% round(3),
         Sensitivity = sens(rf_Results_Thres, truth = Drafted, estimate = RF_Acc_1_Pred7) %>% pull(.estimate) %>% round(3),
         Specificity = spec(rf_Results_Thres, truth = Drafted, estimate = RF_Acc_1_Pred7) %>% pull(.estimate) %>% round(3),
         Precision = precision(rf_Results_Thres, truth = Drafted, estimate = RF_Acc_1_Pred7) %>% pull(.estimate) %>% round(3),
         Recall = recall(rf_Results_Thres, truth = Drafted, estimate = RF_Acc_1_Pred7) %>% pull(.estimate) %>% round(3),
         F1 = f_meas(rf_Results_Thres, truth = Drafted, estimate = RF_Acc_1_Pred7) %>% pull(.estimate) %>% round(3)) 
# - Accuracy: Mtry = 1 | Min = 1
rf_Thres_Metrics <- rf_Thres_Metrics %>% 
  bind_rows(
    tibble(Model = "Random_Forrest_Acc_2_65",
           AUC = roc_auc(rf_Results_Thres, Drafted, RF_Acc_2_Prob) %>% pull(.estimate) %>% round(3),
           Accuracy = accuracy(rf_Results_Thres, truth = Drafted, estimate = RF_Acc_2_Pred65) %>% pull(.estimate) %>% round(3),
           Sensitivity = sens(rf_Results_Thres, truth = Drafted, estimate = RF_Acc_2_Pred65) %>% pull(.estimate) %>% round(3),
           Specificity = spec(rf_Results_Thres, truth = Drafted, estimate = RF_Acc_2_Pred65) %>% pull(.estimate) %>% round(3),
           Precision = precision(rf_Results_Thres, truth = Drafted, estimate = RF_Acc_2_Pred65) %>% pull(.estimate) %>% round(3),
           Recall = recall(rf_Results_Thres, truth = Drafted, estimate = RF_Acc_2_Pred65) %>% pull(.estimate) %>% round(3),
           F1 = f_meas(rf_Results_Thres, truth = Drafted, estimate = RF_Acc_2_Pred65) %>% pull(.estimate) %>% round(3))
  )

#
# Modeling: FINAL Diagnostics ----

# PREDICTIONS
Log_FINAL_Results <- 
  tibble(Drafted = nfl_test$drafted,
         LOG_Acc_1_Prob = predict(log_fit_Acc_1, new_data = nfl_test, type = "prob") %>% pull(.pred_Yes),
         LOG_Acc_1_Pred = predict(log_fit_Acc_1, new_data = nfl_test) %>% pull(),
         LOG_Acc_1_Pred70 = ifelse(LOG_Acc_1_Prob > 0.7, "Yes","No") %>% factor(levels = c("Yes","No")),
         LOG_Acc_2_Prob = predict(log_fit_Acc_2, new_data = nfl_test, type = "prob") %>% pull(.pred_Yes),
         LOG_Acc_2_Pred = predict(log_fit_Acc_2, new_data = nfl_test) %>% pull(),
         LOG_Acc_2_Pred60 = ifelse(LOG_Acc_2_Prob > 0.6, "Yes","No") %>% factor(levels = c("Yes","No")),
         LOG_F_1_Prob = predict(log_fit_F_1, new_data = nfl_test, type = "prob") %>% pull(.pred_Yes),
         LOG_F_1_Pred = predict(log_fit_F_1, new_data = nfl_test) %>% pull(),
         LOG_F_2_Prob = predict(log_fit_F_2, new_data = nfl_test, type = "prob") %>% pull(.pred_Yes),
         LOG_F_2_Pred = predict(log_fit_F_2, new_data = nfl_test) %>% pull())

rf_FINAL_Results <- 
  tibble(Drafted = nfl_test$drafted,
         RF_Acc_1_Prob = predict(rf_fit_Acc_1, new_data = nfl_test, type = "prob") %>% pull(.pred_Yes),
         RF_Acc_1_Pred = predict(rf_fit_Acc_1, new_data = nfl_test) %>% pull(),
         RF_Acc_1_Pred70 = ifelse(RF_Acc_1_Prob > 0.7, "Yes","No") %>% factor(levels = c("Yes","No")),
         RF_Acc_2_Prob = predict(rf_fit_Acc_2, new_data = nfl_test, type = "prob") %>% pull(.pred_Yes),
         RF_Acc_2_Pred = predict(rf_fit_Acc_2, new_data = nfl_test) %>% pull(),
         RF_Acc_2_Pred65 = ifelse(RF_Acc_2_Prob > 0.65, "Yes","No") %>% factor(levels = c("Yes","No")),
         RF_F_1_Prob = predict(rf_fit_F_1, new_data = nfl_test, type = "prob") %>% pull(.pred_Yes),
         RF_F_1_Pred = predict(rf_fit_F_1, new_data = nfl_test) %>% pull(),
         RF_F_2_Prob = predict(rf_fit_F_2, new_data = nfl_test, type = "prob") %>% pull(.pred_Yes),
         RF_F_2_Pred = predict(rf_fit_F_2, new_data = nfl_test) %>% pull())

# CM
# - Logistic Regression
log_CM_FINAL_Acc_1 <- Log_FINAL_Results %>% 
  conf_mat(truth = Drafted, estimate = LOG_Acc_1_Pred) %>% 
  autoplot(type = "heatmap") +
  labs(title = "Accuracy 50%",
       subtitle = "Penalty = 0.003 | Mix = 0.989") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, color = "darkolivegreen", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "skyblue"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none")

log_CM_FINAL_Acc70_1 <- Log_FINAL_Results %>% 
  conf_mat(truth = Drafted, estimate = LOG_Acc_1_Pred70) %>% 
  autoplot(type = "heatmap") +
  labs(title = "Accuracy 70%",
       subtitle = "Penalty = 0.003 | Mix = 0.989") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, color = "darkolivegreen", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "skyblue"),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        legend.position = "none")

log_CM_FINAL_Acc_2 <- Log_FINAL_Results %>% 
  conf_mat(truth = Drafted, estimate = LOG_Acc_2_Pred) %>% 
  autoplot(type = "heatmap") +
  labs(title = "Accuracy 50%",
       subtitle = "Penalty = 0.001 | Mix = 1") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, color = "darkolivegreen", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "skyblue"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(),
        legend.position = "none")

log_CM_FINAL_Acc60_2 <- Log_FINAL_Results %>% 
  conf_mat(truth = Drafted, estimate = LOG_Acc_2_Pred60) %>% 
  autoplot(type = "heatmap") +
  labs(title = "Accuracy 60%",
       subtitle = "Penalty = 0.001 | Mix = 1") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, color = "darkolivegreen", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "skyblue"),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        legend.position = "none")

log_CM_FINAL_F_1 <- Log_FINAL_Results %>% 
  conf_mat(truth = Drafted, estimate = LOG_F_1_Pred) %>% 
  autoplot(type = "heatmap") +
  labs(title = "F Score",
       subtitle = "Penalty = 0.014 | Mix = 0.559") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, color = "darkolivegreen", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "skyblue"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.position = "none")

log_CM_FINAL_F_2 <- Log_FINAL_Results %>% 
  conf_mat(truth = Drafted, estimate = LOG_F_2_Pred) %>% 
  autoplot(type = "heatmap") +
  labs(title = "F Score",
       subtitle = "Penalty = 0.011 | Mix = 1") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, color = "darkolivegreen", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "skyblue"),
        axis.title = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(face = "bold", color = "cyan4"),
        legend.position = "none")

ggarrange(log_CM_FINAL_Acc_1, log_CM_FINAL_Acc70_1,
          log_CM_FINAL_Acc_2, log_CM_FINAL_Acc60_2,
          log_CM_FINAL_F_1, log_CM_FINAL_F_2,
          nrow = 3, ncol = 2)

# - Random Forrest
rf_CM_FINAL_Acc_1 <- rf_FINAL_Results %>% 
  conf_mat(truth = Drafted, estimate = RF_Acc_1_Pred) %>% 
  autoplot(type = "heatmap") +
  labs(title = "Accuracy 50%",
       subtitle = "Mtry = 2 | Min = 36") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, color = "darkolivegreen", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "skyblue"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none")

rf_CM_FINAL_Acc70_1 <- rf_FINAL_Results %>% 
  conf_mat(truth = Drafted, estimate = RF_Acc_1_Pred70) %>% 
  autoplot(type = "heatmap") +
  labs(title = "Accuracy 70%",
       subtitle = "Mtry = 2 | Min = 36") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, color = "darkolivegreen", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "skyblue"),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        legend.position = "none")

rf_CM_FINAL_Acc_2 <- rf_FINAL_Results %>% 
  conf_mat(truth = Drafted, estimate = RF_Acc_2_Pred) %>% 
  autoplot(type = "heatmap") +
  labs(title = "Accuracy 50%",
       subtitle = "Mtry = 1 | Min = 1") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, color = "darkolivegreen", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "skyblue"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(),
        legend.position = "none")

rf_CM_FINAL_Acc65_2 <- rf_FINAL_Results %>% 
  conf_mat(truth = Drafted, estimate = RF_Acc_2_Pred65) %>% 
  autoplot(type = "heatmap") +
  labs(title = "Accuracy 65%",
       subtitle = "Mtry = 1 | Min = 1") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, color = "darkolivegreen", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "skyblue"),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        legend.position = "none")

rf_CM_FINAL_F_1 <- rf_FINAL_Results %>% 
  conf_mat(truth = Drafted, estimate = RF_F_1_Pred) %>% 
  autoplot(type = "heatmap") +
  labs(title = "F Score",
       subtitle = "Mtry = 1 | Min = 1") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, color = "darkolivegreen", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "skyblue"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.position = "none")

rf_CM_FINAL_F_2 <- rf_FINAL_Results %>% 
  conf_mat(truth = Drafted, estimate = RF_F_2_Pred) %>% 
  autoplot(type = "heatmap") +
  labs(title = "F Score",
       subtitle = "Mtry = 1 | Min = 11") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, color = "darkolivegreen", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "skyblue"),
        axis.title = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(face = "bold", color = "cyan4"),
        legend.position = "none")

ggarrange(rf_CM_FINAL_Acc_1, rf_CM_FINAL_Acc70_1,
          rf_CM_FINAL_Acc_2, rf_CM_FINAL_Acc65_2,
          rf_CM_FINAL_F_1, rf_CM_FINAL_F_2,
          nrow = 3, ncol = 2)

# ROC Curve
# - Logistic Regression
Log_ROC_Acc_1_FINAL <- Log_FINAL_Results %>%
  roc_curve(Drafted, LOG_Acc_1_Prob) %>% 
  mutate(Model = "LOG_Acc_p003_m989")
Log_ROC_Acc_2_FINAL <- Log_FINAL_Results %>%
  roc_curve(Drafted, LOG_Acc_2_Prob) %>% 
  mutate(Model = "LOG_Acc_p001_m1")
Log_ROC_F_1_FINAL <- Log_FINAL_Results %>%
  roc_curve(Drafted, LOG_F_1_Prob) %>% 
  mutate(Model = "LOG_F_p014_m559")
Log_ROC_F_2_FINAL <- Log_FINAL_Results %>%
  roc_curve(Drafted, LOG_F_2_Prob) %>% 
  mutate(Model = "LOG_F_p011_m1")
# - Random Forrest
rf_ROC_Acc_1_FINAL <- rf_FINAL_Results %>%
  roc_curve(Drafted, RF_Acc_1_Prob) %>% 
  mutate(Model = "RF_Acc_mtry2_min36")
rf_ROC_Acc_2_FINAL <- rf_FINAL_Results %>%
  roc_curve(Drafted, RF_Acc_2_Prob) %>% 
  mutate(Model = "RF_Acc_mtry1_min1")
rf_ROC_F_1_FINAL <- rf_FINAL_Results %>%
  roc_curve(Drafted, RF_F_1_Prob) %>% 
  mutate(Model = "RF_F_mtry1_min1")
rf_ROC_F_2_FINAL <- rf_FINAL_Results %>%
  roc_curve(Drafted, RF_F_2_Prob) %>% 
  mutate(Model = "RF_F_mtry1_min11")
# - Visual
Log_ROC_Acc_1_FINAL %>% 
  bind_rows(Log_ROC_Acc_2_FINAL, Log_ROC_F_1_FINAL, Log_ROC_F_2_FINAL,
            rf_ROC_Acc_1_FINAL, rf_ROC_Acc_2_FINAL, rf_ROC_F_1_FINAL, rf_ROC_F_2_FINAL) %>% 
  mutate(specificity = 1 - specificity) %>% 
  ggplot(aes(specificity, sensitivity, color = Model)) +
  geom_line() + geom_abline(slope = 1, linetype = 2, alpha = 0.2) +
  labs(
    title = "ROC Curve", subtitle = "FINAL",
    x = "1 - specificity"
  ) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.5, color = "darkgreen"),
        axis.title.y = element_text(face = "bold", color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "tomato"), 
        legend.position = "bottom")


# METRICS
# - Logistic Regression Acc 1
log_Acc_1_FINAL_Metrics <- 
  tibble(Model = "Logistic_Regression_Acc_1",
         AUC = roc_auc(Log_FINAL_Results, Drafted, LOG_Acc_1_Prob) %>% pull(.estimate) %>% round(3),
         Accuracy = accuracy(Log_FINAL_Results, truth = Drafted, estimate = LOG_Acc_1_Pred) %>% pull(.estimate) %>% round(3),
         Sensitivity = sens(Log_FINAL_Results, truth = Drafted, estimate = LOG_Acc_1_Pred) %>% pull(.estimate) %>% round(3),
         Specificity = spec(Log_FINAL_Results, truth = Drafted, estimate = LOG_Acc_1_Pred) %>% pull(.estimate) %>% round(3),
         Precision = precision(Log_FINAL_Results, truth = Drafted, estimate = LOG_Acc_1_Pred) %>% pull(.estimate) %>% round(3),
         Recall = recall(Log_FINAL_Results, truth = Drafted, estimate = LOG_Acc_1_Pred) %>% pull(.estimate) %>% round(3),
         F1 = f_meas(Log_FINAL_Results, truth = Drafted, estimate = LOG_Acc_1_Pred) %>% pull(.estimate) %>% round(3))
# - Logistic Regression Acc 1 (70%)
log_Acc70_1_FINAL_Metrics <- 
  tibble(Model = "Logistic_Regression_Acc70_1",
         AUC = roc_auc(Log_FINAL_Results, Drafted, LOG_Acc_1_Prob) %>% pull(.estimate) %>% round(3),
         Accuracy = accuracy(Log_FINAL_Results, truth = Drafted, estimate = LOG_Acc_1_Pred70) %>% pull(.estimate) %>% round(3),
         Sensitivity = sens(Log_FINAL_Results, truth = Drafted, estimate = LOG_Acc_1_Pred70) %>% pull(.estimate) %>% round(3),
         Specificity = spec(Log_FINAL_Results, truth = Drafted, estimate = LOG_Acc_1_Pred70) %>% pull(.estimate) %>% round(3),
         Precision = precision(Log_FINAL_Results, truth = Drafted, estimate = LOG_Acc_1_Pred70) %>% pull(.estimate) %>% round(3),
         Recall = recall(Log_FINAL_Results, truth = Drafted, estimate = LOG_Acc_1_Pred70) %>% pull(.estimate) %>% round(3),
         F1 = f_meas(Log_FINAL_Results, truth = Drafted, estimate = LOG_Acc_1_Pred70) %>% pull(.estimate) %>% round(3))
# - Logistic Regression Acc 2
log_Acc_2_FINAL_Metrics <- 
  tibble(Model = "Logistic_Regression_Acc_2",
         AUC = roc_auc(Log_FINAL_Results, Drafted, LOG_Acc_2_Prob) %>% pull(.estimate) %>% round(3),
         Accuracy = accuracy(Log_FINAL_Results, truth = Drafted, estimate = LOG_Acc_2_Pred) %>% pull(.estimate) %>% round(3),
         Sensitivity = sens(Log_FINAL_Results, truth = Drafted, estimate = LOG_Acc_2_Pred) %>% pull(.estimate) %>% round(3),
         Specificity = spec(Log_FINAL_Results, truth = Drafted, estimate = LOG_Acc_2_Pred) %>% pull(.estimate) %>% round(3),
         Precision = precision(Log_FINAL_Results, truth = Drafted, estimate = LOG_Acc_2_Pred) %>% pull(.estimate) %>% round(3),
         Recall = recall(Log_FINAL_Results, truth = Drafted, estimate = LOG_Acc_2_Pred) %>% pull(.estimate) %>% round(3),
         F1 = f_meas(Log_FINAL_Results, truth = Drafted, estimate = LOG_Acc_2_Pred) %>% pull(.estimate) %>% round(3))
# - Logistic Regression Acc 2 (60%)
log_Acc60_2_FINAL_Metrics <- 
  tibble(Model = "Logistic_Regression_Acc60_2",
         AUC = roc_auc(Log_FINAL_Results, Drafted, LOG_Acc_2_Prob) %>% pull(.estimate) %>% round(3),
         Accuracy = accuracy(Log_FINAL_Results, truth = Drafted, estimate = LOG_Acc_2_Pred60) %>% pull(.estimate) %>% round(3),
         Sensitivity = sens(Log_FINAL_Results, truth = Drafted, estimate = LOG_Acc_2_Pred60) %>% pull(.estimate) %>% round(3),
         Specificity = spec(Log_FINAL_Results, truth = Drafted, estimate = LOG_Acc_2_Pred60) %>% pull(.estimate) %>% round(3),
         Precision = precision(Log_FINAL_Results, truth = Drafted, estimate = LOG_Acc_2_Pred60) %>% pull(.estimate) %>% round(3),
         Recall = recall(Log_FINAL_Results, truth = Drafted, estimate = LOG_Acc_2_Pred60) %>% pull(.estimate) %>% round(3),
         F1 = f_meas(Log_FINAL_Results, truth = Drafted, estimate = LOG_Acc_2_Pred60) %>% pull(.estimate) %>% round(3))
# - Logistic Regression F 1
log_F_1_FINAL_Metrics <- 
  tibble(Model = "Logistic_Regression_F_1",
         AUC = roc_auc(Log_FINAL_Results, Drafted, LOG_F_1_Prob) %>% pull(.estimate) %>% round(3),
         Accuracy = accuracy(Log_FINAL_Results, truth = Drafted, estimate = LOG_F_1_Pred) %>% pull(.estimate) %>% round(3),
         Sensitivity = sens(Log_FINAL_Results, truth = Drafted, estimate = LOG_F_1_Pred) %>% pull(.estimate) %>% round(3),
         Specificity = spec(Log_FINAL_Results, truth = Drafted, estimate = LOG_F_1_Pred) %>% pull(.estimate) %>% round(3),
         Precision = precision(Log_FINAL_Results, truth = Drafted, estimate = LOG_F_1_Pred) %>% pull(.estimate) %>% round(3),
         Recall = recall(Log_FINAL_Results, truth = Drafted, estimate = LOG_F_1_Pred) %>% pull(.estimate) %>% round(3),
         F1 = f_meas(Log_FINAL_Results, truth = Drafted, estimate = LOG_F_1_Pred) %>% pull(.estimate) %>% round(3))
# - Logistic Regression F 2
log_F_2_FINAL_Metrics <- 
  tibble(Model = "Logistic_Regression_F_2",
         AUC = roc_auc(Log_FINAL_Results, Drafted, LOG_F_2_Prob) %>% pull(.estimate) %>% round(3),
         Accuracy = accuracy(Log_FINAL_Results, truth = Drafted, estimate = LOG_F_2_Pred) %>% pull(.estimate) %>% round(3),
         Sensitivity = sens(Log_FINAL_Results, truth = Drafted, estimate = LOG_F_2_Pred) %>% pull(.estimate) %>% round(3),
         Specificity = spec(Log_FINAL_Results, truth = Drafted, estimate = LOG_F_2_Pred) %>% pull(.estimate) %>% round(3),
         Precision = precision(Log_FINAL_Results, truth = Drafted, estimate = LOG_F_2_Pred) %>% pull(.estimate) %>% round(3),
         Recall = recall(Log_FINAL_Results, truth = Drafted, estimate = LOG_F_2_Pred) %>% pull(.estimate) %>% round(3),
         F1 = f_meas(Log_FINAL_Results, truth = Drafted, estimate = LOG_F_2_Pred) %>% pull(.estimate) %>% round(3))

log_Metrics_FINAL <- log_Acc_1_FINAL_Metrics %>% 
  bind_rows(log_Acc70_1_FINAL_Metrics, log_Acc_2_FINAL_Metrics, log_Acc60_2_FINAL_Metrics, log_F_1_FINAL_Metrics, log_F_2_FINAL_Metrics)


# - Random Forrest Acc 1
rf_Acc_1_FINAL_Metrics <- 
  tibble(Model = "Random_Forrest_Acc_1",
         AUC = roc_auc(rf_FINAL_Results, Drafted, RF_Acc_1_Prob) %>% pull(.estimate) %>% round(3),
         Accuracy = accuracy(rf_FINAL_Results, truth = Drafted, estimate = RF_Acc_1_Pred) %>% pull(.estimate) %>% round(3),
         Sensitivity = sens(rf_FINAL_Results, truth = Drafted, estimate = RF_Acc_1_Pred) %>% pull(.estimate) %>% round(3),
         Specificity = spec(rf_FINAL_Results, truth = Drafted, estimate = RF_Acc_1_Pred) %>% pull(.estimate) %>% round(3),
         Precision = precision(rf_FINAL_Results, truth = Drafted, estimate = RF_Acc_1_Pred) %>% pull(.estimate) %>% round(3),
         Recall = recall(rf_FINAL_Results, truth = Drafted, estimate = RF_Acc_1_Pred) %>% pull(.estimate) %>% round(3),
         F1 = f_meas(rf_FINAL_Results, truth = Drafted, estimate = RF_Acc_1_Pred) %>% pull(.estimate) %>% round(3))
# - Random Forrest Acc 1 (70%)
rf_Acc70_1_FINAL_Metrics <- 
  tibble(Model = "Random_Forrest_Acc70_1",
         AUC = roc_auc(rf_FINAL_Results, Drafted, RF_Acc_1_Prob) %>% pull(.estimate) %>% round(3),
         Accuracy = accuracy(rf_FINAL_Results, truth = Drafted, estimate = RF_Acc_1_Pred70) %>% pull(.estimate) %>% round(3),
         Sensitivity = sens(rf_FINAL_Results, truth = Drafted, estimate = RF_Acc_1_Pred70) %>% pull(.estimate) %>% round(3),
         Specificity = spec(rf_FINAL_Results, truth = Drafted, estimate = RF_Acc_1_Pred70) %>% pull(.estimate) %>% round(3),
         Precision = precision(rf_FINAL_Results, truth = Drafted, estimate = RF_Acc_1_Pred70) %>% pull(.estimate) %>% round(3),
         Recall = recall(rf_FINAL_Results, truth = Drafted, estimate = RF_Acc_1_Pred70) %>% pull(.estimate) %>% round(3),
         F1 = f_meas(rf_FINAL_Results, truth = Drafted, estimate = RF_Acc_1_Pred70) %>% pull(.estimate) %>% round(3))
# - Random Forrest Acc 2
rf_Acc_2_FINAL_Metrics <- 
  tibble(Model = "Random_Forrest_Acc_2",
         AUC = roc_auc(rf_FINAL_Results, Drafted, RF_Acc_2_Prob) %>% pull(.estimate) %>% round(3),
         Accuracy = accuracy(rf_FINAL_Results, truth = Drafted, estimate = RF_Acc_2_Pred) %>% pull(.estimate) %>% round(3),
         Sensitivity = sens(rf_FINAL_Results, truth = Drafted, estimate = RF_Acc_2_Pred) %>% pull(.estimate) %>% round(3),
         Specificity = spec(rf_FINAL_Results, truth = Drafted, estimate = RF_Acc_2_Pred) %>% pull(.estimate) %>% round(3),
         Precision = precision(rf_FINAL_Results, truth = Drafted, estimate = RF_Acc_2_Pred) %>% pull(.estimate) %>% round(3),
         Recall = recall(rf_FINAL_Results, truth = Drafted, estimate = RF_Acc_2_Pred) %>% pull(.estimate) %>% round(3),
         F1 = f_meas(rf_FINAL_Results, truth = Drafted, estimate = RF_Acc_2_Pred) %>% pull(.estimate) %>% round(3))
# - Random Forrest Acc 2 (65%)
rf_Acc65_2_FINAL_Metrics <- 
  tibble(Model = "Random_Forrest_Acc65_2",
         AUC = roc_auc(rf_FINAL_Results, Drafted, RF_Acc_2_Prob) %>% pull(.estimate) %>% round(3),
         Accuracy = accuracy(rf_FINAL_Results, truth = Drafted, estimate = RF_Acc_2_Pred65) %>% pull(.estimate) %>% round(3),
         Sensitivity = sens(rf_FINAL_Results, truth = Drafted, estimate = RF_Acc_2_Pred65) %>% pull(.estimate) %>% round(3),
         Specificity = spec(rf_FINAL_Results, truth = Drafted, estimate = RF_Acc_2_Pred65) %>% pull(.estimate) %>% round(3),
         Precision = precision(rf_FINAL_Results, truth = Drafted, estimate = RF_Acc_2_Pred65) %>% pull(.estimate) %>% round(3),
         Recall = recall(rf_FINAL_Results, truth = Drafted, estimate = RF_Acc_2_Pred65) %>% pull(.estimate) %>% round(3),
         F1 = f_meas(rf_FINAL_Results, truth = Drafted, estimate = RF_Acc_2_Pred65) %>% pull(.estimate) %>% round(3))
# - Random Forrest F 1
rf_F_1_FINAL_Metrics <- 
  tibble(Model = "Random_Forrest_F_1",
         AUC = roc_auc(rf_FINAL_Results, Drafted, RF_F_1_Prob) %>% pull(.estimate) %>% round(3),
         Accuracy = accuracy(rf_FINAL_Results, truth = Drafted, estimate = RF_F_1_Pred) %>% pull(.estimate) %>% round(3),
         Sensitivity = sens(rf_FINAL_Results, truth = Drafted, estimate = RF_F_1_Pred) %>% pull(.estimate) %>% round(3),
         Specificity = spec(rf_FINAL_Results, truth = Drafted, estimate = RF_F_1_Pred) %>% pull(.estimate) %>% round(3),
         Precision = precision(rf_FINAL_Results, truth = Drafted, estimate = RF_F_1_Pred) %>% pull(.estimate) %>% round(3),
         Recall = recall(rf_FINAL_Results, truth = Drafted, estimate = RF_F_1_Pred) %>% pull(.estimate) %>% round(3),
         F1 = f_meas(rf_FINAL_Results, truth = Drafted, estimate = RF_F_1_Pred) %>% pull(.estimate) %>% round(3))
# - Random Forrest F 2
rf_F_2_FINAL_Metrics <- 
  tibble(Model = "Random_Forrest_F_2",
         AUC = roc_auc(rf_FINAL_Results, Drafted, RF_F_2_Prob) %>% pull(.estimate) %>% round(3),
         Accuracy = accuracy(rf_FINAL_Results, truth = Drafted, estimate = RF_F_2_Pred) %>% pull(.estimate) %>% round(3),
         Sensitivity = sens(rf_FINAL_Results, truth = Drafted, estimate = RF_F_2_Pred) %>% pull(.estimate) %>% round(3),
         Specificity = spec(rf_FINAL_Results, truth = Drafted, estimate = RF_F_2_Pred) %>% pull(.estimate) %>% round(3),
         Precision = precision(rf_FINAL_Results, truth = Drafted, estimate = RF_F_2_Pred) %>% pull(.estimate) %>% round(3),
         Recall = recall(rf_FINAL_Results, truth = Drafted, estimate = RF_F_2_Pred) %>% pull(.estimate) %>% round(3),
         F1 = f_meas(rf_FINAL_Results, truth = Drafted, estimate = RF_F_2_Pred) %>% pull(.estimate) %>% round(3))

rf_Metrics_FINAL <- rf_Acc_1_FINAL_Metrics %>% 
  bind_rows(rf_Acc70_1_FINAL_Metrics, rf_Acc_2_FINAL_Metrics, rf_Acc65_2_FINAL_Metrics, rf_F_1_FINAL_Metrics, rf_F_2_FINAL_Metrics)

# FINAL Metrics
metrics_FINAL <- log_Metrics_FINAL %>% 
  bind_rows(rf_Metrics_FINAL)

# Feature Selection ----
rf_fit_F %>% 
  pluck(".workflow",1)
