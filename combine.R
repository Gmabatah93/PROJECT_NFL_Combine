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
library(DALEXtra)
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
  select(player, side, position, school, conference, everything()) %>% 
  select(-year, -pick, -round) %>% 
  mutate(drafted = ifelse(drafted == "Yes", 1,0) %>% factor(levels = c(1,0), labels = c("Yes","No")))

# Target Round
nfl_round <- nfl %>% 
  select(player, side, position, school, conference, everything()) %>% 
  select(-year, -pick, -drafted) %>% 
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
  labs(fill = "Drafted ?",
       caption = "Year: 2000 - 2018") +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 15),
    plot.caption = element_text(color = "gray70", face = "italic")
  ) + 
  scale_fill_manual(values = c("forestgreen","grey80"))

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
  scale_fill_manual(values = c("forestgreen", "grey80"))
gg_draft_Side_prop <- nfl_draft %>%
  group_by(drafted) %>% 
  count(side) %>% 
  mutate(side = fct_reorder(side, n)) %>% 
  ggplot(aes(side, n, fill = relevel(drafted, ref = "No"))) +
  geom_col(position = "fill") + geom_hline(yintercept = 0.66, color = "red") +
  labs(caption = "Year: 2000 - 2018") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_blank(),
    axis.text.x = element_text(face = "italic", size = 15, hjust = 1, angle = 45),
    legend.position = "none",
    plot.caption = element_text(color = "gray70", face = "italic")
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("grey80", "forestgreen"))
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
  labs(title = "Position",
       caption = "Year: 2000 - 2018") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "none",
    plot.caption = element_text(color = "gray70", face = "italic")
  ) +
  scale_fill_manual(values = c("forestgreen", "grey80"))
gg_draft_position_Offense_prop <- nfl_draft %>%
  filter(side == "Offense") %>% 
  group_by(drafted, side) %>% 
  count(position) %>% 
  mutate(position = fct_reorder(position, n)) %>% 
  ggplot(aes(position, n, fill = relevel(drafted, ref = "No"))) +
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
  ggplot(aes(position, n, fill = relevel(drafted, ref = "No"))) +
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
  scale_fill_manual(values = c("forestgreen", "grey80"))
gg_draft_conference_prop <- nfl_draft %>%
  group_by(drafted) %>% 
  count(conference) %>% 
  mutate(conference = fct_reorder(conference, n)) %>% 
  ggplot(aes(conference, n, fill = relevel(drafted, ref = "No"))) +
  geom_col(position = "fill") +
  geom_hline(yintercept = 0.66, color = "red") +
  labs(caption = "Year: 2000 - 2018") +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    legend.position = "none",
    plot.caption = element_text(color = "gray70", face = "italic")
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
  labs(caption = "Year: 2000 - 2018") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        plot.caption = element_text(color = "gray70", face = "italic"))

# Drafted
# - Weight ~ Forty: 89
gg_drafted_Weight_Forty <- nfl_draft %>% 
  ggplot(aes(weight, forty, color = drafted)) +
  geom_point(alpha = 0.1) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    axis.title = element_text(face = "bold", size = 14),
    legend.position = "none"
  ) +
  scale_color_manual(values = c("forestgreen","grey80"))
# - Three Cone ~ Shuttle: 85
gg_drafted_3cone_Shuttle <- nfl_draft %>% 
  ggplot(aes(three_cone, shuttle, color = drafted)) +
  geom_point(alpha = 0.1) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    axis.title = element_text(face = "bold", size = 14),
    legend.position = "none"
  ) +
  scale_color_manual(values = c("forestgreen","grey80"))
# - Forty ~ Broad Jump: -84
gg_drafted_Forty_BJump <- nfl_draft %>% 
  ggplot(aes(forty, broad_jump, color = drafted)) +
  geom_point(alpha = 0.1) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    axis.title = element_text(face = "bold", size = 14),
    legend.position = "none"
  ) +
  scale_color_manual(values = c("forestgreen","grey80"))
# - Forty ~ Three Cone: 83
gg_drafted_Forty_3cone <- nfl_draft %>% 
  ggplot(aes(forty, three_cone, color = drafted)) +
  geom_point(alpha = 0.1) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    axis.title = element_text(face = "bold", size = 14),
    legend.position = "none"
  ) +
  scale_color_manual(values = c("forestgreen","grey80"))
# - Vertical ~ Broad Jump: 82
gg_drafted_Vertical_BJump <- nfl_draft %>% 
  ggplot(aes(vertical, broad_jump, color = drafted)) +
  geom_point(alpha = 0.1) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    axis.title = element_text(face = "bold", size = 14),
    legend.position = "none"
  ) +
  scale_color_manual(values = c("forestgreen","grey80"))
# - Weight ~ Three Cone: 81
gg_drafted_Weight_3cone <- nfl_draft %>% 
  ggplot(aes(weight, three_cone, color = drafted)) +
  geom_point(alpha = 0.1) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    axis.title = element_text(face = "bold", size = 14),
    legend.position = "none"
  ) +
  scale_color_manual(values = c("forestgreen","grey80"))

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
                  title = "PCA Biplot: Combine Stats",
                  legend.title = list(alpha = "Quality of Representation")) +
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size = 18),
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
                  palette = c("forestgreen","gray80"),
                  legend.title = "Drafted ?") +
  guides(alpha = FALSE) +
  theme(plot.title = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "top",
        legend.title = element_text(hjust = 0.2,face = "bold", size = 15))

#
# "DRAFT": Exploratory Data Analysis - Combine ----

# Combine Data: Weight
# - mean
nfl_mean_weight <- nfl_draft$weight %>% mean
# - plot
gg_Combine_Weight_draft <- nfl_draft %>% 
  ggplot(aes(weight)) +
  geom_histogram(bins = 50, alpha = 0.3) + 
  geom_vline(xintercept = nfl_mean_weight, color = "red") +
  labs(title = "Combine: Weight") +
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size = 20),
        axis.title = element_blank())
gg_Combine_Weight_Position_draft <- nfl_draft %>% 
  ggplot(aes(weight, fill = side)) +
  geom_density(alpha = 0.1) +
  scale_fill_manual(values = c("blue","red")) +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "bottom")
# - visual
ggarrange(gg_Combine_Weight_draft, gg_Combine_Weight_Position_draft, ncol = 1)

# - By Side
gg_Combine_Weight_Offense_draft <- nfl_draft %>% 
  filter(side == "Offense") %>% 
  ggplot(aes(position, weight, fill = drafted)) +
  geom_boxplot() + geom_hline(yintercept = nfl_mean_weight, color = "red") +
  geom_jitter(alpha = 0.09) +
  labs(title = "Offense") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        axis.title.y = element_text(color = "red"),
        axis.title.x = element_blank(),
        legend.position = "none") +
  scale_fill_manual(values = c("forestgreen", "gray80"))
gg_Combine_Weight_Defense_draft <- nfl_draft %>% 
  filter(side == "Defense") %>% 
  ggplot(aes(position, weight, fill = drafted)) +
  geom_boxplot() + geom_hline(yintercept = nfl_mean_weight, color = "darkred") +
  geom_jitter(alpha = 0.09) +
  labs(title = "Defense") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        axis.title = element_blank(),
        axis.text.y = element_blank()) +
  scale_fill_manual(values = c("forestgreen", "gray80"))
# -- visual
ggarrange(gg_Combine_Weight_Offense_draft, gg_Combine_Weight_Defense_draft, nrow = 1)  

# - By Conference
nfl_draft %>%
  mutate(cond = case_when(
    conference %in% c("Division I-A (SEC)", "Division I-A (ACC)", "Division I-A (Big 10)", "Division I-A (Big 12)", "Division I-A (Pac-12)") ~ "Elite",
    conference %in% c("Division I-A (American)", "Division I-A (Sunbelt)", "Division I-A (Mountain West)", "Division I-A (MAC)","Division I-A (Conference USA)") ~ "Division I-A",
    conference %in% c("Division I-AA") ~ "Division I-AA",
    conference %in% c("Division II & III") ~ "Division II & III"
  )) %>% 
  ggplot(aes(conference, weight, fill = cond)) +
  geom_boxplot() + geom_hline(yintercept = nfl_mean_weight, color = "red") +
  labs(title = "Conference",
       caption = "Year: 2000 - 2018") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(colour = "red"),
        axis.title.x = element_blank(),
        plot.caption = element_text(color = "gray70", face = "italic")) +
  scale_fill_brewer(palette = "YlGnBu")


# Combine Data: Forty
# - mean
nfl_mean_forty <- nfl_draft$forty %>% mean
# - plot
gg_Combine_40_draft <- nfl_draft %>% 
  ggplot(aes(forty)) +
  geom_histogram(bins = 50, alpha = 0.3) + 
  geom_vline(xintercept = nfl_mean_forty, color = "red") +
  labs(title = "Combine: 40") +
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size = 20),
        axis.title = element_blank())
gg_Combine_40_Position_draft <- nfl_draft %>% 
  ggplot(aes(forty, fill = side)) +
  geom_density(alpha = 0.1) +
  scale_fill_manual(values = c("blue","red")) +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "bottom")
# - visual
ggarrange(gg_Combine_40_draft, gg_Combine_40_Position_draft, ncol = 1)

# - By Side
gg_Combine_40_Offense_draft <- nfl_draft %>% 
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
  scale_fill_manual(values = c("forestgreen", "gray80")) 
gg_Combine_40_Defense_draft <- nfl_draft %>% 
  filter(side == "Defense") %>% 
  ggplot(aes(position, forty, fill = drafted)) +
  geom_boxplot() + geom_hline(yintercept = nfl_mean_forty, color = "darkred") +
  geom_jitter(alpha = 0.09) +
  labs(title = "Defense") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        axis.title = element_blank(),
        axis.text.y = element_blank()) +
  ylim(c(4,6)) +
  scale_fill_manual(values = c("forestgreen", "gray80"))
# -- visual
ggarrange(gg_Combine_40_Offense_draft, gg_Combine_40_Defense_draft, nrow = 1)  


# - By Conference
nfl_draft %>%
  mutate(cond = case_when(
    conference %in% c("Division I-A (SEC)", "Division I-A (ACC)", "Division I-A (Big 10)", "Division I-A (Big 12)", "Division I-A (Pac-12)") ~ "Elite",
    conference %in% c("Division I-A (American)", "Division I-A (Sunbelt)", "Division I-A (Mountain West)", "Division I-A (MAC)","Division I-A (Conference USA)") ~ "Division I-A",
    conference %in% c("Division I-AA") ~ "Division I-AA",
    conference %in% c("Division II & III") ~ "Division II & III"
  )) %>% 
  ggplot(aes(conference, forty, fill = cond)) +
  geom_boxplot() + geom_hline(yintercept = nfl_mean_forty, color = "red") +
  labs(title = "Conference",
       caption = "Year: 2000 - 2018") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(colour = "red"),
        axis.title.x = element_blank(),
        plot.caption = element_text(color = "gray70", face = "italic")) +
  scale_fill_brewer(palette = "YlGnBu")

# Combine Data: Bench
# - mean
nfl_mean_bench <- nfl_draft$bench %>% mean
# - plot
gg_Combine_Bench_draft <- nfl_draft %>% 
  ggplot(aes(bench)) +
  geom_histogram(bins = 50, alpha = 0.3) + 
  geom_vline(xintercept = nfl_mean_bench, color = "red") +
  labs(title = "Combine: Bench") +
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size = 20),
        axis.title = element_blank())
gg_Combine_Bench_Position_draft <- nfl_draft %>% 
  ggplot(aes(bench, fill = side)) +
  geom_density(alpha = 0.1) +
  scale_fill_manual(values = c("blue","red")) +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "bottom")
# - visual
ggarrange(gg_Combine_Bench_draft, gg_Combine_Bench_Position_draft, ncol = 1)

# - By Side
gg_Combine_Bench_Offense_draft <- nfl_draft %>% 
  filter(side == "Offense") %>% 
  ggplot(aes(position, bench, fill = drafted)) +
  geom_boxplot() + geom_hline(yintercept = nfl_mean_bench, color = "red") +
  geom_jitter(alpha = 0.09) +
  labs(title = "Offense") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        axis.title.y = element_text(color = "red"),
        axis.title.x = element_blank(),
        legend.position = "none") +
  scale_fill_manual(values = c("forestgreen","gray80"))
gg_Combine_Bench_Defense_draft <- nfl_draft %>% 
  filter(side == "Defense") %>% 
  ggplot(aes(position, bench, fill = drafted)) +
  geom_boxplot() + geom_hline(yintercept = nfl_mean_bench, color = "darkred") +
  geom_jitter(alpha = 0.09) +
  labs(title = "Defense") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        axis.title = element_blank(),
        axis.text.y = element_blank()) +
  scale_fill_manual(values = c("forestgreen","gray80"))
# -- visual
ggarrange(gg_Combine_Bench_Offense_draft, gg_Combine_Bench_Defense_draft, nrow = 1)  

# - By Conference
nfl_draft %>%
  mutate(cond = case_when(
    conference %in% c("Division I-A (SEC)", "Division I-A (ACC)", "Division I-A (Big 10)", "Division I-A (Big 12)", "Division I-A (Pac-12)") ~ "Elite",
    conference %in% c("Division I-A (American)", "Division I-A (Sunbelt)", "Division I-A (Mountain West)", "Division I-A (MAC)","Division I-A (Conference USA)") ~ "Division I-A",
    conference %in% c("Division I-AA") ~ "Division I-AA",
    conference %in% c("Division II & III") ~ "Division II & III"
  )) %>% 
  ggplot(aes(conference, bench, fill = cond)) +
  geom_boxplot() + geom_hline(yintercept = nfl_mean_bench, color = "red") +
  labs(title = "Conference",
       caption = "Year: 2000 - 2018") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(colour = "red"),
        axis.title.x = element_blank(),
        plot.caption = element_text(color = "gray70", face = "italic")) +
  scale_fill_brewer(palette = "YlGnBu")

# Combine Data: Broad Jump
# - mean
nfl_mean_jump <- nfl_draft$broad_jump %>% mean
# - plot
gg_Combine_Jump_draft <- nfl_draft %>% 
  ggplot(aes(broad_jump)) +
  geom_histogram(bins = 50, alpha = 0.3) + 
  geom_vline(xintercept = nfl_mean_jump, color = "red") +
  labs(title = "Combine: Broad Jump") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        axis.title = element_blank())
gg_Combine_Jump_Position_draft <- nfl_draft %>% 
  ggplot(aes(broad_jump, fill = side)) +
  geom_density(alpha = 0.1) +
  scale_fill_manual(values = c("blue","red")) +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "bottom")
# - visual
ggarrange(gg_Combine_Jump_draft, gg_Combine_Jump_Position_draft, ncol = 1)

# - By Side
gg_Combine_Jump_Offense_draft <- nfl_draft %>% 
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
  scale_fill_manual(values = c("forestgreen","gray80"))
gg_Combine_Jump_Defense_draft <- nfl_draft %>% 
  filter(side == "Defense") %>% 
  ggplot(aes(position, broad_jump, fill = drafted)) +
  geom_boxplot() + geom_hline(yintercept = nfl_mean_jump, color = "darkred") +
  geom_jitter(alpha = 0.09) +
  labs(title = "Defense") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        axis.title = element_blank(),
        axis.text.y = element_blank()) +
  ylim(c(80,140)) +
  scale_fill_manual(values = c("forestgreen", "gray80"))
# -- visual
ggarrange(gg_Combine_Jump_Offense_draft, gg_Combine_Jump_Defense_draft, nrow = 1)  

# - By Conference
nfl_draft %>%
  mutate(cond = case_when(
    conference %in% c("Division I-A (SEC)", "Division I-A (ACC)", "Division I-A (Big 10)", "Division I-A (Big 12)", "Division I-A (Pac-12)") ~ "Elite",
    conference %in% c("Division I-A (American)", "Division I-A (Sunbelt)", "Division I-A (Mountain West)", "Division I-A (MAC)","Division I-A (Conference USA)") ~ "Division I-A",
    conference %in% c("Division I-AA") ~ "Division I-AA",
    conference %in% c("Division II & III") ~ "Division II & III"
  )) %>% 
  ggplot(aes(conference, broad_jump, fill = cond)) +
  geom_boxplot() + geom_hline(yintercept = nfl_mean_jump, color = "red") +
  labs(title = "Conference",
       caption = "Year: 2000 - 2018") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(colour = "red"),
        axis.title.x = element_blank(),
        plot.caption = element_text(color = "gray70", face = "italic")) +
  scale_fill_brewer(palette = "YlGnBu")


# "DRAFT": Exploratory Data Analysis - Offense ----
nfl_draft %>% 
  filter(side =="Offense") %>% 
  count(drafted)
# - note: 874 drafted

# STATS: 

# Summary
nfl_draft %>% 
  filter(side == "Offense") %>% 
  summarise(Avg_Weight = mean(weight),
            Avg_Forty = mean(forty),
            Avg_BroadJump = mean(broad_jump),
            Avg_Bench = mean(bench))

# By Conference
nfl_offense_stat_Conference <- nfl_draft %>% 
  filter(side == "Offense") %>% 
  group_by(conference, drafted) %>% 
  summarise(Weight = mean(weight),
            Forty = mean(forty),
            BroadJump = mean(broad_jump),
            Bench = mean(bench)) %>% 
  gather(Weight, Forty, BroadJump, Bench,
         key = "Measure", value = "Stat")
# - plot
nfl_offense_stat_Conference %>% 
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

# By Position
nfl_offense_stat_Position <- nfl_draft %>% 
  filter(side == "Offense") %>% 
  group_by(position, drafted) %>% 
  summarise(Weight = mean(weight),
            Forty = mean(forty),
            BroadJump = mean(broad_jump),
            Bench = mean(bench)) %>% 
  gather(Weight, Forty, BroadJump, Bench,
         key = "Measure", value = "Stat")
# - plot
nfl_offense_stat_Position %>% 
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

# Statistical Test
nfl_draft %>% 
  filter(side == "Offense") %>% 
  mutate(drafted = ifelse(drafted == "Yes", 1, 0)) %>% 
  glm(drafted ~ weight + forty + broad_jump + bench, family = "binomial", data = .) %>% 
  tidy() %>% 
  mutate(estimate = plogis(estimate),
         p.value = round(p.value,3))

nfl_draft %>% 
  filter(side == "Offense") %>% 
  mutate(drafted = ifelse(drafted == "Yes", 1, 0)) %>% 
  glm(drafted ~ conference + weight + forty + broad_jump + bench, family = "binomial", data = .) %>% 
  tidy() %>% 
  mutate(estimate = plogis(estimate),
         p.value = round(p.value,3))

nfl_draft %>% 
  filter(side == "Offense") %>% 
  mutate(drafted = ifelse(drafted == "Yes", 1, 0)) %>%
  glm(drafted ~ position + weight + forty + broad_jump + bench, family = "binomial", data = .) %>% 
  tidy() %>% 
  mutate(estimate = plogis(estimate),
         p.value = round(p.value,3))

nfl_draft %>% 
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
  step_dummy(side, position, conference)
# - model data
nfl_log_recipe_normal %>% 
  prep() %>% juice() %>% 
  glimpse()


# PCA
set.seed(101)
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
  scale_fill_manual(values = c("antiquewhite","antiquewhite4")) +
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
  prep() %>% juice() %>% 
  glimpse()


nfl_log_recipe_pca %>% 
  prep() %>% 
  tidy(id = "pca") %>% 
  pivot_wider(names_from = component, id_cols = terms) %>% 
  select(terms:PC4)

# Simple
nfl_log_recipe_simple <- 
  recipe(drafted ~ player + school + team +
           position + conference + 
           height + weight + forty + broad_jump + bench,
         data = nfl_train) %>% 
  update_role(player, school, new_role = "id") %>% 
  update_role(team, new_role = "team") %>% 
  step_other(position, threshold = 0.01) %>% 
  step_normalize(all_numeric()) %>% 
  step_dummy(position, conference)
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
set.seed(101)
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
           position + conference + 
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
  set_engine("glmnet", seed = 101, importance = "permutation") %>% 
  set_mode("classification")
# Workflow
log_wflow <- 
  workflow() %>% 
  add_model(log_spec) %>% 
  add_recipe(nfl_log_recipe_normal) 

# Hyperparameters
log_spec %>%  parameters() %>% pull_dials_object("penalty")
log_spec %>%  parameters() %>% pull_dials_object("mixture")

# Custom Grid
log_grid_custom <- 
  crossing(
    penalty = seq(0.001, 0.1, 0.005),
    mixture = c(0, 0.5, 1)
  )
# - visual
log_grid_custom %>% 
  ggplot(aes(penalty, mixture)) +
  geom_point(shape = 1, size = 4, color = "grey60") +
  labs(title = "Custom Grid",
       subtitle = "Logistic Regression") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.4, color = "darkolivegreen"),
        axis.title = element_text(face = "bold", color = "cyan4"))



#
# Modeling: Spec - Random Forrest ----

# Spec
rf_spec <- 
  rand_forest(
    mtry = tune(),
    trees = 1000,
    min_n = tune()
  ) %>% 
  set_engine("ranger", seed = 101, importance = "permutation") %>% # or permutation
  set_mode("classification")

# Workflow
rf_wflow <- 
  workflow() %>% 
  add_model(rf_spec) %>% 
  add_recipe(nfl_rf_recipe_normal)

# Hyperparameters
rf_spec %>%  parameters() %>% pull_dials_object("min_n")
rf_spec %>%  parameters() %>% update(mtry = mtry(c(1,8))) %>%  pull_dials_object("mtry")

# Custom Grid
rf_grid_custom <- 
  crossing(
    min_n = seq(1, 10, 2),
    mtry = 1:8
  )
# - visual
rf_grid_custom %>% 
  ggplot(aes(min_n, mtry)) +
  geom_point(shape = 1, size = 4, color = "grey60") +
  labs(title = "Custom Grid",
       subtitle = "Random Forrest") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.4, color = "darkolivegreen"),
        axis.title = element_text(face = "bold", color = "cyan4"))



#
# Modeling: Fit - Logistic Regression ----
options(tidymodels.dark = TRUE) 

# Normal
set.seed(101)
log_tune_normal <-
  log_wflow %>% 
  tune_grid(
    resamples = nfl_10fold,
    grid = log_grid_custom,
    metrics = nfl_metrics,
    control = nfl_ctrl
  )
# PCA
set.seed(101)
log_tune_pca <- 
  log_wflow %>% 
  update_recipe(nfl_log_recipe_pca) %>%
  tune_grid(
    resamples = nfl_10fold,
    grid = log_grid_custom,
    metrics = nfl_metrics,
    control = nfl_ctrl
  )
# Simple
set.seed(101)
log_tune_simple <- 
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
gg_Log_tune_normal_Acc <- log_tune_normal %>% 
  collect_metrics() %>% 
  filter(.metric == "accuracy") %>%
  mutate(mixture = factor(mixture)) %>% 
  ggplot(aes(penalty, mean)) +
  geom_line(aes(color = mixture)) + geom_point() +
  labs(title = "Logistic Regression",
       subtitle = "Normal",
       y = "Accuracy") +
  ylim(c(0.65, 0.72)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.4, color = "darkolivegreen"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.title = element_text(face = "bold", color = "cyan4"),
        legend.position = "bottom")
# - pca
gg_Log_tune_pca_Acc <- log_tune_pca %>% 
  collect_metrics() %>% 
  filter(.metric == "accuracy") %>%
  mutate(mixture = factor(mixture)) %>% 
  ggplot(aes(penalty, mean)) +
  geom_line(aes(color = mixture)) + geom_point() +
  labs(title = "Logistic Regression",
       subtitle = "PCA",
       y = "Accuracy") +
  ylim(c(0.65, 0.72)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.4, color = "darkolivegreen"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.title = element_text(face = "bold", color = "cyan4"),
        legend.position = "bottom")
# - normal
gg_Log_tune_simple_Acc <- log_tune_simple %>% 
  collect_metrics() %>% 
  filter(.metric == "accuracy") %>%
  mutate(mixture = factor(mixture)) %>% 
  ggplot(aes(penalty, mean)) +
  geom_line(aes(color = mixture)) + geom_point() +
  labs(title = "Logistic Regression",
       subtitle = "Simple",
       y = "Accuracy") +
  ylim(c(0.65, 0.72)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.4, color = "darkolivegreen"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.title = element_text(face = "bold", color = "cyan4"),
        legend.position = "bottom")
# - VISUAL
ggarrange(gg_Log_tune_normal_Acc, gg_Log_tune_pca_Acc, gg_Log_tune_simple_Acc, nrow = 1)

# PLOT: F Score
# - normal
gg_Log_tune_normal_F <- log_tune_normal %>% 
  collect_metrics() %>% 
  filter(.metric == "f_meas") %>%
  mutate(mixture = factor(mixture)) %>% 
  ggplot(aes(penalty, mean)) +
  geom_line(aes(color = mixture)) + geom_point() +
  labs(title = "Logistic Regression",
       subtitle = "Normal",
       y = "F Score") +
  ylim(c(0.785, 0.81)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.4, color = "darkolivegreen"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.title = element_text(face = "bold", color = "cyan4"),
        legend.position = "bottom")
# - pca
gg_Log_tune_pca_F <- log_tune_pca %>% 
  collect_metrics() %>% 
  filter(.metric == "f_meas") %>%
  mutate(mixture = factor(mixture)) %>% 
  ggplot(aes(penalty, mean)) +
  geom_line(aes(color = mixture)) + geom_point() +
  labs(title = "Logistic Regression",
       subtitle = "PCA",
       y = "F Score") +
  ylim(c(0.785, 0.81)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.4, color = "darkolivegreen"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.title = element_text(face = "bold", color = "cyan4"),
        legend.position = "bottom")
# - simple
gg_Log_tune_simple_F <- log_tune_simple %>% 
  collect_metrics() %>% 
  filter(.metric == "f_meas") %>%
  mutate(mixture = factor(mixture)) %>% 
  ggplot(aes(penalty, mean)) +
  geom_line(aes(color = mixture)) + geom_point() +
  labs(title = "Logistic Regression",
       subtitle = "Simple",
       y = "F Score") +
  ylim(c(0.785, 0.81)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.4, color = "darkolivegreen"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.title = element_text(face = "bold", color = "cyan4"),
        legend.position = "bottom")
# - VISUAL
ggarrange(gg_Log_tune_normal_F, gg_Log_tune_pca_F, gg_Log_tune_simple_F, nrow = 1)





# METRICS: Best
# - best: Accuracy
log_tune_normal %>% show_best(metric = "accuracy", n = 1) # 71.7 | 001 | 0.5
log_tune_pca %>% show_best(metric = "accuracy", n = 1)    # 69.9 | 021 | 0
log_tune_simple %>% show_best(metric = "accuracy", n = 1) # 70.7 | 001 | 0
# - best: F Score
log_tune_normal %>% show_best(metric = "f_meas", n = 1)   # 80.3 | 016 | 0.5
log_tune_pca %>% show_best(metric = "f_meas", n = 1)      # 80.1 | 051 | 0.5
log_tune_simple %>% show_best(metric = "f_meas", n = 1)   # 80.1 | 011 | 0



#
# Modeling: FINAL Fit - Logistic Regression ----

# Accuracy (Normal) 71.7%: penaly = 0.001, mix = 0.5
# - hyperparameter
log_best_Acc <- log_tune_normal %>% select_best(metric = "accuracy")
# - fit
log_fit_Acc <- 
  log_wflow %>%
  finalize_workflow(log_best_Acc) %>% 
  fit(nfl_train)

# F Score (Simple) 80.1%: penaly = 0.011, mix = 0
# - hyperparameter
log_best_F <- log_tune_simple %>% select_best(metric = "f_meas")
# - fit
log_fit_F <- 
  log_wflow %>% 
  update_recipe(nfl_log_recipe_simple) %>% 
  finalize_workflow(log_best_F) %>% 
  fit(nfl_train)

#
# Modeling: Fit - Random Forest ----

# Start Parallel Processing
cl_6 <- makeCluster(6)
registerDoParallel(cl_6)

# Normal
set.seed(101)
rf_tune_normal <-
  rf_wflow %>% 
  tune_grid(
    resamples = nfl_10fold,
    grid = rf_grid_custom,
    metrics = nfl_metrics,
    control = nfl_ctrl
  )
# PCA
set.seed(101)
rf_tune_pca <- 
  rf_wflow %>% 
  update_recipe(nfl_rf_recipe_pca) %>%
  tune_grid(
    resamples = nfl_10fold,
    grid = rf_grid_custom,
    metrics = nfl_metrics,
    control = nfl_ctrl
  )
# Simple
set.seed(101)
rf_tune_simple <- 
  rf_wflow %>% 
  update_recipe(nfl_rf_recipe_simple) %>%
  tune_grid(
    resamples = nfl_10fold,
    grid = rf_grid_custom,
    metrics = nfl_metrics,
    control = nfl_ctrl
  )

# Stop Parallell Processing
stopCluster(cl_6)

# - plot: Accuracy
# - normal
gg_RF_tune_normal_Acc <- rf_tune_normal %>% 
  collect_metrics() %>% 
  filter(.metric == "accuracy") %>%
  mutate(mtry = factor(mtry)) %>% 
  ggplot(aes(min_n, mean)) +
  geom_line(aes(color = mtry)) + geom_point() +
  labs(title = "Random Forrest",
       subtitle = "None",
       y = "Accuracy") +
  ylim(c(0.63, 0.71)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.4, color = "darkolivegreen"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.title = element_text(face = "bold", color = "cyan4"),
        legend.text = element_text(size = 7),
        legend.position = "bottom")
# - PCA
gg_RF_tune_pca_Acc <- rf_tune_pca %>% 
  collect_metrics() %>% 
  filter(.metric == "accuracy") %>%
  mutate(mtry = factor(mtry)) %>% 
  ggplot(aes(min_n, mean)) +
  geom_line(aes(color = mtry)) + geom_point() +
  labs(title = "Random Forrest",
       subtitle = "PCA",
       y = "Accuracy") +
  ylim(c(0.63, 0.71)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.4, color = "darkolivegreen"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.title = element_text(face = "bold", color = "cyan4"),
        legend.text = element_text(size = 7),
        legend.position = "bottom")
# - Simple
gg_RF_tune_simple_Acc <- rf_tune_simple %>% 
  collect_metrics() %>% 
  filter(.metric == "accuracy") %>%
  mutate(mtry = factor(mtry)) %>% 
  ggplot(aes(min_n, mean)) +
  geom_line(aes(color = mtry)) + geom_point() +
  labs(title = "Random Forrest",
       subtitle = "Simple",
       y = "Accuracy") +
  ylim(c(0.63, 0.71)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.4, color = "darkolivegreen"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.title = element_text(face = "bold", color = "cyan4"),
        legend.text = element_text(size = 7),
        legend.position = "bottom")
# - Visuals
ggarrange(gg_RF_tune_normal_Acc, gg_RF_tune_pca_Acc, gg_RF_tune_simple_Acc, nrow = 1)

# - PLOT: F Score
# - normal
gg_RF_tune_normal_F <- rf_tune_normal %>% 
  collect_metrics() %>% 
  filter(.metric == "f_meas") %>%
  mutate(mtry = factor(mtry)) %>% 
  ggplot(aes(min_n, mean)) +
  geom_line(aes(color = mtry)) + geom_point() +
  labs(title = "Random Forrest",
       subtitle = "None",
       y = "F Score") +
  ylim(c(0.74, 0.80)) + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.4, color = "darkolivegreen"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.title = element_text(face = "bold", color = "cyan4"),
        legend.text = element_text(size = 7),
        legend.position = "bottom")
# - PCA
gg_RF_tune_pca_F <- rf_tune_pca %>% 
  collect_metrics() %>% 
  filter(.metric == "f_meas") %>%
  mutate(mtry = factor(mtry)) %>% 
  ggplot(aes(min_n, mean)) +
  geom_line(aes(color = mtry)) + geom_point() +
  labs(title = "Random Forrest",
       subtitle = "PCA",
       y = "F Score") +
  ylim(c(0.74, 0.80)) + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.4, color = "darkolivegreen"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.title = element_text(face = "bold", color = "cyan4"),
        legend.text = element_text(size = 7),
        legend.position = "bottom")
# - Simple
gg_RF_tune_simple_F <- rf_tune_simple %>% 
  collect_metrics() %>% 
  filter(.metric == "f_meas") %>%
  mutate(mtry = factor(mtry)) %>% 
  ggplot(aes(min_n, mean)) +
  geom_line(aes(color = mtry)) + geom_point() +
  labs(title = "Random Forrest",
       subtitle = "Simple",
       y = "F Score") +
  ylim(c(0.74, 0.80)) + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.4, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.4, color = "darkolivegreen"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.title = element_text(face = "bold", color = "cyan4"),
        legend.text = element_text(size = 7),
        legend.position = "bottom")
# - Visual
ggarrange(gg_RF_tune_normal_F, gg_RF_tune_pca_F, gg_RF_tune_simple_F, nrow = 1)


# METRICS
# - best: Accuracy
rf_tune_normal %>% show_best("accuracy", n = 1) # 69.6 | 1 | 9
rf_tune_pca %>% show_best("accuracy", n = 1)    # 68.2 | 1 | 5
rf_tune_simple %>% show_best("accuracy", n = 1) # 70.5 | 6 | 9
# - best: F Score
rf_tune_normal %>% show_best("f_meas", n = 1)   # 79.9 | 1 | 9
rf_tune_pca %>% show_best("f_meas", n = 1)      # 79.1 | 1 | 5
rf_tune_simple %>% show_best("f_meas", n = 1)   # 79.6 | 1 | 3


#
# Modeling: FINAL Fit - Random Forrest ----

# Accuracy (Simple) 70.5%: Mtry = 6, Min_n = 9
# - hyperparamter
rf_best_Acc <- rf_tune_simple %>% select_best(metric = "accuracy")
# - fit
rf_fit_Acc <- 
  rf_wflow %>% 
  update_recipe(nfl_rf_recipe_simple) %>% 
  finalize_workflow(rf_best_Acc) %>% 
  fit(nfl_train)

# F Score (NONE) 79.9%: Mtry = 1, Min_n = 9
# - hyperparameter
rf_best_F <- rf_tune_normal %>% select_best(metric = "f_meas")
# - fit
rf_fit_F <- 
  rf_wflow %>% 
  finalize_workflow(rf_best_F) %>% 
  fit(nfl_train)

#
# Modeling: Validation Diagnostics - Logistic Regression ----

# PREDICTIONS
log_Results <- 
  tibble(Drafted = nfl_val$drafted,
         LOG_Acc_Pred = predict(log_fit_Acc, new_data = nfl_val) %>% pull(),
         LOG_Acc_Prob = predict(log_fit_Acc, new_data = nfl_val, type = "prob") %>% pull(.pred_Yes),
         LOG_F_Pred = predict(log_fit_F, new_data = nfl_val) %>% pull(),
         LOG_F_Prob = predict(log_fit_F, new_data = nfl_val, type = "prob") %>% pull(.pred_Yes))


# ROC Curve

# - Accuracy
log_ROC_Acc <- log_Results %>%
  roc_curve(Drafted, LOG_Acc_Prob) %>% 
  mutate(Model = "Normal_Acc_p0.001_m0.5")
# - F Score
log_ROC_F <- log_Results %>%
  roc_curve(Drafted, LOG_F_Prob) %>% 
  mutate(Model = "Simple_F_p0.011_m0")

# - Visual
log_ROC_Acc %>% 
  bind_rows(log_ROC_F) %>% 
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
        legend.position = "bottom") +
  scale_color_manual(values = c("midnightblue","lightskyblue"))


# CONF MATRIX

# - Accuracy
log_CM_Acc <- log_Results %>% 
  conf_mat(truth = Drafted, estimate = LOG_Acc_Pred) %>% 
  autoplot(type = "heatmap") +
  labs(title = "Accuracy", subtitle = "(Normal): penalty = 0.001 | mix = 0.5") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, color = "midnightblue", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "orchid4"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.position = "none")

# - F Score
log_CM_F <- log_Results %>% 
  conf_mat(truth = Drafted, estimate = LOG_F_Pred) %>% 
  autoplot(type = "heatmap") +
  labs(title = "F Score", subtitle = "(Simple): penalty = 0.011 | mix = 0") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, color = "lightskyblue", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "orchid4"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.position = "none")
# - Visual
ggarrange(log_CM_Acc, log_CM_F, nrow = 1)


# Metrics
log_Metrics <-
# - Accuracy
  tibble(Model = "Logistic_Regression_Acc",
         AUC = roc_auc(log_Results, Drafted, LOG_Acc_Prob) %>% pull(.estimate) %>% round(3),
         Accuracy = accuracy(log_Results, truth = Drafted, estimate = LOG_Acc_Pred) %>% pull(.estimate) %>% round(3),
         Sensitivity = sens(log_Results, truth = Drafted, estimate = LOG_Acc_Pred) %>% pull(.estimate) %>% round(3),
         Specificity = spec(log_Results, truth = Drafted, estimate = LOG_Acc_Pred) %>% pull(.estimate) %>% round(3),
         Precision = precision(log_Results, truth = Drafted, estimate = LOG_Acc_Pred) %>% pull(.estimate) %>% round(3),
         Recall = recall(log_Results, truth = Drafted, estimate = LOG_Acc_Pred) %>% pull(.estimate) %>% round(3),
         F1 = f_meas(log_Results, truth = Drafted, estimate = LOG_Acc_Pred) %>% pull(.estimate) %>% round(3))


# - F Score    
log_Metrics <- log_Metrics %>% 
  bind_rows(
    tibble(Model = "Logistic_Regression_F",
           AUC = roc_auc(log_Results, Drafted, LOG_F_Prob) %>% pull(.estimate) %>% round(3),
           Accuracy = accuracy(log_Results, truth = Drafted, estimate = LOG_F_Pred) %>% pull(.estimate) %>% round(3),
           Sensitivity = sens(log_Results, truth = Drafted, estimate = LOG_F_Pred) %>% pull(.estimate) %>% round(3),
           Specificity = spec(log_Results, truth = Drafted, estimate = LOG_F_Pred) %>% pull(.estimate) %>% round(3),
           Precision = precision(log_Results, truth = Drafted, estimate = LOG_F_Pred) %>% pull(.estimate) %>% round(3),
           Recall = recall(log_Results, truth = Drafted, estimate = LOG_F_Pred) %>% pull(.estimate) %>% round(3),
           F1 = f_meas(log_Results, truth = Drafted, estimate = LOG_F_Pred) %>% pull(.estimate) %>% round(3))
  )


#
# Modeling: Validation Diagnostics - Random Forrest ----

# PREDICTIONS
rf_Results <- 
  tibble(Drafted = nfl_val$drafted,
         RF_Acc_Pred = predict(rf_fit_Acc, new_data = nfl_val) %>% pull(),
         RF_Acc_Prob = predict(rf_fit_Acc, new_data = nfl_val, type = "prob") %>% pull(.pred_Yes),
         RF_F_Pred = predict(rf_fit_F, new_data = nfl_val) %>% pull(),
         RF_F_Prob = predict(rf_fit_F, new_data = nfl_val, type = "prob") %>% pull(.pred_Yes))


# ROC Curve
# - Accuracy
rf_ROC_Acc <- rf_Results %>%
  roc_curve(Drafted, RF_Acc_Prob) %>% 
  mutate(Model = "Simple_Acc_mtry6_min9")
# - F Score
rf_ROC_F <- rf_Results %>%
  roc_curve(Drafted, RF_F_Prob) %>% 
  mutate(Model = "None_F_mtry1_min9")
# - Visual
rf_ROC_Acc %>% 
  bind_rows(rf_ROC_Acc, rf_ROC_F) %>% 
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
        legend.position = "bottom") +
  scale_color_manual(values = c("seagreen","seagreen1"))


# CONF MATRIX
# - Accuracy
rf_CM_Acc <- rf_Results %>% 
  conf_mat(truth = Drafted, estimate = RF_Acc_Pred) %>% 
  autoplot(type = "heatmap") +
  labs(title = "Accuracy", subtitle = "(Simple): mtry = 6 | min_n = 9") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, color = "seagreen1", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "orchid4"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.position = "none")
# - F Score
rf_CM_F <- rf_Results %>% 
  conf_mat(truth = Drafted, estimate = RF_F_Pred) %>% 
  autoplot(type = "heatmap") +
  labs(title = "F Score", subtitle = "(None): mtry = 1 | min_n = 9") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, color = "seagreen", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "orchid4"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.position = "none")
# - Visual
ggarrange(rf_CM_Acc, rf_CM_F, nrow = 1)

# Metrics
rf_Metrics <-
  # - Accuracy
  tibble(Model = "Random_Forrest_Acc",
         AUC = roc_auc(rf_Results, Drafted, RF_Acc_Prob) %>% pull(.estimate) %>% round(3),
         Accuracy = accuracy(rf_Results, truth = Drafted, estimate = RF_Acc_Pred) %>% pull(.estimate) %>% round(3),
         Sensitivity = sens(rf_Results, truth = Drafted, estimate = RF_Acc_Pred) %>% pull(.estimate) %>% round(3),
         Specificity = spec(rf_Results, truth = Drafted, estimate = RF_Acc_Pred) %>% pull(.estimate) %>% round(3),
         Precision = precision(rf_Results, truth = Drafted, estimate = RF_Acc_Pred) %>% pull(.estimate) %>% round(3),
         Recall = recall(rf_Results, truth = Drafted, estimate = RF_Acc_Pred) %>% pull(.estimate) %>% round(3),
         F1 = f_meas(rf_Results, truth = Drafted, estimate = RF_Acc_Pred) %>% pull(.estimate) %>% round(3))

# - F Score    
rf_Metrics <- rf_Metrics %>% 
  bind_rows(
    tibble(Model = "Random_Forrest_F",
           AUC = roc_auc(rf_Results, Drafted, RF_F_Prob) %>% pull(.estimate) %>% round(3),
           Accuracy = accuracy(rf_Results, truth = Drafted, estimate = RF_F_Pred) %>% pull(.estimate) %>% round(3),
           Sensitivity = sens(rf_Results, truth = Drafted, estimate = RF_F_Pred) %>% pull(.estimate) %>% round(3),
           Specificity = spec(rf_Results, truth = Drafted, estimate = RF_F_Pred) %>% pull(.estimate) %>% round(3),
           Precision = precision(rf_Results, truth = Drafted, estimate = RF_F_Pred) %>% pull(.estimate) %>% round(3),
           Recall = recall(rf_Results, truth = Drafted, estimate = RF_F_Pred) %>% pull(.estimate) %>% round(3),
           F1 = f_meas(rf_Results, truth = Drafted, estimate = RF_F_Pred) %>% pull(.estimate) %>% round(3))
  )

#
# Modeling: Refit: Probabiliy Threshold - Logistic Regression ----


# Accuracy (Normal) 71.7%: penaly = 0.001, mix = 0.5

# - thresholds
log_Thres <- log_Results %>% 
  threshold_perf(Drafted, LOG_Acc_Prob, threshold = seq(0.2, 1, by = 0.05))
  
log_Thres <- log_Thres %>% 
  filter(.metric != "distance") %>% 
  mutate(group = case_when(
                   .metric == "sens" | .metric == "spec" ~ "1",
                    TRUE ~ "2")
  )
# - max j (0.65)
max_j_log <- log_Thres %>% 
  filter(.metric == "j_index") %>% 
  filter(.estimate == max(.estimate)) %>% 
  pull(.threshold)
# - VISUAL
log_Thres %>% 
  ggplot(aes(.threshold, .estimate, color = .metric, alpha = group)) +
  geom_line(linetype = 2, size = 1) +
  geom_vline(xintercept = max_j_log, alpha = 0.6, size = 3, color = "palegreen4") +
  geom_vline(xintercept = 0.5, alpha = 0.6, linetype = 1, size = 0.5, color = "grey20") +
  labs(title = "Logistic Regression",
       subtitle = "(Normal): penalty = 0.001 | mix = 0.5",
       color = "Metric",
       x = "Optimal Threshold") +
  scale_alpha_manual(values = c(.09, 1), guide = "none") +
  scale_color_manual(values = c("palegreen4","red","blue"),
                     labels = c("J_Index", "Sensitivity", "Specificity")) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "orchid4"),
    axis.title.y = element_text(color = "tomato"),
    axis.title.x = element_text(hjust = 0.53, color = "palegreen4", face = "italic")
  )


# PREDICTIONS
log_Results_Thres <- 
  tibble(Drafted = nfl_val$drafted,
         LOG_Acc_Prob = predict(log_fit_Acc, new_data = nfl_val, type = "prob") %>% pull(.pred_Yes)) %>% 
  mutate(LOG_Acc_Pred65 = ifelse(LOG_Acc_Prob > 0.65, "Yes","No") %>% factor(levels = c("Yes","No")))

# CM
log_Results_Thres %>% 
  conf_mat(truth = Drafted, estimate = LOG_Acc_Pred65) %>% 
  autoplot(type = "heatmap") +
  labs(title = "Logistic Regression (Thres 65%)",
       subtitle = "(Normal): penalty = 0.001 | mix = 0") +
  theme_bw() +
  theme(plot.title = element_blank(),
        plot.subtitle = element_blank(),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.position = "none")

# Metrics
log_Thres_Metrics <-
  tibble(Model = "Logistic_Regression_Acc66",
         AUC = roc_auc(log_Results_Thres, Drafted, LOG_Acc_Prob) %>% pull(.estimate) %>% round(3),
         Accuracy = accuracy(log_Results_Thres, truth = Drafted, estimate = LOG_Acc_Pred65) %>% pull(.estimate) %>% round(3),
         Sensitivity = sens(log_Results_Thres, truth = Drafted, estimate = LOG_Acc_Pred65) %>% pull(.estimate) %>% round(3),
         Specificity = spec(log_Results_Thres, truth = Drafted, estimate = LOG_Acc_Pred65) %>% pull(.estimate) %>% round(3),
         Precision = precision(log_Results_Thres, truth = Drafted, estimate = LOG_Acc_Pred65) %>% pull(.estimate) %>% round(3),
         Recall = recall(log_Results_Thres, truth = Drafted, estimate = LOG_Acc_Pred65) %>% pull(.estimate) %>% round(3),
         F1 = f_meas(log_Results_Thres, truth = Drafted, estimate = LOG_Acc_Pred65) %>% pull(.estimate) %>% round(3)) 


#
# Modeling: Refit: Probabiliy Threshold - Random Forrest ----


# Accuracy (Simple) 70.5%: Mtry = 6, Min_n = 9
# - thresholds
rf_Thres <- rf_Results %>% 
  threshold_perf(Drafted, RF_Acc_Prob, threshold = seq(0.2, 1, by = 0.05))

rf_Thres <- log_Thres %>% 
  filter(.metric != "distance") %>% 
  mutate(group = case_when(
    .metric == "sens" | .metric == "spec" ~ "1",
    TRUE ~ "2")
  )
# - max j 0.65
max_j_rf <- rf_Thres %>% 
  filter(.metric == "j_index") %>% 
  filter(.estimate == max(.estimate)) %>% 
  pull(.threshold)
# - VISUAL
rf_Thres %>% 
  ggplot(aes(.threshold, .estimate, color = .metric, alpha = group)) +
  geom_line(linetype = 2, size = 1) +
  geom_vline(xintercept = max_j_rf, alpha = 0.6, size = 3, color = "palegreen4") +
  geom_vline(xintercept = 0.5, alpha = 0.6, linetype = 1, size = 1, color = "grey20") +
  labs(title = "Random Forrest",
       subtitle = "(None): mtry = 6 | min = 9",
       color = "Metric",
       x = "Optimal Threshold") +
  scale_alpha_manual(values = c(.08, 1), guide = "none") +
  scale_color_manual(values = c("palegreen4","red","blue"),
                     labels = c("J_Index", "Sensitivity", "Specificity")) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "orchid4"),
    axis.title.y = element_text(color = "tomato"),
    axis.title.x = element_text(hjust = 0.52, color = "palegreen4", face = "italic")
  )


# PREDICTIONS
rf_Results_Thres <- 
  tibble(Drafted = nfl_val$drafted,
         RF_Acc_Prob = predict(rf_fit_Acc, new_data = nfl_val, type = "prob") %>% pull(.pred_Yes)) %>% 
  mutate(RF_Acc_Pred65 = ifelse(RF_Acc_Prob > 0.65, "Yes","No") %>% factor(levels = c("Yes","No")))

# CM
rf_Results_Thres %>% 
  conf_mat(truth = Drafted, estimate = RF_Acc_Pred65) %>% 
  autoplot(type = "heatmap") +
  labs(title = "Random Forrest (Thres 60%)",
       subtitle = "Mtry = 2 | Mix = 9") +
  theme_bw() +
  theme(plot.title = element_blank(),
        plot.subtitle = element_blank(),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.position = "none")

# Metrics
rf_Thres_Metrics <-
  tibble(Model = "Random_Forrest_Acc65",
         AUC = roc_auc(rf_Results_Thres, Drafted, RF_Acc_Prob) %>% pull(.estimate) %>% round(3),
         Accuracy = accuracy(rf_Results_Thres, truth = Drafted, estimate = RF_Acc_Pred65) %>% pull(.estimate) %>% round(3),
         Sensitivity = sens(rf_Results_Thres, truth = Drafted, estimate = RF_Acc_Pred65) %>% pull(.estimate) %>% round(3),
         Specificity = spec(rf_Results_Thres, truth = Drafted, estimate = RF_Acc_Pred65) %>% pull(.estimate) %>% round(3),
         Precision = precision(rf_Results_Thres, truth = Drafted, estimate = RF_Acc_Pred65) %>% pull(.estimate) %>% round(3),
         Recall = recall(rf_Results_Thres, truth = Drafted, estimate = RF_Acc_Pred65) %>% pull(.estimate) %>% round(3),
         F1 = f_meas(rf_Results_Thres, truth = Drafted, estimate = RF_Acc_Pred65) %>% pull(.estimate) %>% round(3)) 


#
# Modeling: FINAL Diagnostics ----

# PREDICTIONS
Log_FINAL_Results <- 
  tibble(Drafted = nfl_test$drafted,
         LOG_Acc_Prob = predict(log_fit_Acc, new_data = nfl_test, type = "prob") %>% pull(.pred_Yes),
         LOG_Acc_Pred = predict(log_fit_Acc, new_data = nfl_test) %>% pull(),
         LOG_Acc65_Pred = ifelse(LOG_Acc_Prob > 0.65, "Yes","No") %>% factor(levels = c("Yes","No")),
         LOG_F_Prob = predict(log_fit_F, new_data = nfl_test, type = "prob") %>% pull(.pred_Yes),
         LOG_F_Pred = predict(log_fit_F, new_data = nfl_test) %>% pull())

rf_FINAL_Results <- 
  tibble(Drafted = nfl_test$drafted,
         RF_Acc_Prob = predict(rf_fit_Acc, new_data = nfl_test, type = "prob") %>% pull(.pred_Yes),
         RF_Acc_Pred = predict(rf_fit_Acc, new_data = nfl_test) %>% pull(),
         RF_Acc65_Pred = ifelse(RF_Acc_Prob > 0.65, "Yes","No") %>% factor(levels = c("Yes","No")),
         RF_F_Prob = predict(rf_fit_F, new_data = nfl_test, type = "prob") %>% pull(.pred_Yes),
         RF_F_Pred = predict(rf_fit_F, new_data = nfl_test) %>% pull())


# ROC Curve
# - Logistic Regression
Log_ROC_Acc_FINAL <- Log_FINAL_Results %>%
  roc_curve(Drafted, LOG_Acc_Prob) %>% 
  mutate(Model = "LOG_Normal_Acc_p0.001_m0.5")
Log_ROC_F_FINAL <- Log_FINAL_Results %>%
  roc_curve(Drafted, LOG_F_Prob) %>% 
  mutate(Model = "LOG_Simple_F_p011_m0")
# - Random Forrest
rf_ROC_Acc_FINAL <- rf_FINAL_Results %>%
  roc_curve(Drafted, RF_Acc_Prob) %>% 
  mutate(Model = "RF_Simple_Acc_mtry6_min9")
rf_ROC_F_FINAL <- rf_FINAL_Results %>%
  roc_curve(Drafted, RF_F_Prob) %>% 
  mutate(Model = "RF_None_F_mtry1_min9")
# - Visual
Log_ROC_Acc_FINAL %>% 
  bind_rows(Log_ROC_F_FINAL, rf_ROC_Acc_FINAL, rf_ROC_F_FINAL) %>% 
  mutate(specificity = 1 - specificity) %>% 
  ggplot(aes(specificity, sensitivity, color = Model)) +
  geom_line() + geom_abline(slope = 1, linetype = 2, alpha = 0.2) +
  labs(
    title = "ROC Curve", subtitle = "FINAL",
    x = "1 - specificity"
  ) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.5, color = "darkgreen", face = "bold"),
        axis.title.y = element_text(face = "bold", color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "tomato"), 
        legend.position = "bottom") +
  scale_color_manual(values = c("midnightblue","lightskyblue","seagreen1","seagreen"))


# CM
# - Logistic Regression
log_CM_FINAL_Acc <- Log_FINAL_Results %>% 
  conf_mat(truth = Drafted, estimate = LOG_Acc_Pred) %>% 
  autoplot(type = "heatmap") +
  labs(title = "Accuracy 50%",
       subtitle = "(Normal): penalty = 0.001 | mix = 0.5") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, color = "midnightblue", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "orchid4"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"),
        legend.position = "none")

log_CM_FINAL_Acc65 <- Log_FINAL_Results %>% 
  conf_mat(truth = Drafted, estimate = LOG_Acc65_Pred) %>% 
  autoplot(type = "heatmap") +
  labs(title = "Accuracy 65%",
       subtitle = "(Normal): penalty = 0.001 | mix = 0.5") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, color = "darkolivegreen", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "orchid4"),
        axis.text.y = element_blank(),
        axis.title = element_blank(), 
        axis.title.x = element_text(face = "bold", color = "cyan4"),
        legend.position = "none")

log_CM_FINAL_F <- Log_FINAL_Results %>% 
  conf_mat(truth = Drafted, estimate = LOG_F_Pred) %>% 
  autoplot(type = "heatmap") +
  labs(title = "F Score",
       subtitle = "(Simple): penalty = 0.011 | mix = 0") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, color = "lightskyblue", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "orchid4"),
        axis.title = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(face = "bold", color = "cyan4"),
        legend.position = "none")

ggarrange(log_CM_FINAL_Acc, log_CM_FINAL_F, nrow = 1)
ggarrange(log_CM_FINAL_Acc, log_CM_FINAL_Acc65, log_CM_FINAL_F, nrow = 1)

# - Random Forrest
rf_CM_FINAL_Acc <- rf_FINAL_Results %>% 
  conf_mat(truth = Drafted, estimate = RF_Acc_Pred) %>% 
  autoplot(type = "heatmap") +
  labs(title = "Accuracy 50%",
       subtitle = "(Simple): mtry = 6 | min = 9") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, color = "seagreen", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "orchid4"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"),
        legend.position = "none")

rf_CM_FINAL_Acc65 <- rf_FINAL_Results %>% 
  conf_mat(truth = Drafted, estimate = RF_Acc65_Pred) %>% 
  autoplot(type = "heatmap") +
  labs(title = "Accuracy 65%",
       subtitle = "(Simple): mtry = 6 | min = 9") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, color = "darkolivegreen", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "orchid4"),
        axis.title = element_blank(), 
        axis.text.y = element_blank(),
        axis.title.x = element_text(face = "bold", color = "cyan4"),
        legend.position = "none")

rf_CM_FINAL_F <- rf_FINAL_Results %>% 
  conf_mat(truth = Drafted, estimate = RF_F_Pred) %>% 
  autoplot(type = "heatmap") +
  labs(title = "F Score",
       subtitle = "(Normal): mtry = 1 | min = 9") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, color = "seagreen1", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "orchid4"),
        axis.title = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(face = "bold", color = "cyan4"),
        legend.position = "none")

ggarrange(rf_CM_FINAL_Acc, rf_CM_FINAL_F, nrow = 1)
ggarrange(rf_CM_FINAL_Acc, rf_CM_FINAL_Acc65, rf_CM_FINAL_F, nrow = 1)




# METRICS
# - Logistic Regression 50% (penalty = 0.001, mix = 0.5)
log_Acc_FINAL_Metrics <- 
  tibble(Model = "Logistic_Regression_Acc",
         AUC = roc_auc(Log_FINAL_Results, Drafted, LOG_Acc_Prob) %>% pull(.estimate) %>% round(3),
         Accuracy = accuracy(Log_FINAL_Results, truth = Drafted, estimate = LOG_Acc_Pred) %>% pull(.estimate) %>% round(3),
         Sensitivity = sens(Log_FINAL_Results, truth = Drafted, estimate = LOG_Acc_Pred) %>% pull(.estimate) %>% round(3),
         Specificity = spec(Log_FINAL_Results, truth = Drafted, estimate = LOG_Acc_Pred) %>% pull(.estimate) %>% round(3),
         Precision = precision(Log_FINAL_Results, truth = Drafted, estimate = LOG_Acc_Pred) %>% pull(.estimate) %>% round(3),
         Recall = recall(Log_FINAL_Results, truth = Drafted, estimate = LOG_Acc_Pred) %>% pull(.estimate) %>% round(3),
         F1 = f_meas(Log_FINAL_Results, truth = Drafted, estimate = LOG_Acc_Pred) %>% pull(.estimate) %>% round(3))
# - Logistic Regression 65% (penalty = 0.001, mix = 0.5)
log_Acc65_FINAL_Metrics <- 
  tibble(Model = "Logistic_Regression_Acc65",
         AUC = roc_auc(Log_FINAL_Results, Drafted, LOG_Acc_Prob) %>% pull(.estimate) %>% round(3),
         Accuracy = accuracy(Log_FINAL_Results, truth = Drafted, estimate = LOG_Acc65_Pred) %>% pull(.estimate) %>% round(3),
         Sensitivity = sens(Log_FINAL_Results, truth = Drafted, estimate = LOG_Acc65_Pred) %>% pull(.estimate) %>% round(3),
         Specificity = spec(Log_FINAL_Results, truth = Drafted, estimate = LOG_Acc65_Pred) %>% pull(.estimate) %>% round(3),
         Precision = precision(Log_FINAL_Results, truth = Drafted, estimate = LOG_Acc65_Pred) %>% pull(.estimate) %>% round(3),
         Recall = recall(Log_FINAL_Results, truth = Drafted, estimate = LOG_Acc65_Pred) %>% pull(.estimate) %>% round(3),
         F1 = f_meas(Log_FINAL_Results, truth = Drafted, estimate = LOG_Acc65_Pred) %>% pull(.estimate) %>% round(3))
# - Logistic Regression F (penalty = 0.011, mix = 0)
log_F_FINAL_Metrics <- 
  tibble(Model = "Logistic_Regression_F",
         AUC = roc_auc(Log_FINAL_Results, Drafted, LOG_F_Prob) %>% pull(.estimate) %>% round(3),
         Accuracy = accuracy(Log_FINAL_Results, truth = Drafted, estimate = LOG_F_Pred) %>% pull(.estimate) %>% round(3),
         Sensitivity = sens(Log_FINAL_Results, truth = Drafted, estimate = LOG_F_Pred) %>% pull(.estimate) %>% round(3),
         Specificity = spec(Log_FINAL_Results, truth = Drafted, estimate = LOG_F_Pred) %>% pull(.estimate) %>% round(3),
         Precision = precision(Log_FINAL_Results, truth = Drafted, estimate = LOG_F_Pred) %>% pull(.estimate) %>% round(3),
         Recall = recall(Log_FINAL_Results, truth = Drafted, estimate = LOG_F_Pred) %>% pull(.estimate) %>% round(3),
         F1 = f_meas(Log_FINAL_Results, truth = Drafted, estimate = LOG_F_Pred) %>% pull(.estimate) %>% round(3))

log_Metrics_FINAL <- log_Acc_FINAL_Metrics %>% 
  bind_rows(log_Acc65_FINAL_Metrics, log_F_FINAL_Metrics)


# - Random Forrest Acc 50% (mtry = 6, min = 9)
rf_Acc_FINAL_Metrics <- 
  tibble(Model = "Random_Forrest_Acc",
         AUC = roc_auc(rf_FINAL_Results, Drafted, RF_Acc_Prob) %>% pull(.estimate) %>% round(3),
         Accuracy = accuracy(rf_FINAL_Results, truth = Drafted, estimate = RF_Acc_Pred) %>% pull(.estimate) %>% round(3),
         Sensitivity = sens(rf_FINAL_Results, truth = Drafted, estimate = RF_Acc_Pred) %>% pull(.estimate) %>% round(3),
         Specificity = spec(rf_FINAL_Results, truth = Drafted, estimate = RF_Acc_Pred) %>% pull(.estimate) %>% round(3),
         Precision = precision(rf_FINAL_Results, truth = Drafted, estimate = RF_Acc_Pred) %>% pull(.estimate) %>% round(3),
         Recall = recall(rf_FINAL_Results, truth = Drafted, estimate = RF_Acc_Pred) %>% pull(.estimate) %>% round(3),
         F1 = f_meas(rf_FINAL_Results, truth = Drafted, estimate = RF_Acc_Pred) %>% pull(.estimate) %>% round(3))
# - Random Forrest Acc 65% (mtry = 6, min = 9)
rf_Acc65_FINAL_Metrics <- 
  tibble(Model = "Random_Forrest_Acc65",
         AUC = roc_auc(rf_FINAL_Results, Drafted, RF_Acc_Prob) %>% pull(.estimate) %>% round(3),
         Accuracy = accuracy(rf_FINAL_Results, truth = Drafted, estimate = RF_Acc65_Pred) %>% pull(.estimate) %>% round(3),
         Sensitivity = sens(rf_FINAL_Results, truth = Drafted, estimate = RF_Acc65_Pred) %>% pull(.estimate) %>% round(3),
         Specificity = spec(rf_FINAL_Results, truth = Drafted, estimate = RF_Acc65_Pred) %>% pull(.estimate) %>% round(3),
         Precision = precision(rf_FINAL_Results, truth = Drafted, estimate = RF_Acc65_Pred) %>% pull(.estimate) %>% round(3),
         Recall = recall(rf_FINAL_Results, truth = Drafted, estimate = RF_Acc65_Pred) %>% pull(.estimate) %>% round(3),
         F1 = f_meas(rf_FINAL_Results, truth = Drafted, estimate = RF_Acc65_Pred) %>% pull(.estimate) %>% round(3))
# - Random Forrest F (mtry = 1, min = 9)
rf_F_FINAL_Metrics <- 
  tibble(Model = "Random_Forrest_F",
         AUC = roc_auc(rf_FINAL_Results, Drafted, RF_F_Prob) %>% pull(.estimate) %>% round(3),
         Accuracy = accuracy(rf_FINAL_Results, truth = Drafted, estimate = RF_F_Pred) %>% pull(.estimate) %>% round(3),
         Sensitivity = sens(rf_FINAL_Results, truth = Drafted, estimate = RF_F_Pred) %>% pull(.estimate) %>% round(3),
         Specificity = spec(rf_FINAL_Results, truth = Drafted, estimate = RF_F_Pred) %>% pull(.estimate) %>% round(3),
         Precision = precision(rf_FINAL_Results, truth = Drafted, estimate = RF_F_Pred) %>% pull(.estimate) %>% round(3),
         Recall = recall(rf_FINAL_Results, truth = Drafted, estimate = RF_F_Pred) %>% pull(.estimate) %>% round(3),
         F1 = f_meas(rf_FINAL_Results, truth = Drafted, estimate = RF_F_Pred) %>% pull(.estimate) %>% round(3))


rf_Metrics_FINAL <- rf_Acc_FINAL_Metrics %>% 
  bind_rows(rf_Acc65_FINAL_Metrics, rf_F_FINAL_Metrics)

# VALIDATION Metrics
metrics_VAL <- log_Metrics %>% 
  bind_rows(log_Thres_Metrics, rf_Metrics, rf_Thres_Metrics)  

# FINAL Metrics
metrics_FINAL <- log_Metrics_FINAL %>% 
  bind_rows(rf_Metrics_FINAL)

#
# Feature Selection: VIP ----

log_fit_Acc %>% 
  pull_workflow_fit() %>% 
  vip(num_features = 20) +
  theme_bw()

rf_fit_F %>% 
  pull_workflow_fit() %>% 
  vip(num_features = 20) + 
  theme_bw()

log_fit_F %>% 
  pull_workflow_fit() %>% 
  vip(num_features = 20) + 
  theme_bw()

# DALEX
# Feature Selection: DALEX (Explainer) ----

# Preprocess
custom_func_Prob <- function(model, newdata) {
  predict(object = model, new_data = newdata, type = "prob")$.pred_Yes
}

custom_func_Pred <- function(model, newdata) {
  ifelse(predict(object = model, new_data = newdata)$.pred_class == "Yes",1,0)
}


# Explainer 
# - Logistic Regression FULL (Acc): P = 0.001, M = 0.5
EXP_log_Acc <- explain_tidymodels(model = log_fit_Acc,
                                  data = nfl_test,
                                  y = nfl_test$drafted == "Yes",
                                  predict_function = custom_func_Prob,
                                  label = "LOG-Acc")
# Random Forrest SIMPLE (F): Mtry = 6, Min = 9
EXP_rf_F <- explain_tidymodels(model = rf_fit_F,
                               data = nfl_test,
                               y = nfl_test$drafted == "Yes",
                               predict_function = custom_func_Prob,
                               label = "RF-F")

# Logistic Regression (F Score): P = 1, P = 9
EXP_log_F <- explain_tidymodels(model = log_fit_F,
                                data = nfl_test,
                                y = nfl_test$drafted == "Yes",
                                predict_function = custom_func_Prob,
                                label = "LOG-F")

# Explainer (WR | RB | OG | CB | DE)
# - Data
nfl_test_Positions <- nfl_test %>% 
  filter(position %in% c("WR","RB","OG","CB","DE"))
# - Random Forrest
EXP_rf_F_Positions <- explain_tidymodels(model = rf_fit_F,
                                         data = nfl_test_Positions,
                                         y = nfl_test_Positions$drafted == "Yes",
                                         predict_function = custom_func_Prob,
                                         label = "RF-F")
# - Logistic Regression
EXP_log_F_Positions <- explain_tidymodels(model = log_fit_F,
                                          data = nfl_test_Positions,
                                          y = nfl_test_Positions$drafted == "Yes",
                                          predict_function = custom_func_Prob,
                                          label = "LOG-F")

# Model-Performance
md_rf <- model_diagnostics(explainer = EXP_rf_F)
md_log <- model_diagnostics(explainer = EXP_log_F)

nfl_test_pred <- nfl_test %>% 
  mutate(
    rf_prob = md_rf$y_hat %>% round(3),
    log_prob = md_log$y_hat %>% round(3)
  ) %>% 
  mutate(
    rf_pred = ifelse(rf_prob > 0.5, "Yes","No"),
    log_pred = ifelse(log_prob > 0.5, "Yes","No")
  ) %>% 
  select(player, drafted, rf_prob, rf_pred, log_prob, log_pred,side:team)
  

#
# Feature Selection: DALEX (Dataset) ----

# Feature Importance
set.seed(101)
vip_log_Acc <- model_parts(explainer = EXP_log_Acc,
                           type = "difference",
                           B = 20)
set.seed(101)
vip_rf_F <- model_parts(explainer = EXP_rf_F,
                        type = "difference",
                        B = 20)
set.seed(101)
vip_log_F <- model_parts(explainer = EXP_log_F,
                         type = "difference",
                         B = 20)

plot(vip_log_Acc, vip_rf_F, vip_log_F,
     max_vars = 10) +
  labs(y = "Loss in AUC after permutations") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
    plot.subtitle = element_blank(),
    axis.title.x = element_text(color = "tomato")
  )

# Partial Dependency
vip_variables <- c("forty","weight","bench")

set.seed(101)
pdp_rf <- model_profile(explainer = EXP_rf_F,
                        variables = vip_variables)
pdp_log_F <- model_profile(explainer = EXP_log_F,
                           variables = vip_variables)

plot(pdp_rf, pdp_log_F) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "tomato", alpha = 0.7) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
    plot.subtitle = element_blank(),
    axis.title.y = element_text(color = "tomato")
  )


gg_pdp_rf <- plot(pdp_rf, geom = "profiles") +
  geom_hline(yintercept = 0.5, linetype = 2, color = "tomato", alpha = 0.7) +
  labs(title = "CP Profile: Random Forrest",
       subtitle = "(NONE: mtry = 1, min = 9)") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", color = "seagreen1"),
    plot.subtitle = element_text(hjust = 0.5, face = "italic", color = "orchid4"),
    axis.title.y = element_text(color = "tomato")
  )
gg_pdp_log <- plot(pdp_log_F, geom = "profiles") +
  geom_hline(yintercept = 0.5, linetype = 2, color = "tomato", alpha = 0.7) +
  labs(title = "CP Profile: Logistic Regression",
       subtitle = '(SIMPLE: penalty = 0.011, mix = 0 "LASSO"') +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", color = "lightskyblue"),
    plot.subtitle = element_text(hjust = 0.5, face = "italic", color = "orchid4"),
    axis.title.y = element_text(color = "tomato")
  ) 

ggarrange(gg_pdp_log, gg_pdp_rf, ncol = 1)

# - Categorical
pdp_rf_Position <- model_profile(explainer = EXP_rf_F,
                                 variables = "position",
                                 variable_type = "categorical")
pdp_log_F_Poistion <- model_profile(explainer = EXP_log_F,
                                    variables = "position",
                                    variable_type = "categorical")
pdp_rf_Conference <- model_profile(explainer = EXP_rf_F,
                                   variables = "conference",
                                   variable_type = "categorical")
pdp_log_F_Conference <- model_profile(explainer = EXP_log_F,
                                     variables = "conference",
                                     variable_type = "categorical")

gg_pdp_rf_Position <- plot(pdp_rf_Position) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "tomato", alpha = 0.7) +
  labs(title = "Random Forrest",
       subtitle = '(NONE: mtry = 1, min = 9)') +
  ylim(c(0,0.8)) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", color = "seagreen1"),
    plot.subtitle = element_text(hjust = 0.5, face = "italic", color = "orchid4"),
    axis.title.y = element_text(color = "tomato"),
    axis.text.x = element_text(angle = 45, hjust = 0.8),
    strip.text = element_blank())

gg_pdp_log_Position <- plot(pdp_log_F_Poistion) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "tomato", alpha = 0.7) +
  ylim(c(0,0.8)) +
  labs(title = "Logistic Regression",
       subtitle = '(SIMPLE: penalty = 0.011, mix = 0 "LASSO"') +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", color = "lightskyblue"),
    plot.subtitle = element_text(hjust = 0.5, face = "italic", color = "orchid4"),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 45),
    strip.text = element_blank())

gg_pdp_rf_Conference <- plot(pdp_rf_Conference) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "tomato", alpha = 0.7) +
  ylim(c(0,0.8)) +
  labs(title = "Random Forrest",
       subtitle = '(NONE: mtry = 1, min = 9)') +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", color = "seagreen1"),
    plot.subtitle = element_text(hjust = 0.5, face = "italic", color = "orchid4"),
    axis.title.y = element_text(color = "tomato"),
    axis.text.x = element_text(angle = 45, hjust = 0.8),
    strip.text = element_blank())
gg_pdp_log_Conference <- plot(pdp_log_F_Conference) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "tomato", alpha = 0.7) +
  ylim(c(0,0.8)) +
  labs(title = "Logistic Regression",
       subtitle = '(SIMPLE: penalty = 0.011, mix = 0 "LASSO"') +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", color = "lightskyblue"),
    plot.subtitle = element_text(hjust = 0.5, face = "italic", color = "orchid4"),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 0.8),
    strip.text = element_blank())

ggarrange(gg_pdp_rf_Position, gg_pdp_log_Position, nrow = 1)
ggarrange(gg_pdp_rf_Conference, gg_pdp_log_Conference, nrow = 1)


# Partial Dependency (Grouped)
# - Conference
set.seed(101)
pdp_conf_rf <- model_profile(explainer = EXP_rf_F,
                             variables = vip_variables,
                             groups = "conference")
pdp_conf_log_F <- model_profile(explainer = EXP_log_F,
                                variables = vip_variables,
                                groups = "conference")

plot(pdp_conf_rf) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "tomato", alpha = 0.7) +
  labs(title = "Random Forrest",
       subtitle = "(NONE: mtry = 1, min = 9)") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", color = "seagreen1"),
    plot.subtitle = element_text(hjust = 0.5, face = "italic", color = "orchid4"),
    axis.title.y = element_text(color = "tomato"),
    legend.position = "bottom"
  )

plot(pdp_conf_log_F) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "tomato", alpha = 0.7) +
  labs(title = "Logistic Regression",
       subtitle = '(SIMPLE: penalty = 0.011, mix = 0 "LASSO")') +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", color = "lightskyblue"),
    plot.subtitle = element_text(hjust = 0.5, face = "italic", color = "orchid4"),
    axis.title.y = element_text(color = "tomato"),
    legend.position = "bottom"
  )


# - Position
set.seed(101)
pdp_pos_rf_F <- model_profile(explainer = EXP_rf_F,
                              variables = vip_variables,
                              groups = "position")
pdp_pos_log_F <- model_profile(explainer = EXP_log_F,
                               variables = vip_variables,
                               groups = "position")

plot(pdp_pos_rf_F) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "tomato", alpha = 0.7) +
  labs(title = "Random Forrest",
       subtitle = "(NONE: mtry = 1, min = 9)") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", color = "seagreen1"),
    plot.subtitle = element_text(hjust = 0.5, face = "italic", color = "orchid4"),
    axis.title.y = element_text(color = "tomato"),
    legend.position = "bottom"
  )

plot(pdp_pos_log_F) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "tomato", alpha = 0.7) +
  labs(title = "Logistic Regression",
       subtitle = '(SIMPLE: penalty = 0.011, mix = 0 "LASSO")') +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", color = "lightskyblue"),
    plot.subtitle = element_text(hjust = 0.5, face = "italic", color = "orchid4"),
    axis.title.y = element_text(color = "tomato"),
    legend.position = "bottom"
  )

# - Positions (Filtered)
set.seed(101)
pdp_pos_rf_F_Filtered <- model_profile(explainer = EXP_rf_F_Positions,
                                       variables = vip_variables,
                                       groups = "position")
pdp_pos_log_F_Filtered <- model_profile(explainer = EXP_log_F_Positions,
                                        variables = vip_variables,
                                        groups = "position")

plot(pdp_pos_rf_F_Filtered) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "tomato", alpha = 0.7) +
  labs(title = "Random Forrest",
       subtitle = "(NONE: mtry = 1, min = 9)") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", color = "seagreen1"),
    plot.subtitle = element_text(hjust = 0.5, face = "italic", color = "orchid4"),
    axis.title.y = element_text(color = "tomato"),
    legend.position = "bottom"
  )

plot(pdp_pos_log_F_Filtered) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "tomato", alpha = 0.7) +
  labs(title = "Logistic Regression",
       subtitle = '(SIMPLE: penalty = 0.011, mix = 0 "LASSO")') +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", color = "lightskyblue"),
    plot.subtitle = element_text(hjust = 0.5, face = "italic", color = "orchid4"),
    axis.title.y = element_text(color = "tomato"),
    legend.position = "bottom"
  )

#
# Feature Selection: DALEX (Instance) ----

# Wide Recievers
# - Random Forrest-(NONE: mtry=1 | min = 9)
cm_WR_RF <- nfl_test_pred %>% 
  filter(position == "WR") %>%
  mutate(rf_pred = factor(rf_pred,levels = c("Yes","No"))) %>% 
  conf_mat(truth = drafted, estimate = rf_pred)
gg_cm_WR_RF <- cm_WR_RF %>%  
  autoplot(type = "heatmap") +
  labs(title = "Random Forrest", subtitle = "Wide Receivers") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, color = "seagreen1", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "orchid4"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.position = "none")
cm_WR_RF %>% summary()

# - Logistic Regression-(SIMPLE: penaly=0.011 | mix=0)
cm_WR_LOG <- nfl_test_pred %>% 
  filter(position == "WR") %>%
  mutate(rf_pred = factor(log_pred,levels = c("Yes","No"))) %>% 
  conf_mat(truth = drafted, estimate = rf_pred) 
gg_cm_WR_LOG <- cm_WR_LOG %>%   
  autoplot(type = "heatmap") +
  labs(title = "Logistic Regression", subtitle = "Wide Receivers") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, color = "midnightblue", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "orchid4"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.position = "none")
cm_WR_LOG %>% summary()

# Running Backs
# - Random Forrest-(NONE: mtry=1 | min = 9)
cm_RB_RF <- nfl_test_pred %>% 
  filter(position == "RB") %>%
  mutate(rf_pred = factor(rf_pred,levels = c("Yes","No"))) %>% 
  conf_mat(truth = drafted, estimate = rf_pred) 
gg_cm_RB_RF <- cm_RB_RF %>% 
  autoplot(type = "heatmap") +
  labs(title = "Random Forrest", subtitle = "Running Backs") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, color = "seagreen1", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "orchid4"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.position = "none")
cm_RB_RF %>% summary()

# - Logistic Regression-(SIMPLE: penaly=0.011 | mix=0)
cm_RB_LOG <- nfl_test_pred %>% 
  filter(position == "RB") %>%
  mutate(rf_pred = factor(rf_pred,levels = c("Yes","No"))) %>% 
  conf_mat(truth = drafted, estimate = rf_pred)
gg_cm_RB_LOG <- cm_RB_LOG %>% 
  autoplot(type = "heatmap") +
  labs(title = "Logistic Regression", subtitle = "Running Backs") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, color = "midnightblue", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "orchid4"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.position = "none")
cm_RB_LOG %>% summary()

# Offensive Guards
# - Random Forrest-(NONE: mtry=1 | min = 9)
cm_OG_RF <- nfl_test_pred %>% 
  filter(position == "OG") %>%
  mutate(rf_pred = factor(rf_pred,levels = c("Yes","No"))) %>% 
  conf_mat(truth = drafted, estimate = rf_pred) 
gg_cm_OG_RF <- cm_OG_RF %>% 
  autoplot(type = "heatmap") +
  labs(title = "Random Forrest", subtitle = "Offensive Guards") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, color = "seagreen1", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "orchid4"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.position = "none")
cm_OG_RF %>% summary()

# - Logistic Regression-(SIMPLE: penaly=0.011 | mix=0)
cm_OG_LOG <- nfl_test_pred %>% 
  filter(position == "OG") %>%
  mutate(rf_pred = factor(log_pred,levels = c("Yes","No"))) %>% 
  conf_mat(truth = drafted, estimate = rf_pred)
gg_cm_OG_LOG <- cm_OG_LOG %>% 
  autoplot(type = "heatmap") +
  labs(title = "Logistic Regression", subtitle = "Offensive Guards") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, color = "midnightblue", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "orchid4"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.position = "none")
cm_OG_LOG %>% summary()

# Cornerbacks
# - Random Forrest-(NONE: mtry=1 | min = 9)
cm_CB_RF <- nfl_test_pred %>% 
  filter(position == "CB") %>%
  mutate(rf_pred = factor(rf_pred,levels = c("Yes","No"))) %>% 
  conf_mat(truth = drafted, estimate = rf_pred)
gg_cm_CB_RF <- cm_CB_RF %>% 
  autoplot(type = "heatmap") +
  labs(title = "Random Forrest", subtitle = "Cornerbacks") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, color = "seagreen1", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "orchid4"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.position = "none")
cm_CB_RF %>% summary()
# - Logistic Regression-(SIMPLE: penaly=0.011 | mix=0)
cm_CB_LOG <- nfl_test_pred %>% 
  filter(position == "CB") %>%
  mutate(rf_pred = factor(log_pred,levels = c("Yes","No"))) %>% 
  conf_mat(truth = drafted, estimate = rf_pred) 
gg_cm_CB_LOG <- cm_CB_LOG %>% 
  autoplot(type = "heatmap") +
  labs(title = "Logistic Regression", subtitle = "Cornerbacks") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, color = "midnightblue", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "orchid4"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.position = "none")
cm_CB_LOG %>% summary()

# Defensive Ends
# - Random Forrest-(NONE: mtry=1 | min = 9)
cm_DE_RF <- nfl_test_pred %>% 
  filter(position == "DE") %>%
  mutate(rf_pred = factor(rf_pred,levels = c("Yes","No"))) %>% 
  conf_mat(truth = drafted, estimate = rf_pred)
gg_cm_DE_RF <- cm_DE_RF %>% 
  autoplot(type = "heatmap") +
  labs(title = "Random Forrest", subtitle = "Defensive End") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, color = "seagreen1", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "orchid4"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.position = "none")
cm_DE_RF %>% summary()
# - Logistic Regression-(SIMPLE: penaly=0.011 | mix=0)
cm_DE_LOG <- nfl_test_pred %>% 
  filter(position == "DE") %>%
  mutate(rf_pred = factor(log_pred,levels = c("Yes","No"))) %>% 
  conf_mat(truth = drafted, estimate = rf_pred)
gg_cm_DE_LOG <- cm_DE_LOG %>%   
  autoplot(type = "heatmap") +
  labs(title = "Logistic Regression", subtitle = "Defensive End") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, color = "midnightblue", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "orchid4"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.position = "none")
cm_DE_LOG %>% summary()

# Visual
ggarrange(gg_cm_WR_RF, gg_cm_RB_RF, gg_cm_OG_RF, gg_cm_CB_RF, gg_cm_DE_RF, nrow = 1)
ggarrange(gg_cm_WR_LOG, gg_cm_RB_LOG, gg_cm_OG_LOG, gg_cm_CB_LOG, gg_cm_DE_LOG, nrow = 1)
#
# Feature Selection: DALEX (WR) ----

# DATA
nfl_test_pred_WR <- nfl_test_pred %>% 
  filter(player %in% c("Sammy Watkins","Randall Cobb","Marques Colston","Marcus Lucas"))
# - drafted
nfl_test_pred %>% filter(position == "WR" & drafted == "Yes") %>% view()
nfl_test_SW <- nfl_test %>% filter(player == "Sammy Watkins")
nfl_test_RC <- nfl_test %>% filter(player == "Randall Cobb")
nfl_test_MC <- nfl_test %>% filter(player == "Marques Colston")
# - not drafted
nfl_test_ML <- nfl_test %>% filter(player == "Marcus Lucas")



# BREAKDOWN
# - Sammy Watkins
bd_rf_F_SW <- predict_parts(explainer = EXP_rf_F,
                           new_observation = nfl_test_SW,
                           type = "break_down",
                           keep_distributions = TRUE)
bd_log_F_SW <- predict_parts(explainer = EXP_log_F,
                             new_observation = nfl_test_SW,
                             type = "break_down",
                             keep_distributions = TRUE)

gg_bd_rf_WR_SW <- plot(bd_rf_F_SW, max_features = 5) +
  labs(title = "Sammy Watkins",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_bd_log_WR_SW <- plot(bd_log_F_SW, max_features = 5) +
  labs(title = "Sammy Watkins",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())


# - Randall Cobb
bd_rf_F_RC <- predict_parts(explainer = EXP_rf_F,
                            new_observation = nfl_test_RC,
                            type = "break_down",
                            keep_distributions = TRUE)
bd_log_F_RC <- predict_parts(explainer = EXP_log_F,
                             new_observation = nfl_test_RC,
                             type = "break_down",
                             keep_distributions = TRUE)

gg_bd_rf_WR_RC <- plot(bd_rf_F_RC, max_features = 5) +
  labs(title = "Randall Cobb",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_bd_log_WR_RC <- plot(bd_log_F_RC, max_features = 5) +
  labs(title = "Randall Cobb",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# - Marques Colston
bd_rf_F_MC <- predict_parts(explainer = EXP_rf_F,
                            new_observation = nfl_test_MC,
                            type = "break_down",
                            keep_distributions = TRUE)
bd_log_F_MC <- predict_parts(explainer = EXP_log_F,
                             new_observation = nfl_test_MC,
                             type = "break_down",
                             keep_distributions = TRUE)

gg_bd_rf_WR_MC <- plot(bd_rf_F_MC, max_features = 5) +
  labs(title = "Marques Colston",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_bd_log_WR_MC <- plot(bd_log_F_MC, max_features = 5) +
  labs(title = "Marques Colston",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())


# - Marcus Lucas
bd_rf_F_ML <- predict_parts(explainer = EXP_rf_F,
                            new_observation = nfl_test_ML,
                            type = "break_down",
                            keep_distributions = TRUE)
bd_log_F_ML <- predict_parts(explainer = EXP_log_F,
                             new_observation = nfl_test_ML,
                             type = "break_down",
                             keep_distributions = TRUE)

gg_bd_rf_WR_ML <- plot(bd_rf_F_ML, max_features = 5) +
  labs(title = "Marcus Lucas",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "tomato"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_bd_log_WR_ML <- plot(bd_log_F_ML, max_features = 5) +
  labs(title = "Marcus Lucas",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "tomato"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# VISUAL
ggarrange(gg_bd_rf_WR_ML, gg_bd_rf_WR_SW, gg_bd_rf_WR_RC, gg_bd_rf_WR_LS, gg_bd_rf_WR_MC, gg_bd_rf_WR_RK, 
          nrow = 2, ncol = 3)
ggarrange(gg_bd_log_WR_ML, gg_bd_log_WR_SW, gg_bd_log_WR_RC, gg_bd_log_WR_LS, gg_bd_log_WR_MC, gg_bd_log_WR_RK, 
          nrow = 2, ncol = 3)





# SHAP
# - Marcus Lucas
shap_rf_ML <- predict_parts(explainer = EXP_rf_F,
                            new_observation = nfl_test_ML,
                            type = "shap",
                            B = 20)
shap_log_ML <- predict_parts(explainer = EXP_log_F,
                             new_observation = nfl_test_ML,
                             type = "shap",
                             B = 20)

gg_shap_rf_WR_ML <- plot(shap_rf_ML) +
  labs(title = "Marcus Lucas",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "tomato"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_shap_log_WR_ML <- plot(shap_log_ML) +
  labs(title = "Marcus Lucas",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "tomato"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# - Sammy Watkins
shap_rf_SW <- predict_parts(explainer = EXP_rf_F,
                            new_observation = nfl_test_SW,
                            type = "shap",
                            B = 20)
shap_log_SW <- predict_parts(explainer = EXP_log_F,
                             new_observation = nfl_test_SW,
                             type = "shap",
                             B = 20)

gg_shap_rf_WR_SW <- plot(shap_rf_SW) +
  labs(title = "Sammy Watkins",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_shap_log_WR_SW <- plot(shap_log_SW) +
  labs(title = "Sammy Watkins",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# - Randall Cobb
shap_rf_RC <- predict_parts(explainer = EXP_rf_F,
                            new_observation = nfl_test_RC,
                            type = "shap",
                            B = 20)
shap_log_RC <- predict_parts(explainer = EXP_log_F,
                             new_observation = nfl_test_RC,
                             type = "shap",
                             B = 20)

gg_shap_rf_WR_RC <- plot(shap_rf_RC) +
  labs(title = "Randall Cobb",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_shap_log_WR_RC <- plot(shap_log_RC) +
  labs(title = "Randall Cobb",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# - Marques Colston
shap_rf_MC <- predict_parts(explainer = EXP_rf_F,
                            new_observation = nfl_test_MC,
                            type = "shap",
                            B = 20)
shap_log_MC <- predict_parts(explainer = EXP_log_F,
                             new_observation = nfl_test_MC,
                             type = "shap",
                             B = 20)

gg_shap_rf_WR_MC <- plot(shap_rf_MC) +
  labs(title = "Marques Colston",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_shap_log_WR_MC <- plot(shap_log_MC) +
  labs(title = "Marques Colston",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# VISUAL
ggarrange(gg_shap_rf_WR_ML, gg_shap_rf_WR_SW, gg_shap_rf_WR_RC, gg_shap_rf_WR_LS, gg_shap_rf_WR_MC, gg_shap_rf_WR_RK,
          nrow = 2, ncol = 3)
ggarrange(gg_shap_log_WR_ML, gg_shap_log_WR_SW, gg_shap_log_WR_RC, gg_shap_log_WR_LS, gg_shap_log_WR_MC, gg_shap_log_WR_RK,
          nrow = 2, ncol = 3)

#
# Feature Selection: DALEX (RB) ----

# Data
nfl_test_pred_RB <- nfl_test_pred %>% 
  filter(player %in% c("Nick Chubb","Maurice Jones-Drew","Mike Davis","Nate Ilaoa","LeGarrette Blount","Mike Bell"))
# - drafted
nfl_test_pred %>% filter(position == "RB" & drafted == "Yes") %>% view()
nfl_test_NC <- nfl_test %>% filter(player == "Nick Chubb")
nfl_test_MJD <- nfl_test %>% filter(player == "Maurice Jones-Drew")
nfl_test_MD <- nfl_test %>% filter(player == "Mike Davis")
nfl_test_NI <- nfl_test %>% filter(player == "Nate Ilaoa")
# - not drafted
nfl_test_pred %>% filter(position == "RB" & drafted == "No") %>% view()
nfl_test_LB <- nfl_test %>% filter(player == "LeGarrette Blount")
nfl_test_MB <- nfl_test %>% filter(player == "Mike Bell")


# SHAP
# - Nick Chubb
shap_rf_NC <- predict_parts(explainer = EXP_rf_F,
                            new_observation = nfl_test_NC,
                            type = "shap",
                            B = 20)
shap_log_NC <- predict_parts(explainer = EXP_log_F,
                             new_observation = nfl_test_NC,
                             type = "shap",
                             B = 20)

gg_shap_rf_WR_NC <- plot(shap_rf_NC) +
  labs(title = "Nick Chubb",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_shap_log_WR_NC <- plot(shap_log_NC) +
  labs(title = "Nick Chubb",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# - Mike Davis
shap_rf_MD <- predict_parts(explainer = EXP_rf_F,
                            new_observation = nfl_test_MD,
                            type = "shap",
                            B = 20)
shap_log_MD <- predict_parts(explainer = EXP_log_F,
                             new_observation = nfl_test_MD,
                             type = "shap",
                             B = 20)

gg_shap_rf_WR_MD <- plot(shap_rf_MD) +
  labs(title = "Mike Davis",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_shap_log_WR_MD <- plot(shap_log_MD) +
  labs(title = "Mike Davis",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# - LaGarrette Blount
shap_rf_LB <- predict_parts(explainer = EXP_rf_F,
                            new_observation = nfl_test_LB,
                            type = "shap",
                            B = 20)
shap_log_LB <- predict_parts(explainer = EXP_log_F,
                             new_observation = nfl_test_LB,
                             type = "shap",
                             B = 20)

gg_shap_rf_WR_LB <- plot(shap_rf_LB) +
  labs(title = "LaGarrette Blount",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "tomato"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_shap_log_WR_LB <- plot(shap_log_LB) +
  labs(title = "LaGarrette Blount",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "tomato"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# - Nate Ilaoa
shap_rf_NI <- predict_parts(explainer = EXP_rf_F,
                            new_observation = nfl_test_NI,
                            type = "shap",
                            B = 20)
shap_log_NI <- predict_parts(explainer = EXP_log_F,
                             new_observation = nfl_test_NI,
                             type = "shap",
                             B = 20)

gg_shap_rf_WR_NI <- plot(shap_rf_NI) +
  labs(title = "Nate Ilaoa",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_shap_log_WR_NI <- plot(shap_log_NI) +
  labs(title = "Nate Ilaoa",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# - Mike Bell
shap_rf_MB <- predict_parts(explainer = EXP_rf_F,
                            new_observation = nfl_test_MB,
                            type = "shap",
                            B = 20)
shap_log_MB <- predict_parts(explainer = EXP_log_F,
                             new_observation = nfl_test_MB,
                             type = "shap",
                             B = 20)

gg_shap_rf_WR_MB <- plot(shap_rf_MB) +
  labs(title = "Mike Bell",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "tomato"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_shap_log_WR_MB <- plot(shap_log_MB) +
  labs(title = "Mike Bell",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "tomato"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# - Maurice Jones-Drew
shap_rf_MJD <- predict_parts(explainer = EXP_rf_F,
                             new_observation = nfl_test_MJD,
                             type = "shap",
                             B = 20)
shap_log_MJD <- predict_parts(explainer = EXP_log_F,
                              new_observation = nfl_test_MJD,
                              type = "shap",
                              B = 20)

gg_shap_rf_WR_MJD <- plot(shap_rf_MJD) +
  labs(title = "Maurice Jones-Drew",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_shap_log_WR_MJD <- plot(shap_log_MJD) +
  labs(title = "Maurice Jones-Drew",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# Visual
ggarrange(gg_shap_rf_WR_NC, gg_shap_rf_WR_MD, gg_shap_rf_WR_LB, gg_shap_rf_WR_NI, gg_shap_rf_WR_MB, gg_shap_rf_WR_MJD,
          nrow = 2, ncol = 3)
ggarrange(gg_shap_log_WR_NC, gg_shap_log_WR_MD, gg_shap_log_WR_LB, gg_shap_log_WR_NI, gg_shap_log_WR_MB, gg_shap_log_WR_MJD,
          nrow = 2, ncol = 3)



# BREAKDOWN
# - Nick Chubb
bd_rf_NC <- predict_parts(explainer = EXP_rf_F,
                          new_observation = nfl_test_NC,
                          type = "break_down",
                          keep_distributions = TRUE)
bd_log_NC <- predict_parts(explainer = EXP_log_F,
                           new_observation = nfl_test_NC,
                           type = "break_down",
                           keep_distributions = TRUE)

gg_bd_rf_WR_NC <- plot(bd_rf_NC, max_features = 5) +
  labs(title = "Nick Chubb",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_bd_log_WR_NC <- plot(bd_log_NC, max_features = 5) +
  labs(title = "Nick Chubb",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# - Mike Davis
bd_rf_MD <- predict_parts(explainer = EXP_rf_F,
                          new_observation = nfl_test_MD,
                          type = "break_down",
                          keep_distributions = TRUE)
bd_log_MD <- predict_parts(explainer = EXP_log_F,
                           new_observation = nfl_test_MD,
                           type = "break_down",
                           keep_distributions = TRUE)

gg_bd_rf_WR_MD <- plot(bd_rf_MD, max_features = 5) +
  labs(title = "Mike Davis",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_bd_log_WR_MD <- plot(bd_log_MD, max_features = 5) +
  labs(title = "Mike Davis",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# - LaGarrette Blount
bd_rf_LB <- predict_parts(explainer = EXP_rf_F,
                          new_observation = nfl_test_LB,
                          type = "break_down",
                          keep_distributions = TRUE)
bd_log_LB <- predict_parts(explainer = EXP_log_F,
                           new_observation = nfl_test_LB,
                           type = "break_down",
                           keep_distributions = TRUE)

gg_bd_rf_WR_LB <- plot(bd_rf_LB, max_features = 5) +
  labs(title = "LaGarrette Blount",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "tomato"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_bd_log_WR_LB <- plot(bd_log_LB, max_features = 5) +
  labs(title = "LaGarrette Blount",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "tomato"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# - Nate Ilaoa
bd_rf_NI <- predict_parts(explainer = EXP_rf_F,
                          new_observation = nfl_test_NI,
                          type = "break_down",
                          keep_distributions = TRUE)
bd_log_NI <- predict_parts(explainer = EXP_log_F,
                           new_observation = nfl_test_NI,
                           type = "break_down",
                           keep_distributions = TRUE)

gg_bd_rf_WR_NI <- plot(bd_rf_NI, max_features = 5) +
  labs(title = "Nate Ilaoa",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_bd_log_WR_NI <- plot(bd_log_NI, max_features = 5) +
  labs(title = "Nate Ilaoa",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# - Mike Bell
bd_rf_MB <- predict_parts(explainer = EXP_rf_F,
                          new_observation = nfl_test_MB,
                          type = "break_down",
                          keep_distributions = TRUE)
bd_log_MB <- predict_parts(explainer = EXP_log_F,
                           new_observation = nfl_test_MB,
                           type = "break_down",
                           keep_distributions = TRUE)

gg_bd_rf_WR_MB <- plot(bd_rf_MB, max_features = 5) +
  labs(title = "Mike Bell",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "tomato"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_bd_log_WR_MB <- plot(bd_log_MB, max_features = 5) +
  labs(title = "Mike Bell",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "tomato"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# - Maurice Jones-Drew
bd_rf_MJD <- predict_parts(explainer = EXP_rf_F,
                           new_observation = nfl_test_MJD,
                           type = "break_down",
                           keep_distributions = TRUE)
bd_log_MJD <- predict_parts(explainer = EXP_log_F,
                            new_observation = nfl_test_MJD,
                            type = "break_down",
                            keep_distributions = TRUE)

gg_bd_rf_WR_MJD <- plot(bd_rf_MJD, max_features = 5) +
  labs(title = "Maurice Jones-Drew",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_bd_log_WR_MJD <- plot(bd_log_MJD, max_features = 5) +
  labs(title = "Maurice Jones-Drew",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# Visual
ggarrange(gg_bd_rf_WR_NC, gg_bd_rf_WR_MD, gg_bd_rf_WR_LB, gg_bd_rf_WR_NI, gg_bd_rf_WR_MB, gg_bd_rf_WR_MJD,
          nrow = 2, ncol = 3)
ggarrange(gg_bd_log_WR_NC, gg_bd_log_WR_MD, gg_bd_log_WR_LB, gg_bd_log_WR_NI, gg_bd_log_WR_MB, gg_bd_log_WR_MJD,
          nrow = 2, ncol = 3)
#
# Feature Selection: DALEX (OG) ----

# Data
nfl_test_pred_OG <- nfl_test_pred %>%
  filter(player %in% c("Steve Schilling","Dion Dawkins","Leander Jordan","Mike Iupati","Ryan Groy","Tony Tella"))
# - CM
# - drafted
nfl_test_pred %>% filter(position == "OG" & drafted == "Yes")
nfl_test_SS <- nfl_test %>% filter(player == "Steve Schilling")
nfl_test_DD <- nfl_test %>% filter(player == "Dion Dawkins")
nfl_test_LJ <- nfl_test %>% filter(player == "Leander Jordan")
nfl_test_MI <- nfl_test %>% filter(player == "Mike Iupati")
# - not drafted
nfl_test_pred %>% filter(position == "OG" & drafted == "No")
nfl_test_RG <- nfl_test %>% filter(player == "Ryan Groy")
nfl_test_TT <- nfl_test %>% filter(player == "Tony Tella")


# SHAP
# - Dion Dawkins
shap_rf_DD <- predict_parts(explainer = EXP_rf_F,
                            new_observation = nfl_test_DD,
                            type = "shap",
                            B = 20)
shap_log_DD <- predict_parts(explainer = EXP_log_F,
                             new_observation = nfl_test_DD,
                             type = "shap",
                             B = 20)

gg_shap_rf_OG_DD <- plot(shap_rf_DD) +
  labs(title = "Dion Dawkins",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_shap_log_OG_DD <- plot(shap_log_DD) +
  labs(title = "Dion Dawkins",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# -	Ryan Groy
shap_rf_RG <- predict_parts(explainer = EXP_rf_F,
                            new_observation = nfl_test_RG,
                            type = "shap",
                            B = 20)
shap_log_RG <- predict_parts(explainer = EXP_log_F,
                             new_observation = nfl_test_RG,
                             type = "shap",
                             B = 20)

gg_shap_rf_OG_RG <- plot(shap_rf_RG) +
  labs(title = "Ryan Groy",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "tomato"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_shap_log_OG_RG <- plot(shap_log_RG) +
  labs(title = "Ryan Groy",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "tomato"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# - Steve Schilling
shap_rf_SS <- predict_parts(explainer = EXP_rf_F,
                            new_observation = nfl_test_SS,
                            type = "shap",
                            B = 20)
shap_log_SS <- predict_parts(explainer = EXP_log_F,
                             new_observation = nfl_test_SS,
                             type = "shap",
                             B = 20)

gg_shap_rf_OG_SS <- plot(shap_rf_SS) +
  labs(title = "Steve Schilling",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_shap_log_OG_SS <- plot(shap_log_SS) +
  labs(title = "Steve Schilling",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# - Mike Iupati
shap_rf_MI <- predict_parts(explainer = EXP_rf_F,
                            new_observation = nfl_test_MI,
                            type = "shap",
                            B = 20)
shap_log_MI <- predict_parts(explainer = EXP_log_F,
                             new_observation = nfl_test_MI,
                             type = "shap",
                             B = 20)

gg_shap_rf_OG_MI <- plot(shap_rf_MI) +
  labs(title = "Mike Iupati",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_shap_log_OG_MI <- plot(shap_log_MI) +
  labs(title = "Mike Iupati",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# - Tony Tella
shap_rf_TT <- predict_parts(explainer = EXP_rf_F,
                            new_observation = nfl_test_TT,
                            type = "shap",
                            B = 20)
shap_log_TT <- predict_parts(explainer = EXP_log_F,
                             new_observation = nfl_test_TT,
                             type = "shap",
                             B = 20)

gg_shap_rf_OG_TT <- plot(shap_rf_TT) +
  labs(title = "Tony Tella",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "tomato"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_shap_log_OG_TT <- plot(shap_log_TT) +
  labs(title = "Tony Tella",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "tomato"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# -	Leander Jordan
shap_rf_LJ <- predict_parts(explainer = EXP_rf_F,
                            new_observation = nfl_test_LJ,
                            type = "shap",
                            B = 20)
shap_log_LJ <- predict_parts(explainer = EXP_log_F,
                             new_observation = nfl_test_LJ,
                             type = "shap",
                             B = 20)

gg_shap_rf_OG_LJ <- plot(shap_rf_LJ) +
  labs(title = "Leander Jordan",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_shap_log_OG_LJ <- plot(shap_log_LJ) +
  labs(title = "Leander Jordan",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# Visual
ggarrange(gg_shap_rf_OG_DD, gg_shap_rf_OG_RG, gg_shap_rf_OG_SS, gg_shap_rf_OG_MI, gg_shap_rf_OG_TT, gg_shap_rf_OG_LJ,
          nrow = 2, ncol = 3)
ggarrange(gg_shap_log_OG_DD, gg_shap_log_OG_RG, gg_shap_log_OG_SS, gg_shap_log_OG_MI, gg_shap_log_OG_TT, gg_shap_log_OG_LJ,
          nrow = 2, ncol = 3)


# BREAKDOWN
# - Dion Dawkins
bd_rf_DD <- predict_parts(explainer = EXP_rf_F,
                          new_observation = nfl_test_DD,
                          type = "break_down",
                          keep_distributions = TRUE)
bd_log_DD <- predict_parts(explainer = EXP_log_F,
                           new_observation = nfl_test_DD,
                           type = "break_down",
                           keep_distributions = TRUE)

gg_bd_rf_OG_DD <- plot(bd_rf_DD, max_features = 5) +
  labs(title = "Dion Dawkins",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_bd_log_OG_DD <- plot(bd_log_DD, max_features = 5) +
  labs(title = "Dion Dawkins",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# -	Ryan Groy
bd_rf_RG <- predict_parts(explainer = EXP_rf_F,
                          new_observation = nfl_test_RG,
                          type = "break_down",
                          keep_distributions = TRUE)
bd_log_RG <- predict_parts(explainer = EXP_log_F,
                           new_observation = nfl_test_RG,
                           type = "break_down",
                           keep_distributions = TRUE)

gg_bd_rf_OG_RG <- plot(bd_rf_RG, max_features = 5) +
  labs(title = "Ryan Groy",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "tomato"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_bd_log_OG_RG <- plot(bd_log_RG, max_features = 5) +
  labs(title = "Ryan Groy",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "tomato"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# - Steve Schilling
bd_rf_SS <- predict_parts(explainer = EXP_rf_F,
                          new_observation = nfl_test_SS,
                          type = "break_down",
                          keep_distributions = TRUE)
bd_log_SS <- predict_parts(explainer = EXP_log_F,
                           new_observation = nfl_test_SS,
                           type = "break_down",
                           keep_distributions = TRUE)

gg_bd_rf_OG_SS <- plot(bd_rf_SS, max_features = 5) +
  labs(title = "Steve Schilling",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_bd_log_OG_SS <- plot(bd_log_SS, max_features = 5) +
  labs(title = "Steve Schilling",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# - Mike Iupati
bd_rf_MI <- predict_parts(explainer = EXP_rf_F,
                          new_observation = nfl_test_MI,
                          type = "break_down",
                          keep_distributions = TRUE)
bd_log_MI <- predict_parts(explainer = EXP_log_F,
                           new_observation = nfl_test_MI,
                           type = "break_down",
                           keep_distributions = TRUE)

gg_bd_rf_OG_MI <- plot(bd_rf_MI, max_features = 5) +
  labs(title = "Mike Iupati",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_bd_log_OG_MI <- plot(bd_log_MI, max_features = 5) +
  labs(title = "Mike Iupati",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# - Tony Tella
bd_rf_TT <- predict_parts(explainer = EXP_rf_F,
                          new_observation = nfl_test_TT,
                          type = "break_down",
                          keep_distributions = TRUE)
bd_log_TT <- predict_parts(explainer = EXP_log_F,
                           new_observation = nfl_test_TT,
                           type = "break_down",
                           keep_distributions = TRUE)

gg_bd_rf_OG_TT <- plot(bd_rf_TT, max_features = 5) +
  labs(title = "Tony Tella",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "tomato"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_bd_log_OG_TT <- plot(bd_log_TT, max_features = 5) +
  labs(title = "Tony Tella",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "tomato"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# -	Leander Jordan
bd_rf_LJ <- predict_parts(explainer = EXP_rf_F,
                          new_observation = nfl_test_LJ,
                          type = "break_down",
                          keep_distributions = TRUE)
bd_log_LJ <- predict_parts(explainer = EXP_log_F,
                           new_observation = nfl_test_LJ,
                           type = "break_down",
                           keep_distributions = TRUE)

gg_bd_rf_OG_LJ <- plot(bd_rf_LJ, max_features = 5) +
  labs(title = "Leander Jordan",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_bd_log_OG_LJ <- plot(bd_log_LJ, max_features = 5) +
  labs(title = "Leander Jordan",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# Visual
ggarrange(gg_bd_rf_OG_DD, gg_bd_rf_OG_RG, gg_bd_rf_OG_SS, gg_bd_rf_OG_MI, gg_bd_rf_OG_TT, gg_bd_rf_OG_LJ,
          nrow = 2, ncol = 3)
ggarrange(gg_bd_log_OG_DD, gg_bd_log_OG_RG, gg_bd_log_OG_SS, gg_bd_log_OG_MI, gg_bd_log_OG_TT, gg_bd_log_OG_LJ,
          nrow = 2, ncol = 3)
#
# Feature Selection: DALEX (CB) ----

# Data
nfl_test_pred_CB <- nfl_test_pred %>%
  filter(player %in% c("Eric Rowe","Prince Amukamara","Brandon Dixon","Quandre Diggs","Tony Brown","Channing Stribling"))
# - drafted
nfl_test_pred %>% filter(position == "CB" & drafted == "Yes")
nfl_test_ER <- nfl_test %>% filter(player == "Eric Rowe")
nfl_test_PA <- nfl_test %>% filter(player == "Prince Amukamara")
nfl_test_BD <- nfl_test %>% filter(player == "Brandon Dixon")
nfl_test_QD <- nfl_test %>% filter(player == "Quandre Diggs")
# - not drafted
nfl_test_pred %>% filter(position == "CB" & drafted == "No")
nfl_test_TB <- nfl_test %>% filter(player == "Tony Brown")
nfl_test_CS <- nfl_test %>% filter(player == "Channing Stribling")


# SHAP
# - Tony Brown
shap_rf_TB <- predict_parts(explainer = EXP_rf_F,
                            new_observation = nfl_test_TB,
                            type = "shap",
                            B = 20)
shap_log_TB <- predict_parts(explainer = EXP_log_F,
                             new_observation = nfl_test_TB,
                             type = "shap",
                             B = 20)

gg_shap_rf_CB_TB <- plot(shap_rf_TB) +
  labs(title = "Tony Brown",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "tomato"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_shap_log_CB_TB <- plot(shap_log_TB) +
  labs(title = "Tony Brown",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "tomato"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# - Channing Stribling
shap_rf_CS <- predict_parts(explainer = EXP_rf_F,
                            new_observation = nfl_test_CS,
                            type = "shap",
                            B = 20)
shap_log_CS <- predict_parts(explainer = EXP_log_F,
                             new_observation = nfl_test_CS,
                             type = "shap",
                             B = 20)

gg_shap_rf_CB_CS <- plot(shap_rf_CS) +
  labs(title = "Channing Stribling",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "tomato"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_shap_log_CB_CS <- plot(shap_log_CS) +
  labs(title = "Channing Stribling",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "tomato"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# - Quandre Diggs
shap_rf_QD <- predict_parts(explainer = EXP_rf_F,
                            new_observation = nfl_test_QD,
                            type = "shap",
                            B = 20)
shap_log_QD <- predict_parts(explainer = EXP_log_F,
                             new_observation = nfl_test_QD,
                             type = "shap",
                             B = 20)

gg_shap_rf_CB_QD <- plot(shap_rf_QD) +
  labs(title = "Quandre Diggs",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_shap_log_CB_QD <- plot(shap_log_QD) +
  labs(title = "Quandre Diggs",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# - Eric Rowe
shap_rf_ER <- predict_parts(explainer = EXP_rf_F,
                            new_observation = nfl_test_ER,
                            type = "shap",
                            B = 20)
shap_log_ER <- predict_parts(explainer = EXP_log_F,
                             new_observation = nfl_test_ER,
                             type = "shap",
                             B = 20)

gg_shap_rf_CB_ER <- plot(shap_rf_ER) +
  labs(title = "Eric Rowe",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_shap_log_CB_ER <- plot(shap_log_ER) +
  labs(title = "Eric Rowe",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# - Brandon Dixon
shap_rf_BD <- predict_parts(explainer = EXP_rf_F,
                            new_observation = nfl_test_BD,
                            type = "shap",
                            B = 20)
shap_log_BD <- predict_parts(explainer = EXP_log_F,
                             new_observation = nfl_test_BD,
                             type = "shap",
                             B = 20)

gg_shap_rf_CB_BD <- plot(shap_rf_BD) +
  labs(title = "Brandon Dixon",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_shap_log_CB_BD <- plot(shap_log_BD) +
  labs(title = "Brandon Dixon",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# - Prince Amukamara
shap_rf_PA <- predict_parts(explainer = EXP_rf_F,
                            new_observation = nfl_test_PA,
                            type = "shap",
                            B = 20)
shap_log_PA <- predict_parts(explainer = EXP_log_F,
                             new_observation = nfl_test_PA,
                             type = "shap",
                             B = 20)

gg_shap_rf_CB_PA <- plot(shap_rf_PA) +
  labs(title = "Prince Amukamara",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_shap_log_CB_PA <- plot(shap_log_PA) +
  labs(title = "Prince Amukamara",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# Visual
ggarrange(gg_shap_rf_CB_TB, gg_shap_rf_CB_CS, gg_shap_rf_CB_QD, gg_shap_rf_CB_ER, gg_shap_rf_CB_BD, gg_shap_rf_CB_PA,
          nrow = 2, ncol = 3)
ggarrange(gg_shap_log_CB_TB, gg_shap_log_CB_CS, gg_shap_log_CB_QD, gg_shap_log_CB_ER, gg_shap_log_CB_BD, gg_shap_log_CB_PA,
          nrow = 2, ncol = 3)



# BREAKDOWN
# - Tony Brown
bd_rf_TB <- predict_parts(explainer = EXP_rf_F,
                          new_observation = nfl_test_TB,
                          type = "break_down",
                          keep_distributions = TRUE)
bd_log_TB <- predict_parts(explainer = EXP_log_F,
                           new_observation = nfl_test_TB,
                           type = "break_down",
                           keep_distributions = TRUE)

gg_bd_rf_CB_TB <- plot(bd_rf_TB, max_features = 5) +
  labs(title = "Tony Brown",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "tomato"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_bd_log_CB_TB <- plot(bd_log_TB, max_features = 5) +
  labs(title = "Dion Dawkins",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "tomato"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# - Channing Stribling
bd_rf_CS <- predict_parts(explainer = EXP_rf_F,
                          new_observation = nfl_test_CS,
                          type = "break_down",
                          keep_distributions = TRUE)
bd_log_CS <- predict_parts(explainer = EXP_log_F,
                           new_observation = nfl_test_CS,
                           type = "break_down",
                           keep_distributions = TRUE)

gg_bd_rf_CB_CS <- plot(bd_rf_CS, max_features = 5) +
  labs(title = "Channing Stribling",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "tomato"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_bd_log_CB_CS <- plot(bd_log_CS, max_features = 5) +
  labs(title = "Channing Stribling",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "tomato"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# - Quandre Diggs
bd_rf_QD <- predict_parts(explainer = EXP_rf_F,
                          new_observation = nfl_test_QD,
                          type = "break_down",
                          keep_distributions = TRUE)
bd_log_QD <- predict_parts(explainer = EXP_log_F,
                           new_observation = nfl_test_QD,
                           type = "break_down",
                           keep_distributions = TRUE)

gg_bd_rf_CB_QD <- plot(bd_rf_QD, max_features = 5) +
  labs(title = "Quandre Diggs",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_bd_log_CB_QD <- plot(bd_log_QD, max_features = 5) +
  labs(title = "Quandre Diggs",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# - Eric Rowe
bd_rf_ER <- predict_parts(explainer = EXP_rf_F,
                          new_observation = nfl_test_ER,
                          type = "break_down",
                          keep_distributions = TRUE)
bd_log_ER <- predict_parts(explainer = EXP_log_F,
                           new_observation = nfl_test_ER,
                           type = "break_down",
                           keep_distributions = TRUE)

gg_bd_rf_CB_ER <- plot(bd_rf_ER, max_features = 5) +
  labs(title = "Eric Rowe",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_bd_log_CB_ER <- plot(bd_log_ER, max_features = 5) +
  labs(title = "Eric Rowe",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# - Brandon Dixon
bd_rf_BD <- predict_parts(explainer = EXP_rf_F,
                          new_observation = nfl_test_BD,
                          type = "break_down",
                          keep_distributions = TRUE)
bd_log_BD <- predict_parts(explainer = EXP_log_F,
                           new_observation = nfl_test_BD,
                           type = "break_down",
                           keep_distributions = TRUE)

gg_bd_rf_CB_BD <- plot(bd_rf_BD, max_features = 5) +
  labs(title = "Brandon Dixon",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_bd_log_CB_BD <- plot(bd_log_BD, max_features = 5) +
  labs(title = "Brandon Dixon",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# - Prince Amukamara
bd_rf_PA <- predict_parts(explainer = EXP_rf_F,
                          new_observation = nfl_test_PA,
                          type = "break_down",
                          keep_distributions = TRUE)
bd_log_PA <- predict_parts(explainer = EXP_log_F,
                           new_observation = nfl_test_PA,
                           type = "break_down",
                           keep_distributions = TRUE)

gg_bd_rf_CB_PA <- plot(bd_rf_PA, max_features = 5) +
  labs(title = "Prince Amukamara",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_bd_log_CB_PA <- plot(bd_log_PA, max_features = 5) +
  labs(title = "Prince Amukamara",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# Visual
ggarrange(gg_bd_rf_CB_TB, gg_bd_rf_CB_CS, gg_bd_rf_CB_QD, gg_bd_rf_CB_ER, gg_bd_rf_CB_BD, gg_bd_rf_CB_PA,
          nrow = 2, ncol = 3)
ggarrange(gg_bd_log_CB_TB, gg_bd_log_CB_CS, gg_bd_log_CB_QD, gg_bd_log_CB_ER, gg_bd_log_CB_BD, gg_bd_log_CB_PA,
          nrow = 2, ncol = 3)
#
# Feature Selection: DALEX (DE) ----

# Data
nfl_test_pred_DE <- nfl_test_pred %>%
  filter(player %in% c("Ryan Kerrigan","Brett Keisel","Michael Sam","John Frank","Mike Kudla","James Cowser"))
# - drafted
nfl_test_pred %>% filter(position == "DE" & drafted == "Yes")
nfl_test_RK <- nfl_test %>% filter(player == "Ryan Kerrigan")
nfl_test_BK <- nfl_test %>% filter(player == "Brett Keisel")
nfl_test_MS <- nfl_test %>% filter(player == "Michael Sam")
# - not drafted
nfl_test_pred %>% filter(position == "DE" & drafted == "No")
nfl_test_JF <- nfl_test %>% filter(player == "John Frank")
nfl_test_MK <- nfl_test %>% filter(player == "Mike Kudla")
nfl_test_JC <- nfl_test %>% filter(player == "James Cowser")

# SHAP
# - James Cowser
shap_rf_JC <- predict_parts(explainer = EXP_rf_F,
                            new_observation = nfl_test_JC,
                            type = "shap",
                            B = 20)
shap_log_JC <- predict_parts(explainer = EXP_log_F,
                             new_observation = nfl_test_JC,
                             type = "shap",
                             B = 20)

gg_shap_rf_DE_JC <- plot(shap_rf_JC) +
  labs(title = "James Cowser",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "tomato"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_shap_log_DE_JC <- plot(shap_log_JC) +
  labs(title = "James Cowser",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "tomato"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# - Michael Sam
shap_rf_MS <- predict_parts(explainer = EXP_rf_F,
                            new_observation = nfl_test_MS,
                            type = "shap",
                            B = 20)
shap_log_MS <- predict_parts(explainer = EXP_log_F,
                             new_observation = nfl_test_MS,
                             type = "shap",
                             B = 20)

gg_shap_rf_DE_MS <- plot(shap_rf_MS) +
  labs(title = "Michael Sam",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_shap_log_DE_MS <- plot(shap_log_MS) +
  labs(title = "Michael Sam",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# - Ryan Kerrigan
shap_rf_RK <- predict_parts(explainer = EXP_rf_F,
                            new_observation = nfl_test_RK,
                            type = "shap",
                            B = 20)
shap_log_RK <- predict_parts(explainer = EXP_log_F,
                             new_observation = nfl_test_RK,
                             type = "shap",
                             B = 20)

gg_shap_rf_DE_RK <- plot(shap_rf_RK) +
  labs(title = "Ryan Kerrigan",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_shap_log_DE_RK <- plot(shap_log_RK) +
  labs(title = "Ryan Kerrigan",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# - Mike Kudla
shap_rf_MK <- predict_parts(explainer = EXP_rf_F,
                            new_observation = nfl_test_MK,
                            type = "shap",
                            B = 20)
shap_log_MK <- predict_parts(explainer = EXP_log_F,
                             new_observation = nfl_test_MK,
                             type = "shap",
                             B = 20)

gg_shap_rf_DE_MK <- plot(shap_rf_MK) +
  labs(title = "Mike Kudla",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "tomato"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_shap_log_DE_MK <- plot(shap_log_MK) +
  labs(title = "Mike Kudla",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "tomato"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# - Brett Keisel
shap_rf_BK <- predict_parts(explainer = EXP_rf_F,
                            new_observation = nfl_test_BK,
                            type = "shap",
                            B = 20)
shap_log_BK <- predict_parts(explainer = EXP_log_F,
                             new_observation = nfl_test_BK,
                             type = "shap",
                             B = 20)

gg_shap_rf_DE_BK <- plot(shap_rf_BK) +
  labs(title = "Brett Keisel",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_shap_log_DE_BK <- plot(shap_log_BK) +
  labs(title = "Brett Keisel",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())


# - John Frank
shap_rf_JF <- predict_parts(explainer = EXP_rf_F,
                            new_observation = nfl_test_JF,
                            type = "shap",
                            B = 20)
shap_log_JF <- predict_parts(explainer = EXP_log_F,
                             new_observation = nfl_test_JF,
                             type = "shap",
                             B = 20)

gg_shap_rf_DE_JF <- plot(shap_rf_JF) +
  labs(title = "John Frank",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "tomato"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_shap_log_DE_JF <- plot(shap_log_JF) +
  labs(title = "John Frank",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "tomato"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# Visual
ggarrange(gg_shap_rf_DE_JC, gg_shap_rf_DE_MS, gg_shap_rf_DE_RK, gg_shap_rf_DE_MK, gg_shap_rf_DE_BK, gg_shap_rf_DE_JF,
          nrow = 2, ncol = 3)
ggarrange(gg_shap_log_DE_JC, gg_shap_log_DE_MS, gg_shap_log_DE_RK, gg_shap_log_DE_MK, gg_shap_log_DE_BK, gg_shap_log_DE_JF,
          nrow = 2, ncol = 3)

# BREAKDOWN
# - James Cowser
bd_rf_JC <- predict_parts(explainer = EXP_rf_F,
                          new_observation = nfl_test_JC,
                          type = "break_down",
                          keep_distributions = TRUE)
bd_log_JC <- predict_parts(explainer = EXP_log_F,
                           new_observation = nfl_test_JC,
                           type = "break_down",
                           keep_distributions = TRUE)

gg_bd_rf_DE_JC <- plot(bd_rf_JC, max_features = 5) +
  labs(title = "James Cowser",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "tomato"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_bd_log_DE_JC <- plot(bd_log_JC, max_features = 5) +
  labs(title = "James Cowser",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "tomato"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# - Michael Sam
bd_rf_MS <- predict_parts(explainer = EXP_rf_F,
                          new_observation = nfl_test_MS,
                          type = "break_down",
                          keep_distributions = TRUE)
bd_log_MS <- predict_parts(explainer = EXP_log_F,
                           new_observation = nfl_test_MS,
                           type = "break_down",
                           keep_distributions = TRUE)

gg_bd_rf_DE_MS <- plot(bd_rf_MS, max_features = 5) +
  labs(title = "Michael Sam",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_bd_log_DE_MS <- plot(bd_log_MS, max_features = 5) +
  labs(title = "Michael Sam",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# - Ryan Kerrigan
bd_rf_RK <- predict_parts(explainer = EXP_rf_F,
                          new_observation = nfl_test_RK,
                          type = "break_down",
                          keep_distributions = TRUE)
bd_log_RK <- predict_parts(explainer = EXP_log_F,
                           new_observation = nfl_test_RK,
                           type = "break_down",
                           keep_distributions = TRUE)

gg_bd_rf_DE_RK <- plot(bd_rf_RK, max_features = 5) +
  labs(title = "Ryan Kerrigan",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_bd_log_DE_RK <- plot(bd_log_RK, max_features = 5) +
  labs(title = "Ryan Kerrigan",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# - Mike Kudla
bd_rf_MK <- predict_parts(explainer = EXP_rf_F,
                          new_observation = nfl_test_MK,
                          type = "break_down",
                          keep_distributions = TRUE)
bd_log_MK <- predict_parts(explainer = EXP_log_F,
                           new_observation = nfl_test_MK,
                           type = "break_down",
                           keep_distributions = TRUE)

gg_bd_rf_DE_MK <- plot(bd_rf_MK, max_features = 5) +
  labs(title = "Mike Kudla",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "tomato"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_bd_log_DE_MK <- plot(bd_log_MK, max_features = 5) +
  labs(title = "Mike Kudla",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "tomato"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# - Brett Keisel
bd_rf_BK <- predict_parts(explainer = EXP_rf_F,
                          new_observation = nfl_test_BK,
                          type = "break_down",
                          keep_distributions = TRUE)
bd_log_BK <- predict_parts(explainer = EXP_log_F,
                           new_observation = nfl_test_BK,
                           type = "break_down",
                           keep_distributions = TRUE)

gg_bd_rf_DE_BK <- plot(bd_rf_BK, max_features = 5) +
  labs(title = "Brett Keisel",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_bd_log_DE_BK <- plot(bd_log_BK, max_features = 5) +
  labs(title = "Brett Keisel",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "darkolivegreen"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

# - John Frank
bd_rf_JF <- predict_parts(explainer = EXP_rf_F,
                          new_observation = nfl_test_JF,
                          type = "break_down",
                          keep_distributions = TRUE)
bd_log_JF <- predict_parts(explainer = EXP_log_F,
                           new_observation = nfl_test_JF,
                           type = "break_down",
                           keep_distributions = TRUE)

gg_bd_rf_DE_JF <- plot(bd_rf_JF, max_features = 5) +
  labs(title = "John Frank",
       subtitle = "Random Forrest") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "tomato"),
        plot.subtitle = element_text(hjust = 0.5, color = "seagreen1"),
        strip.text = element_blank())
gg_bd_log_DE_JF <- plot(bd_log_JF, max_features = 5) +
  labs(title = "John Frank",
       subtitle = "Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "tomato"),
        plot.subtitle = element_text(hjust = 0.5, color = "lightskyblue"),
        strip.text = element_blank())

ggarrange(gg_bd_rf_DE_JC, gg_bd_rf_DE_MS, gg_bd_rf_DE_RK, gg_bd_rf_DE_MK, gg_bd_rf_DE_BK, gg_bd_rf_DE_JF,
          nrow = 2, ncol = 3)
ggarrange(gg_bd_log_DE_JC, gg_bd_log_DE_MS, gg_bd_log_DE_RK, gg_bd_log_DE_MK, gg_bd_log_DE_BK, gg_bd_log_DE_JF,
          nrow = 2, ncol = 3)
