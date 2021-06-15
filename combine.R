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
  labs(title = "Position",
       caption = "Year: 2000 - 2018") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "none",
    plot.caption = element_text(color = "gray70", face = "italic")
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
  scale_fill_manual(values = c("gray80", "forestgreen"))
gg_Combine_Weight_Defense_draft <- nfl_draft %>% 
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
  scale_fill_manual(values = c("gray80", "forestgreen")) 
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
  scale_fill_manual(values = c("gray80", "forestgreen"))
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
  scale_fill_manual(values = c("gray80", "forestgreen"))
gg_Combine_Bench_Defense_draft <- nfl_draft %>% 
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
  scale_fill_manual(values = c("gray80", "forestgreen"))
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
  scale_fill_manual(values = c("gray80", "forestgreen"))
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
  juice() %>% 
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
  juice() %>% 
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
  juice() %>% 
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
  juice() %>% 
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
  juice() %>% 
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
  juice() %>% 
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
# - simple
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
  labs(title = "Radnom Forrest",
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

# F Score (Simple) 79.9%: Mtry = 1, Min_n = 9
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
  mutate(Model = "None_Acc_mtry6_min9")
# - F Score
rf_ROC_F <- rf_Results %>%
  roc_curve(Drafted, RF_F_Prob) %>% 
  mutate(Model = "Simple_F_mtry1_min9")
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
  labs(title = "Accuracy", subtitle = "(None): mtry = 6 | min_n = 9") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, color = "seagreen", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "orchid4"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"), 
        legend.position = "none")
# - F Score
rf_CM_F <- rf_Results %>% 
  conf_mat(truth = Drafted, estimate = RF_F_Pred) %>% 
  autoplot(type = "heatmap") +
  labs(title = "F Score", subtitle = "(Simple): mtry = 1 | min_n = 9") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, color = "seagreen1", face = "bold"),
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
  mutate(Model = "RF_None_Acc_mtry6_min9")
rf_ROC_F_FINAL <- rf_FINAL_Results %>%
  roc_curve(Drafted, RF_F_Prob) %>% 
  mutate(Model = "RF_Simple_F_mtry1_min9")
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
  scale_color_manual(values = c("midnightblue","lightskyblue","seagreen","seagreen1"))


# CM
# - Logistic Regression
log_CM_FINAL_Acc <- Log_FINAL_Results %>% 
  conf_mat(truth = Drafted, estimate = LOG_Acc_Pred) %>% 
  autoplot(type = "heatmap") +
  labs(title = "Accuracy 50%",
       subtitle = "(Normal): penalty = 0.001 | mix = 0.5") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, color = "darkolivegreen", face = "bold"),
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
  theme(plot.title = element_text(hjust = 0.5, color = "darkolivegreen", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "orchid4"),
        axis.title = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(face = "bold", color = "cyan4"),
        legend.position = "none")

ggarrange(log_CM_FINAL_Acc, log_CM_FINAL_Acc65, log_CM_FINAL_F, nrow = 1)

# - Random Forrest
rf_CM_FINAL_Acc <- rf_FINAL_Results %>% 
  conf_mat(truth = Drafted, estimate = RF_Acc_Pred) %>% 
  autoplot(type = "heatmap") +
  labs(title = "Accuracy 50%",
       subtitle = "(None): mtry = 6 | min = 9") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, color = "darkolivegreen", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "orchid4"),
        axis.title.y = element_text(color = "tomato"),
        axis.title.x = element_text(face = "bold", color = "cyan4"),
        legend.position = "none")

rf_CM_FINAL_Acc65 <- rf_FINAL_Results %>% 
  conf_mat(truth = Drafted, estimate = RF_Acc65_Pred) %>% 
  autoplot(type = "heatmap") +
  labs(title = "Accuracy 65%",
       subtitle = "(None): mtry = 6 | min = 9") +
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
       subtitle = "(Simple): mtry = 1 | min = 9") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, color = "darkolivegreen", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "orchid4"),
        axis.title = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(face = "bold", color = "cyan4"),
        legend.position = "none")

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


# Top Models: 
# - F Score
#       1) 80.2%: Random Forrest.Acc
#       2) 79.8%: Random Forrest.F
# - Accuracy
#       1) 71.7%: Random Forrest.Acc
#       2) 71.4%: Random Forrest.F
# - Sensitivity
#       1) 94.7%: Random Forrest.F
#       2) 89.9%: Logistic Regression.F


# Variable Importance: Impurity
# - Random Forrest.F (Mtry = 1 | Min = 1)
# - vip
log_fit_Acc %>% 
  pull_workflow_fit() %>% 
  vip(num_features = 20) +
  theme_bw()

# - Logistic Regression.F (Penalty = 0.006 | Mix = 1)
# - vo[
# - vip
log_fit_F %>% 
  pull_workflow_fit() %>% 
  vip(num_features = 20) + 
  theme_bw()

# DALEX
# Feature Selection: DALEX ----

# Preprocess
custom_func_Prob <- function(object, newdata) {
  predict(log_fit_Acc, new_data = newdata, type = "prob")$.pred_Yes
}

custom_func_Pred <- function(object, newdata) {
  ifelse(predict(log_fit_Acc, new_data = newdata)$.pred_class == "Yes",1,0)
}


# Explainer 
EXP_log_Acc_Prob <- explain_tidymodels(model = log_fit_Acc,
                                       data = nfl_test,
                                       y = nfl_test$drafted == "Yes",
                                       predict_function = custom_func_Prob,
                                       label = "Logistic Regression (Prob)")
EXP_log_Acc_Pred <- explain_tidymodels(model = log_fit_Acc,
                                       data = nfl_test,
                                       y = nfl_test$drafted == "Yes",
                                       predict_function = custom_func_Pred,
                                       label = "Logistic Regression (Pred)")

# Dataset-Level
# model
# - performance
mp_log_Prob <- model_performance(EXP_log_Acc_Prob)
mp_log_Pred <- model_performance(EXP_log_Acc_Pred)

# - feature importance
set.seed(101)
vip_log_Prob <- model_parts(explainer = EXP_log_Acc_Prob,
                            type = "variable_importance",
                            B = 20)
set.seed(101)
vip_log_Pred <- model_parts(explainer = EXP_log_Acc_Pred,
                            type = "difference",
                            B = 20)

plot(vip_log_Prob, vip_log_Pred, show_boxplots = FALSE)

# - partial dependency
vip_variables <- c("forty","weight")

set.seed(101)
pdp_log_Prob <- model_profile(explainer = EXP_log_Acc_Prob)
set.seed(101)
pdp_log_Pred <- model_profile(explainer = EXP_log_Acc_Pred)

pdp_pos_log_Prob <- model_profile(explainer = EXP_log_Acc_Prob,
                                  variables = vip_variables,
                                  groups = "position")
pdp_pos_log_Pred <- model_profile(explainer = EXP_log_Acc_Pred,
                                  variables = vip_variables,
                                  groups = "position")

pdp_conf_log_Prob <- model_profile(explainer = EXP_log_Acc_Prob,
                                   variables = vip_variables,
                                   groups = "conference")

pdp_conf_log_Pred <- model_profile(explainer = EXP_log_Acc_Pred,
                                   variables = vip_variables,
                                   groups = "conference")

plot(pdp_log_Prob, pdp_log_Pred)

plot(pdp_pos_log_Prob)
plot(pdp_pos_log_Pred)

plot(pdp_conf_log_Prob)
plot(pdp_conf_log_Pred)

# Instance-Level: Running Backs
nfl_test_DC <- nfl_test %>% filter(player == "Dalvin Cook")

# - breakdown
bd_log_DC_Prob <- predict_parts(explainer = EXP_log_Acc_Prob,
                                new_observation = nfl_test_DC,
                                type = "break_down")

bd_log_DC_Pred <- predict_parts(explainer = EXP_log_Acc_Pred,
                                new_observation = nfl_test_DC,
                                type = "break_down")

plot(bd_log_DC_Prob)

# Instance-Level: Offensive Takles
nfl_test_KM <- nfl_test %>% filter(player == "Kolton Miller")

# - breakdown
bd_log_KM_Prob <- predict_parts(explainer = EXP_log_Acc_Prob,
                                new_observation = nfl_test_KM,
                                type = "break_down")
bd_log_KM_Pred <- predict_parts(explainer = EXP_log_Acc_Pred,
                                new_observation = nfl_test_KM,
                                type = "break_down")

plot(bd_log_KM_Prob)
plot(bd_log_KM_Pred)
