library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
theme_set(theme_minimal())
library(broom)
library(stringr)
library(forcats)
library(FactoMineR)
library(factoextra)
library(tidymodels)
library(workflowsets)

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


nfl <- nfl %>% 
  select(player, side, position, school, conference, year, everything())




#
# Exploratory Data Analysis ----

# Count
nfl %>%
  count(position) %>% 
  mutate(position = fct_reorder(position, n)) %>% 
  ggplot(aes(position,n)) +
  geom_col()
nfl %>%
  count(conference) %>% 
  mutate(conference = fct_reorder(conference, n)) %>% 
  ggplot(aes(conference,n)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combine Data: Weight
nfl_mean_weight <- nfl$weight %>% mean
nfl %>% 
  ggplot(aes(weight)) +
  geom_histogram() + 
  geom_vline(xintercept = nfl_mean_weight, color = "red")
# - By poisition
nfl %>% 
  ggplot(aes(weight, fill = side)) +
  geom_density(alpha = 0.3)
nfl %>% 
  ggplot(aes(position, weight, fill = side)) +
  geom_boxplot() + geom_hline(yintercept = nfl_mean_weight, color = "red") +
  geom_jitter(alpha = 0.09)
# - By Conference
nfl %>%
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
nfl_mean_forty <- nfl$forty %>% mean
nfl %>% 
  ggplot(aes(forty)) +
  geom_histogram() + 
  geom_vline(xintercept = nfl_mean_forty, color = "red")
# - By poisition
nfl %>% 
  ggplot(aes(forty, fill = side)) +
  geom_density(alpha = 0.3)
nfl %>% 
  ggplot(aes(position, forty, fill = side)) +
  geom_boxplot() + geom_hline(yintercept = nfl_mean_forty, color = "red") +
  geom_jitter(alpha = 0.09) 
# - By Conference
nfl %>%
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
nfl_mean_bench <- nfl$bench %>% mean
nfl %>% 
  ggplot(aes(bench)) +
  geom_histogram() + 
  geom_vline(xintercept = nfl_mean_bench, color = "red")
# - By poisition
nfl %>% 
  ggplot(aes(bench, fill = side)) +
  geom_density(alpha = 0.3)
nfl %>% 
  ggplot(aes(position, bench, fill = side)) +
  geom_boxplot() + geom_hline(yintercept = nfl_mean_bench, color = "red") +
  geom_jitter(alpha = 0.09) 
# - By Conference
nfl %>%
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
# Exploratory Data Analysis: PCA ----

# Corrplot
nfl %>% 
  select(height:shuttle) %>% cor %>% 
  corrplot::corrplot(method = "number", type = "upper")

# PCA Object
combine_PCA <- nfl %>% 
  select(height:drafted) %>% 
  PCA(quali.sup = 9, graph = FALSE)
combine_PCA$svd$vs^2
combine_PCA$svd$V
# - Graph
combine_PCA %>% 
  fviz_pca_biplot(repel = TRUE,
                  arrowsize = 1,
                  col.var = "grey25", alpha.var = "cos2",
                  geom.ind = "point", pointsize = 0.5,
                  col.ind = "grey85", 
                  legend.title = list(alpha = "Quality of Representation"))

# Eigen: 70.1% Explained by PC1
combine_PCA %>% fviz_eig(addlabels = TRUE)
# - variables:
combine_PCA$var$coord[,c(1,2)]
combine_PCA$var$cos2[,c(1,2)]
combine_PCA$var$contrib[,c(1,2)]
combine_PCA %>% fviz_contrib(choice = "var", axes = 1)
combine_PCA %>% fviz_contrib(choice = "var", axes = 2)
combine_PCA %>% fviz_contrib(choice = "var", axes = c(1,2))



# - PCA: Drafted
combine_PCA %>% 
  fviz_pca_biplot(repel = TRUE,
                  arrowsize = 1, 
                  col.var = "grey25", alpha.var = "cos2",
                  geom.ind = "point", pointsize = 0.5,
                  col.ind = nfl$drafted, 
                  addEllipses = TRUE, ellispe.type = "norm",
                  palette = c("tomato", "forestgreen"),
                  legend.title = "Drafted")

# - PCA: Round
combine_PCA %>% 
  fviz_pca_biplot(repel = TRUE,
                  arrowsize = 1, 
                  col.var = "grey25", alpha.var = "cos2",
                  geom.ind = "point", pointsize = 0.5,
                  col.ind = nfl$round, 
                  addEllipses = TRUE, ellispe.type = "norm",
                  legend.title = "Round")

# - PCA: Conference
combine_PCA %>% 
  fviz_pca_biplot(repel = TRUE,
                  arrowsize = 1, 
                  col.var = "grey25", alpha.var = "cos2",
                  geom.ind = "point", pointsize = 0.5,
                  col.ind = nfl$conference, 
                  addEllipses = TRUE, ellispe.type = "norm",
                  legend.title = "Conference",
                  legend.position = "bottom")

# - PCA: Side
combine_PCA %>% 
  fviz_pca_biplot(repel = TRUE,
                  arrowsize = 1, 
                  col.var = "grey25", alpha.var = "cos2",
                  geom.ind = "point", pointsize = 0.5,
                  col.ind = nfl$side, 
                  addEllipses = TRUE, ellispe.type = "norm",
                  palette = c("blue","red"),
                  legend.title = "Side")

# - PCA: Position
combine_PCA %>% 
  fviz_pca_biplot(repel = TRUE,
                  arrowsize = 1, 
                  col.var = "grey25", alpha.var = "cos2",
                  geom.ind = "point", pointsize = 0.5,
                  col.ind = nfl$position, 
                  addEllipses = TRUE, ellispe.type = "norm",
                  legend.title = "Position")


# New Dataset
nfl_combine_summary <- nfl %>% 
  select(side, position, conference, weight, forty, broad_jump, bench, drafted)
  
#
# Exploratory Data Analysis: Offense ----
nfl_combine_summary %>% 
  filter(side =="Offense") %>% 
  count(drafted)
# - note: 874 drafted

# STATS: 
# - Summary
nfl_combine_summary %>% 
  filter(side == "Offense") %>% 
  summarise(Avg_Weight = mean(weight),
            Avg_Forty = mean(forty),
            Avg_BroadJump = mean(broad_jump),
            Avg_Bench = mean(bench))

# - By Conference
combine_summary_Offense_Conference_Gather <- nfl_combine_summary %>% 
  filter(side == "Offense") %>% 
  group_by(conference, drafted) %>% 
  summarise(Weight = mean(weight),
            Forty = mean(forty),
            BroadJump = mean(broad_jump),
            Bench = mean(bench)) %>% 
  gather(Weight, Forty, BroadJump, Bench,
         key = "Measure", value = "Stat")

combine_summary_Offense_Conference_Gather %>% 
  ggplot(aes(conference, Stat, fill = drafted)) +
  geom_col(position = "dodge") +
  facet_wrap(~Measure, scales = "free_y") +
  labs(title = "Offense-Conference: Stats") +
  theme_bw() + theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 90, hjust = 1)
  ) +
  scale_fill_manual(values = c("grey70","forestgreen"))
# - By Position
combine_summary_Offense_Position_Gather <- nfl_combine_summary %>% 
  filter(side == "Offense") %>% 
  group_by(position, drafted) %>% 
  summarise(Weight = mean(weight),
            Forty = mean(forty),
            BroadJump = mean(broad_jump),
            Bench = mean(bench)) %>% 
  gather(Weight, Forty, BroadJump, Bench,
         key = "Measure", value = "Stat")

combine_summary_Offense_Position_Gather %>% 
  ggplot(aes(position, Stat, fill = drafted)) +
  geom_col(position = "dodge") +
  facet_wrap(~Measure, scales = "free_y") +
  labs(title = "Offense-Position: Stats") +
  theme_bw() + theme(
    plot.title = element_text(hjust = 0.5, size = 12)
  ) +
  scale_fill_manual(values = c("grey70","forestgreen"))


# Statistical Test
nfl_combine_summary %>% 
  filter(side == "Offense") %>% 
  glm(drafted ~ weight + forty + broad_jump + bench, family = "binomial", data = .) %>% 
  tidy() %>% 
  mutate(estimate = plogis(estimate))

nfl_combine_summary %>% 
  filter(side == "Offense") %>% 
  glm(drafted ~ conference + weight + forty + broad_jump + bench, family = "binomial", data = .) %>% 
  tidy() %>% 
  mutate(estimate = plogis(estimate))

nfl_combine_summary %>% 
  filter(side == "Offense") %>% 
  glm(drafted ~ position + weight + forty + broad_jump + bench, family = "binomial", data = .) %>% 
  tidy() %>% 
  mutate(estimate = plogis(estimate))

nfl_combine_summary %>% 
  filter(side == "Offense") %>% 
  glm(drafted ~ position + conference + weight + forty + broad_jump + bench, family = "binomial", data = .) %>% 
  tidy() %>% 
  mutate(estimate = plogis(estimate),
         p.value = round(p.value,3)) %>% View


#
# Exploratory Data Analysis: Defense ----
nfl %>% 
  filter(side == "Defense") %>% 
  count(drafted)
# - note: 1013 drafted

# STATS: 
# - Summary
nfl_combine_summary %>% 
  filter(side == "Defense") %>% 
  summarise(Avg_Weight = mean(weight),
            Avg_Forty = mean(forty),
            Avg_BroadJump = mean(broad_jump),
            Avg_Bench = mean(bench))
# - By Conference
combine_summary_Defense_Conference_Gather <- nfl_combine_summary %>% 
  filter(side == "Defense") %>% 
  group_by(conference, drafted) %>% 
  summarise(Weight = mean(weight),
            Forty = mean(forty),
            BroadJump = mean(broad_jump),
            Bench = mean(bench)) %>% 
  gather(Weight, Forty, BroadJump, Bench,
         key = "Measure", value = "Stat")

combine_summary_Offense_Conference_Gather %>% 
  ggplot(aes(conference, Stat, fill = drafted)) +
  geom_col(position = "dodge") +
  facet_wrap(~Measure, scales = "free_y") +
  labs(title = "Defense-Conference: Stats") +
  theme_bw() + theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 90, hjust = 1)
  ) +
  scale_fill_manual(values = c("grey70","forestgreen"))
# - By Position
combine_summary_Defense_Position_Gather <- nfl_combine_summary %>% 
  filter(side == "Defense") %>% 
  group_by(position, drafted) %>% 
  summarise(Weight = mean(weight),
            Forty = mean(forty),
            BroadJump = mean(broad_jump),
            Bench = mean(bench)) %>% 
  gather(Weight, Forty, BroadJump, Bench,
         key = "Measure", value = "Stat")

combine_summary_Offense_Position_Gather %>% 
  ggplot(aes(position, Stat, fill = drafted)) +
  geom_col(position = "dodge") +
  facet_wrap(~Measure, scales = "free_y") +
  labs(title = "Defense-Position: Stats") +
  theme_bw() + theme(
    plot.title = element_text(hjust = 0.5, size = 12)
  ) +
  scale_fill_manual(values = c("grey70","forestgreen"))

# Statistical Test
nfl_combine_summary %>% 
  filter(side == "Defense") %>% 
  glm(drafted ~ weight + forty + broad_jump + bench, family = "binomial", data = .) %>% 
  tidy() %>% 
  mutate(estimate = plogis(estimate))

nfl_combine_summary %>% 
  filter(side == "Offense") %>% 
  glm(drafted ~ conference + weight + forty + broad_jump + bench, family = "binomial", data = .) %>% 
  tidy() %>% 
  mutate(estimate = plogis(estimate))

nfl_combine_summary %>% 
  filter(side == "Offense") %>% 
  glm(drafted ~ position + weight + forty + broad_jump + bench, family = "binomial", data = .) %>% 
  tidy() %>% 
  mutate(estimate = plogis(estimate))


# Modeling: Prepocess ----
# Data
nfl_data <- nfl %>% 
  select(position, conference, weight, height, forty, bench, broad_jump, drafted)
nfl_data %>% 
  ggplot(aes(drafted)) +
  geom_bar()
nfl_data$drafted %>% table %>% prop.table() %>% round(2)

# Split
nfl_split <- initial_split(nfl_data, prop = 0.80, strata = drafted)
nfl_train <- training(nfl_split)
nfl_test <- testing(nfl_split)
# - Validation Set
nfl_val <- validation_split(nfl_train, prop = 0.75)
# - K-Folds
nfl_10fold <- vfold_cv(nfl_train, v = 10)

# Preprocess
nfl_recipe <- 
  recipe(drafted ~ position + conference + weight + forty + broad_jump + bench,
       data = nfl_train) %>% 
  step_other(position, threshold = 0.01) %>% 
  step_dummy(position, conference)

# Modeling: Fit ----

# Logistic Regression
# - fit
log_model <- 
  logistic_reg() %>% 
  set_engine("glm")

log_wfow <- 
  workflow() %>% 
  add_model(log_model) %>% 
  add_recipe(nfl_recipe) 

log_final <- 
  fit(log_wfow, data = nfl_train)

# Regularized Regression
# - tune
log_reg_tune_spec <- 
  logistic_reg(
    penalty = tune(), 
    mixture = tune()
) %>% 
  set_engine("glmnet") %>% 
  set_mode("classification")

log_reg_grid <- 
  grid_regular(
    penalty(),
    mixture(),
    levels = 15
  )
# - fit
log_reg_wflow <- 
  workflow() %>% 
  add_model(log_reg_tune_spec) %>% 
  add_recipe(nfl_recipe)

log_reg_resample <- 
  log_reg_wflow %>% 
  tune_grid(
    resamples = nfl_10fold,
    grid = log_reg_grid
  )
# - results
log_reg_resample %>% 
  collect_metrics() %>% 
  filter(.metric == "roc_auc") %>% 
  mutate(mixture = factor( round(mixture,3))) %>% 
  ggplot(aes(penalty, mean, color = mixture)) +
  geom_line(size = 1.5, alpha = 0.6) + 
  theme_bw()
log_reg_best <- 
  log_reg_resample %>%
  select_best("roc_auc")
# - final
log_reg_final <- 
  log_reg_wflow %>% 
  finalize_workflow(log_reg_best)

# Decision Tree
# - Tune
dt_tune_spec <- 
  decision_tree(
    cost_complexity = tune(),
    tree_depth = tune()
) %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

tree_grid <- 
  grid_regular(
    cost_complexity(),
    tree_depth(),
    levels = 5
)
# - fit
dt_wflow <- 
  workflow() %>% 
  add_model(dt_tune_spec) %>% 
  add_formula(drafted ~ position + conference + weight + forty + broad_jump + bench)
  
dt_resample <- 
  dt_wflow %>% 
  tune_grid(
    resamples = nfl_10fold,
    grid = tree_grid
  )
# - results
dt_resample %>% 
  collect_metrics() %>% 
  mutate(tree_depth = factor(tree_depth)) %>% 
  ggplot(aes(cost_complexity, mean, color = tree_depth)) +
  geom_line(size = 1.5, alpha = 0.6) + geom_point(size = 2) +
  facet_wrap(~.metric, scales = "free", nrow = 2) +
  theme_bw()
dt_resample %>% show_best("roc_auc")
dt_best <- dt_resample %>% select_best("roc_auc")
#- final 
dt_final <- 
  dt_wflow %>% 
  finalize_workflow(dt_best)

# Random Forrest
# - grid search
rf_tune_spec <- 
  rand_forest(
    mtry = tune(),
    trees = tune(),
    min_n = tune()
) %>% 
  set_engine("ranger") %>% 
  set_mode("classification")

forrest_grid <- 
  grid_regular(
    mtry() %>% range_set(c(1,20)),
    trees(), 
    min_n(),
    levels = 5
  )
# - fit
rf_wflow <- 
  workflow() %>% 
  add_model(rf_tune_spec) %>% 
  add_formula(drafted ~ position + conference + weight + forty + broad_jump + bench)

rf_resample <- 
  rf_wflow %>% 
  tune_grid(
    resamples = nfl_10fold,
    grid = forrest_grid
  )
# - results
rf_resample %>% 
  collect_metrics() %>% 
  filter(.metric == "roc_auc") %>% 
  mutate(mtry = factor(mtry),
         trees = factor(trees),
         min_n = factor(min_n)) %>% 
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(size = 1.5, alpha = 0.6) + geom_point(size = 2) +
  facet_wrap(~trees, nrow = 1)
rf_best <- rf_resample %>% select_best("roc_auc")  
# - final
rf_final <- 
  rf_wflow %>% 
  finalize_workflow(rf_best)

# Support Vector Machine
# - tune
svm_recipe <- 
  recipe(drafted ~ position + conference + weight + forty + broad_jump + bench, data = nfl_train) %>% 
  step_other(position, threshold = 0.01) %>% 
  step_dummy(position, conference) %>% 
  step_normalize(weight, forty, broad_jump, bench)

svm_spec <- 
  svm_rbf(cost = tune(), rbf_sigma = tune()) %>% 
  set_engine("kernlab") %>% 
  set_mode("classification")

svm_grid <- 
  grid_regular(
    cost(), rbf_sigma(),
    levels = 6
  )
# - fit
svm_wflow <- 
  workflow() %>% 
  add_model(svm_spec) %>% 
  add_recipe(svm_recipe)

svm_fit <- 
  svm_wflow %>% 
  tune_grid(resamples = nfl_val, grid = svm_grid)
# - results
svm_fit %>% 
  collect_metrics() %>%
  mutate(cost = factor(cost),
         rbf_sigma = factor(rbf_sigma)) %>% 
  ggplot(aes(cost, mean, color = rbf_sigma)) +
  geom_point(size = 2)
svm_best <- svm_fit %>% select_best("roc_auc")
# - final
svm_final <- 
  svm_wflow %>% 
  finalize_workflow(svm_best)
#
# Modeling: Diagnostics ----

# Logistic Regression
log_model_fit %>% tidy()
log_fit %>% tidy()

# - Predictions
LOG_test_pred <- 
  data_frame(LOG_Pred = predict(log_fit, new_data = nfl_test) %>% pull(),
             LOG_Prob = predict(log_fit, new_data = nfl_test, type = "prob") %>% pull())
# - Conf Matrix
LOG_test_pred %>% 
  mutate(Actual = nfl_test$drafted) %>% 
  conf_mat(truth = Actual, estimate = LOG_Pred)
# - Metrics
LOG_test_results <- 
  data_frame(Model = "Logistic_Regression",
             Accuracy = accuracy(LOG_test_pred, truth = nfl_test$drafted, estimate = LOG_Pred) %>% pull(.estimate) %>% round(3),
             Detection_Rate = detection_prevalence(LOG_test_pred , truth = nfl_test$drafted, estimate = LOG_Pred, event_level = "second") %>% pull(.estimate) %>% round(3),
             MCC = mcc(LOG_test_pred, truth = nfl_test$drafted, estimate = LOG_Pred, event_level = "second") %>% pull(.estimate) %>% round(3),
             Sensitivity = sens(LOG_test_pred, truth = nfl_test$drafted, estimate = LOG_Pred, event_level = "second") %>% pull(.estimate) %>% round(3),
             Specificity = spec(LOG_test_pred, truth = nfl_test$drafted, estimate = LOG_Pred, event_level = "second") %>% pull(.estimate) %>% round(3),
             Precision = precision(LOG_test_pred, truth = nfl_test$drafted, estimate = LOG_Pred, event_level = "second") %>% pull(.estimate) %>% round(3),
             Recall = recall(LOG_test_pred, truth = nfl_test$drafted, estimate = LOG_Pred, event_level = "second") %>% pull(.estimate) %>% round(3),
             F1 = f_meas(LOG_test_pred, truth = nfl_test$drafted, estimate = LOG_Pred, event_level = "second") %>% pull(.estimate) %>% round(3),
             PPV = ppv(LOG_test_pred, truth = nfl_test$drafted, estimate = LOG_Pred, event_level = "second") %>% pull(.estimate) %>% round(3),
             NPV = npv(LOG_test_pred, truth = nfl_test$drafted, estimate = LOG_Pred, event_level = "second") %>% pull(.estimate) %>% round(3),
             AUC = roc_auc(LOG_test_pred, nfl_test$drafted, LOG_Prob, event_level = "second") %>% pull(.estimate) %>% round(3))
# - ROC Curve
nfl_roc <- nfl_test_pred %>% 
  roc_curve(Actual, Probability, event_level = "second")
nfl_test_pred %>% 
  roc_auc(Actual, Probability, event_level = "second")

nfl_roc %>% autoplot()
# - PR Curve
nfl_pr <- nfl_test_pred %>% 
  pr_curve(Actual, Probability, event_level = "second")

nfl_pr %>% autoplot()
# - Gain & Lift
nfl_gain <- nfl_test_pred %>% 
  gain_curve(Actual, Probability, event_level = "second")
nfl_lift <- nfl_test_pred %>% 
  lift_curve(Actual, Probability, event_level = "second")

nfl_gain %>% autoplot()
nfl_lift %>% autoplot()


# Regularized Regression
log_reg_fit %>% tidy()

# - Predictions
LOG_test_pred <- 
  data_frame(LOG_Pred = predict(log_reg_fit, new_data = nfl_test) %>% pull(),
             LOG_Prob = predict(log_fit, new_data = nfl_test, type = "prob") %>% pull())
# - Conf Matrix
LOG_test_pred %>% 
  mutate(Actual = nfl_test$drafted) %>% 
  conf_mat(truth = Actual, estimate = LOG_Pred)
# - Metrics
LOG_test_results <- 
  data_frame(Model = "Logistic_Regression",
             Accuracy = accuracy(LOG_test_pred, truth = nfl_test$drafted, estimate = LOG_Pred) %>% pull(.estimate) %>% round(3),
             Detection_Rate = detection_prevalence(LOG_test_pred , truth = nfl_test$drafted, estimate = LOG_Pred, event_level = "second") %>% pull(.estimate) %>% round(3),
             MCC = mcc(LOG_test_pred, truth = nfl_test$drafted, estimate = LOG_Pred, event_level = "second") %>% pull(.estimate) %>% round(3),
             Sensitivity = sens(LOG_test_pred, truth = nfl_test$drafted, estimate = LOG_Pred, event_level = "second") %>% pull(.estimate) %>% round(3),
             Specificity = spec(LOG_test_pred, truth = nfl_test$drafted, estimate = LOG_Pred, event_level = "second") %>% pull(.estimate) %>% round(3),
             Precision = precision(LOG_test_pred, truth = nfl_test$drafted, estimate = LOG_Pred, event_level = "second") %>% pull(.estimate) %>% round(3),
             Recall = recall(LOG_test_pred, truth = nfl_test$drafted, estimate = LOG_Pred, event_level = "second") %>% pull(.estimate) %>% round(3),
             F1 = f_meas(LOG_test_pred, truth = nfl_test$drafted, estimate = LOG_Pred, event_level = "second") %>% pull(.estimate) %>% round(3),
             PPV = ppv(LOG_test_pred, truth = nfl_test$drafted, estimate = LOG_Pred, event_level = "second") %>% pull(.estimate) %>% round(3),
             NPV = npv(LOG_test_pred, truth = nfl_test$drafted, estimate = LOG_Pred, event_level = "second") %>% pull(.estimate) %>% round(3),
             AUC = roc_auc(LOG_test_pred, nfl_test$drafted, LOG_Prob, event_level = "second") %>% pull(.estimate) %>% round(3))


# Random Forrest
# - Predictions
RF_test_pred <- 
  data_frame(RF_Pred = predict(rf_fit, new_data = nfl_test) %>% pull(),
             RF_Prob = predict(rf_fit, new_data = nfl_test, type = "prob") %>% pull())
# - Conf Matrix
RF_test_pred %>% 
  mutate(Actual = nfl_test$drafted) %>% 
  conf_mat(truth = Actual, estimate = RF_Pred)
# - Metrics
RF_test_results <- 
  data_frame(Model = "Random_Forrest",
             Accuracy = accuracy(RF_test_pred, truth = nfl_test$drafted, estimate = RF_Pred) %>% pull(.estimate) %>% round(3),
             Detection_Rate = detection_prevalence(RF_test_pred , truth = nfl_test$drafted, estimate = RF_Pred, event_level = "second") %>% pull(.estimate) %>% round(3),
             MCC = mcc(RF_test_pred, truth = nfl_test$drafted, estimate = RF_Pred, event_level = "second") %>% pull(.estimate) %>% round(3),
             Sensitivity = sens(RF_test_pred, truth = nfl_test$drafted, estimate = RF_Pred, event_level = "second") %>% pull(.estimate) %>% round(3),
             Specificity = spec(RF_test_pred, truth = nfl_test$drafted, estimate = RF_Pred, event_level = "second") %>% pull(.estimate) %>% round(3),
             Precision = precision(RF_test_pred, truth = nfl_test$drafted, estimate = RF_Pred, event_level = "second") %>% pull(.estimate) %>% round(3),
             Recall = recall(RF_test_pred, truth = nfl_test$drafted, estimate = RF_Pred, event_level = "second") %>% pull(.estimate) %>% round(3),
             F1 = f_meas(RF_test_pred, truth = nfl_test$drafted, estimate = RF_Pred, event_level = "second") %>% pull(.estimate) %>% round(3),
             PPV = ppv(RF_test_pred, truth = nfl_test$drafted, estimate = RF_Pred, event_level = "second") %>% pull(.estimate) %>% round(3),
             NPV = npv(RF_test_pred, truth = nfl_test$drafted, estimate = RF_Pred, event_level = "second") %>% pull(.estimate) %>% round(3),
             AUC = roc_auc(RF_test_pred, nfl_test$drafted, RF_Prob, event_level = "second") %>% pull(.estimate) %>% round(3))


LOG_test_results %>% 
  bind_rows(RF_test_results)
